{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Reading PromQL back into an expression.
--
-- What 'PromQL.Render.render' writes reads back as an expression that
-- 'PromQL.Render.render' writes the same way.
-- It need not be the same expression, because some are written alike:
-- every 'Regexp' is text by the time it is in a query, so one is read back as
-- a 'Pattern', and @round(x)@ is one call whichever way it was built.
--
-- The grammar is Prometheus's, including where a minus binds: it binds less
-- tightly than exponentiation, so @-1 ^ 2@ is @-(1 ^ 2)@.
-- So is the case of a word: keywords and aggregations are read in any case,
-- and functions only as they are written.
--
-- Some of what Prometheus reads has no expression here and is refused, such
-- as a minus in front of something other than a number, @group_left@, @\@@,
-- a duration in more than one unit, a quoted label or metric name, and a
-- function that 'Call' has no constructor for.
-- Where the reason is one of the 'Unsupported' things, the message says so.
module PromQL.Parse
  ( parseExpr,
    parseDuration,
    parseSelector,
    ParseError,
    Unsupported (..),
    renderParseError,
  )
where

import Control.Monad (guard, void, (>=>))
import qualified Data.ByteString as ByteString
import Data.Char (chr, digitToInt, isAlphaNum, isAsciiLower, isAsciiUpper, isDigit, isHexDigit, isOctDigit)
import Data.List.NonEmpty (nonEmpty)
import Data.Maybe (isNothing)
import Data.Scientific (scientific, toRealFloat)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Word (Word8)
import PromQL.Expr
import Text.Megaparsec hiding (ParseError)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lexer

type Parser = Parsec Unsupported Text

-- | Where the text stopped being PromQL, or stopped being PromQL an
-- expression here can hold, which is one of the 'Unsupported' things.
type ParseError = ParseErrorBundle Text Unsupported

-- | What Prometheus reads and an expression here cannot hold, or what the
-- grammar refuses in a way worth saying more about than where.
data Unsupported
  = -- | A minus in front of something other than a number.
    NegatedNonNumber
  | -- | @group_left@ or @group_right@.
    GroupModifier
  | -- | The samples in a window, somewhere other than as a function's
    -- argument.
    RangeOutsideFunction
  | -- | An offset written before a selector's window rather than after it.
    OffsetBeforeWindow
  | -- | A window after something that is not a selector, without the colon
    -- that would make it a subquery.
    RangeOfNonSelector
  | RangeOfRange
  | NegativeOffset
  | -- | An offset after something that is not a selector.
    OffsetOfNonSelector
  | UnknownFunction !Text
  | WrongArguments !Text
  | -- | A duration in more than one unit, such as @1h30m@.
    CompoundDuration
  | -- | An escape for something that is not a character.
    EscapeNotACharacter
  | -- | Escaped bytes that are not the UTF-8 of any text.
    EscapedBytesNotUTF8
  deriving (Show, Eq, Ord)

instance ShowErrorComponent Unsupported where
  showErrorComponent = \case
    NegatedNonNumber -> "a minus in front of anything but a number is not something an expression here can hold"
    GroupModifier -> "group_left and group_right are not something an expression here can hold"
    RangeOutsideFunction -> "the samples in a window are only something a function takes"
    OffsetBeforeWindow -> "an offset goes after a selector's window, not before it"
    RangeOfNonSelector -> "only a selector can be read over a window; anything else is a subquery, with a colon"
    RangeOfRange -> "a window of samples cannot be read over another window"
    NegativeOffset -> "a negative offset is not something an expression here can hold"
    OffsetOfNonSelector -> "only a selector can be offset here"
    UnknownFunction name -> Text.unpack (Text.concat [name, " is not a function an expression here can hold"])
    WrongArguments name -> Text.unpack (Text.concat [name, " is not given those arguments"])
    CompoundDuration -> "a duration in more than one unit is not something an expression here can hold"
    EscapeNotACharacter -> "the escape is not for a character"
    EscapedBytesNotUTF8 -> "the escaped bytes are not UTF-8"

-- | The error as a person reads it: where, and what was expected there.
renderParseError :: ParseError -> String
renderParseError = errorBundlePretty

parseExpr :: Text -> Either ParseError Expr
parseExpr = parseWhole exprParser

parseDuration :: Text -> Either ParseError Duration
parseDuration = parseWhole durationParser

-- | A selector, as a series is written in a promtool test file.
parseSelector :: Text -> Either ParseError Selector
parseSelector = parseWhole selectorParser

parseWhole :: Parser a -> Text -> Either ParseError a
parseWhole parser = parse (spaceConsumer *> parser <* eof) ""

-- | Spaces, line breaks and comments, which Prometheus reads nothing into.
--
-- Which of these is skipped first makes no difference.
{-# ANN spaceConsumer ("DisableMutation: SwitchFunctionArguments" :: String) #-}
spaceConsumer :: Parser ()
spaceConsumer = Lexer.space space1 (Lexer.skipLineComment "#") empty

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme spaceConsumer

symbol :: Text -> Parser ()
symbol = void . Lexer.symbol spaceConsumer

-- | A word, and not the start of a longer one.
keyword :: Text -> Parser ()
keyword word = lexeme . try $ string' word *> notFollowedBy (satisfy continuesWord)

continuesWord :: Char -> Bool
continuesWord character = isAlphaNum character || character == '_' || character == ':'

exprParser :: Parser Expr
exprParser = orLevel

-- | The levels of Prometheus's precedence table, loosest first.
orLevel :: Parser Expr
orLevel = leftAssociative andLevel [Or <$ keyword "or"]

andLevel :: Parser Expr
andLevel = leftAssociative comparisonLevel [And <$ keyword "and", Unless <$ keyword "unless"]

comparisonLevel :: Parser Expr
comparisonLevel =
  leftAssociative
    additionLevel
    [ do
        comparison <-
          choice
            [ Equal <$ symbol "==",
              NotEqual <$ symbol "!=",
              LessOrEqual <$ symbol "<=",
              GreaterOrEqual <$ symbol ">=",
              LessThan <$ symbol "<",
              GreaterThan <$ symbol ">"
            ]
        answering <- option AsAFilter (AsZeroOrOne <$ keyword "bool")
        pure (Compare comparison answering)
    ]

additionLevel :: Parser Expr
additionLevel = leftAssociative multiplicationLevel [Add <$ symbol "+", Subtract <$ symbol "-"]

multiplicationLevel :: Parser Expr
multiplicationLevel =
  leftAssociative
    signed
    [ Multiply <$ symbol "*",
      Divide <$ symbol "/",
      Modulo <$ symbol "%",
      Atan2 <$ keyword "atan2"
    ]

-- | A minus or a plus binds as tightly as a multiplication, which is less
-- tightly than exponentiation.
--
-- The only one of those an expression here can hold is a minus in front of a
-- number, which is that number negated.
signed :: Parser Expr
signed =
  choice
    [ do
        offsetAtStart <- getOffset
        symbol "-"
        operand <- signed
        case operand of
          Number x -> pure (Number (negate x))
          _ -> do
            setOffset offsetAtStart
            customFailure NegatedNonNumber,
      symbol "+" *> signed,
      powerLevel
    ]

-- | Exponentiation, the one operator that associates to the right, and whose
-- right-hand side may start with a sign.
powerLevel :: Parser Expr
powerLevel = do
  base <- operandExpr
  option base $ do
    symbol "^"
    matchOn <- matchingParser
    Binary Power matchOn base <$> signed

-- | Each operator of one level, then the next level, as many times as they
-- come, grouped to the left.
leftAssociative :: Parser Expr -> [Parser BinOp] -> Parser Expr
leftAssociative operand operators = do
  first <- operand
  rest <- many $ do
    operator <- choice operators
    matchOn <- matchingParser
    right <- operand
    pure (operator, matchOn, right)
  pure (foldl' (\left (operator, matchOn, right) -> Binary operator matchOn left right) first rest)

matchingParser :: Parser Matching
matchingParser = do
  matchOn <-
    option OnEverything $
      choice
        [ On <$> (keyword "on" *> labelList),
          Ignoring <$> (keyword "ignoring" *> labelList)
        ]
  notFollowedBy (keyword "group_left" <|> keyword "group_right")
    <|> customFailure GroupModifier
  pure matchOn

labelList :: Parser [LabelName]
labelList = between (symbol "(") (symbol ")") (sepEndBy labelNameParser (symbol ","))

labelNameParser :: Parser LabelName
labelNameParser = LabelName <$> lexeme identifier <?> "a label name"

identifier :: Parser Text
identifier = do
  first <- satisfy (\character -> isAsciiUpper character || isAsciiLower character || character == '_')
  rest <- takeWhileP Nothing (\character -> isAsciiUpper character || isAsciiLower character || character == '_' || isDigit character)
  pure (Text.cons first rest)

metricIdentifier :: Parser Text
metricIdentifier = do
  first <- satisfy (\character -> isAsciiUpper character || isAsciiLower character || character == '_' || character == ':')
  rest <- takeWhileP Nothing continuesWord
  pure (Text.cons first rest)

-- | What a function's argument or an operator's operand can be: an
-- expression, or the samples in a window, which only some functions take.
data Operand
  = OperandExpr !Expr
  | OperandRange !RangeVector

operandExpr :: Parser Expr
operandExpr = do
  offsetAtStart <- getOffset
  operandParser >>= \case
    OperandExpr expr -> pure expr
    OperandRange _ -> do
      setOffset offsetAtStart
      customFailure RangeOutsideFunction

-- | Something that binds as tightly as anything does, and whatever brackets
-- and offsets come after it.
operandParser :: Parser Operand
operandParser = do
  atom <- atomParser
  withRanges atom

withRanges :: Operand -> Parser Operand
withRanges operand =
  option operand $ do
    offsetAtStart <- getOffset
    symbol "["
    window <- durationParser
    step <- optional (symbol ":" *> optional durationParser)
    symbol "]"
    ranged <- case (operand, step) of
      (OperandExpr (Vector selector), Nothing) -> do
        offset <- optional offsetParser
        case selectorOffset selector of
          Just _ -> do
            setOffset offsetAtStart
            customFailure OffsetBeforeWindow
          Nothing -> pure (OperandRange (Over selector {selectorOffset = offset} window))
      (OperandExpr _, Nothing) -> do
        setOffset offsetAtStart
        customFailure RangeOfNonSelector
      (OperandExpr expr, Just sampleStep) -> pure (OperandRange (Sampled expr window sampleStep))
      (OperandRange _, _) -> do
        setOffset offsetAtStart
        customFailure RangeOfRange
    withRanges ranged

offsetParser :: Parser Duration
offsetParser = do
  keyword "offset"
  notFollowedBy (symbol "-") <|> customFailure NegativeOffset
  durationParser

atomParser :: Parser Operand
atomParser =
  let bracesOnly :: Parser Selector
      bracesOnly = do
        matchers <- matchersParser
        pure
          Selector
            { selectorMetric = Nothing,
              selectorMatchers = matchers,
              selectorOffset = Nothing
            }
   in choice
        [ OperandExpr . String <$> stringParser,
          OperandExpr . Window <$> try (lexeme durationLiteral),
          OperandExpr . Number <$> numberParser,
          OperandExpr . Window . DurationVariable <$> lexeme variable,
          between (symbol "(") (symbol ")") (OperandExpr <$> exprParser),
          OperandExpr . Vector <$> bracesOnly,
          namedParser
        ]
        >>= \case
          OperandExpr expr -> do
            offsetAtStart <- getOffset
            offset <- optional offsetParser
            case (expr, offset) of
              (_, Nothing) -> pure (OperandExpr expr)
              (Vector selector, Just duration) | isNothing (selectorOffset selector) -> pure (OperandExpr (Vector selector {selectorOffset = Just duration}))
              _ -> do
                setOffset offsetAtStart
                customFailure OffsetOfNonSelector
          ranged -> pure ranged

-- | A function, an aggregation, or a metric, which all start with a name.
namedParser :: Parser Operand
namedParser = do
  offsetAtStart <- getOffset
  name <- lexeme metricIdentifier
  case lookup (Text.toLower name) aggregations of
    Just aggregationParser -> OperandExpr <$> aggregationCall aggregationParser
    Nothing -> do
      call' <- optional (lookAhead (symbol "("))
      case call' of
        Just () -> OperandExpr <$> functionCall offsetAtStart name
        Nothing -> do
          matchers <- option [] matchersParser
          pure
            ( OperandExpr
                ( Vector
                    Selector
                      { selectorMetric = Just (MetricName name),
                        selectorMatchers = matchers,
                        selectorOffset = Nothing
                      }
                )
            )

selectorParser :: Parser Selector
selectorParser = do
  name <- optional (lexeme metricIdentifier)
  matchers <- case name of
    Nothing -> matchersParser
    Just _ -> option [] matchersParser
  offset <- optional offsetParser
  pure
    Selector
      { selectorMetric = MetricName <$> name,
        selectorMatchers = matchers,
        selectorOffset = offset
      }

matchersParser :: Parser [Matcher]
matchersParser = between (symbol "{") (symbol "}") (sepEndBy matcherParser (symbol ","))

matcherParser :: Parser Matcher
matcherParser = do
  name <- labelNameParser
  operator <-
    choice
      [ (Matches . Pattern) <$ symbol "=~",
        (DoesNotMatch . Pattern) <$ symbol "!~",
        IsNot <$ symbol "!=",
        Is <$ symbol "="
      ]
  value <- stringParser
  pure
    Matcher
      { matcherLabel = name,
        matcherMatch = operator value
      }

-- | What an aggregation takes besides the vector, and how it is built.
aggregations :: [(Text, Parser (Maybe Grouping -> Expr -> Expr))]
aggregations =
  let plain :: Aggregation -> Parser (Maybe Grouping -> Expr -> Expr)
      plain aggregation = pure (Aggregate aggregation)
      withParameter :: Parser Aggregation -> Parser (Maybe Grouping -> Expr -> Expr)
      withParameter parameter = do
        aggregation <- parameter
        symbol ","
        pure (Aggregate aggregation)
   in [ ("sum", plain Sum),
        ("avg", plain Average),
        ("min", plain Minimum),
        ("max", plain Maximum),
        ("count", plain Count),
        ("group", plain Group),
        ("stddev", plain StandardDeviation),
        ("stdvar", plain StandardVariance),
        ("count_values", withParameter (CountValues . LabelName <$> stringParser)),
        ("topk", withParameter (TopK <$> exprParser)),
        ("bottomk", withParameter (BottomK <$> exprParser)),
        ("quantile", withParameter (Quantile <$> exprParser))
      ]

-- | An aggregation, whose grouping may come before its arguments or after
-- them.
aggregationCall :: Parser (Maybe Grouping -> Expr -> Expr) -> Parser Expr
aggregationCall aggregationParser = do
  before <- optional groupingParser
  symbol "("
  build <- aggregationParser
  inner <- exprParser
  optional (symbol ",") *> symbol ")"
  grouping <- case before of
    Just grouping -> pure grouping
    Nothing -> option Nothing groupingParser
  pure (build grouping inner)

-- | @by ()@ keeps no labels, which is what no grouping does.
groupingParser :: Parser (Maybe Grouping)
groupingParser =
  choice
    [ fmap By . nonEmpty <$> (keyword "by" *> labelList),
      fmap Without . nonEmpty <$> (keyword "without" *> labelList)
    ]

functionCall :: Int -> Text -> Parser Expr
functionCall offsetAtStart name = do
  arguments <- between (symbol "(") (symbol ")") (sepEndBy argumentParser (symbol ","))
  let refuse unsupported = setOffset offsetAtStart *> customFailure unsupported
  case lookup name functions of
    Nothing -> refuse (UnknownFunction name)
    Just build -> maybe (refuse (WrongArguments name)) pure (build arguments)

-- | Each function, and the expression a call to it with these arguments is.
functions :: [(Text, [Operand] -> Maybe Expr)]
functions =
  let asExpr :: Operand -> Maybe Expr
      asExpr = \case
        OperandExpr expr -> Just expr
        OperandRange _ -> Nothing
      asString :: Expr -> Maybe Text
      asString = \case
        String text -> Just text
        _ -> Nothing
      labelReplace :: [Operand] -> Maybe Expr
      labelReplace arguments = do
        (inner, texts) <- case arguments of
          OperandExpr inner : rest -> Just (inner, rest)
          _ -> Nothing
        strings <- traverse (asExpr >=> asString) texts
        case strings of
          [destination, text, source, regexp] ->
            Just
              ( LabelReplace
                  inner
                  Replacement
                    { replacementDestination = LabelName destination,
                      replacementText = text,
                      replacementSource = LabelName source,
                      replacementRegexp = Pattern regexp
                    }
              )
          _ -> Nothing
   in concat
        [ [ (name, \case [OperandRange rangeVector] -> Just (OverTime function rangeVector); _ -> Nothing)
          | (name, function) <- rangeFunctions
          ],
          [ (name, \arguments -> Apply <$> (traverse asExpr arguments >>= build))
          | (name, build) <- calls
          ],
          [ ( "quantile_over_time",
              \case
                [OperandExpr quantile, OperandRange rangeVector] -> Just (QuantileOverTime quantile rangeVector)
                _ -> Nothing
            ),
            ( "absent_over_time",
              \case
                [OperandRange rangeVector] -> Just (Apply (AbsentOverTime rangeVector))
                _ -> Nothing
            ),
            ("label_replace", labelReplace)
          ]
        ]

-- | An argument, which is the samples in a window only where that is the whole
-- of it, as in @rate(x[5m])@, and an expression otherwise.
argumentParser :: Parser Operand
argumentParser =
  choice
    [ try $ do
        operand <- operandParser
        case operand of
          OperandRange _ -> operand <$ lookAhead (symbol "," <|> symbol ")")
          OperandExpr _ -> empty,
      OperandExpr <$> exprParser
    ]

rangeFunctions :: [(Text, RangeFunction)]
rangeFunctions =
  [ ("rate", Rate),
    ("irate", InstantRate),
    ("increase", Increase),
    ("delta", Delta),
    ("idelta", InstantDelta),
    ("deriv", Derivative),
    ("changes", Changes),
    ("resets", Resets),
    ("avg_over_time", AverageOverTime),
    ("min_over_time", MinimumOverTime),
    ("max_over_time", MaximumOverTime),
    ("sum_over_time", SumOverTime),
    ("count_over_time", CountOverTime),
    ("last_over_time", LastOverTime),
    ("present_over_time", PresentOverTime),
    ("stddev_over_time", StandardDeviationOverTime),
    ("stdvar_over_time", StandardVarianceOverTime)
  ]

calls :: [(Text, [Expr] -> Maybe Call)]
calls =
  let one :: (Expr -> Call) -> [Expr] -> Maybe Call
      one build = \case
        [inner] -> Just (build inner)
        _ -> Nothing
      two :: (Expr -> Expr -> Call) -> [Expr] -> Maybe Call
      two build = \case
        [first, second] -> Just (build first second)
        _ -> Nothing
   in [ ("abs", one Absolute),
        ("ceil", one Ceiling),
        ("floor", one Floor),
        ("exp", one Exponential),
        ("ln", one NaturalLog),
        ("log2", one Log2),
        ("log10", one Log10),
        ("sqrt", one SquareRoot),
        ("sgn", one Sign),
        ( "round",
          \case
            [inner] -> Just (Round inner Nothing)
            [inner, places] -> Just (Round inner (Just places))
            _ -> Nothing
        ),
        ("scalar", one Scalar),
        ("vector", one ConstantVector),
        ("clamp_min", two ClampBelow),
        ("clamp_max", two ClampAbove),
        ( "clamp",
          \case
            [inner, lower, upper] -> Just (Clamp inner lower upper)
            _ -> Nothing
        ),
        ("absent", one Absent),
        ("timestamp", one Timestamp),
        ("histogram_quantile", two HistogramQuantile),
        ("sort", one Sort),
        ("sort_desc", one SortDescending)
      ]

durationParser :: Parser Duration
durationParser = lexeme (durationLiteral <|> DurationVariable <$> variable) <?> "a duration"

-- | A whole number and one unit.
durationLiteral :: Parser Duration
durationLiteral = do
  n <- Lexer.decimal
  unit <-
    choice
      [ Milliseconds <$ string "ms",
        Seconds <$ string "s",
        Minutes <$ string "m",
        Hours <$ string "h",
        Days <$ string "d",
        Weeks <$ string "w",
        Years <$ string "y"
      ]
  notFollowedBy (satisfy (\character -> isAlphaNum character || character == '_'))
    <|> customFailure CompoundDuration
  pure (unit n)

variable :: Parser Text
variable = do
  dollar <- char '$'
  name <- takeWhile1P (Just "a variable's name") (\character -> isAlphaNum character || character == '_')
  pure (Text.cons dollar name)

-- | A number as Prometheus reads one, without its sign: a minus in front of a
-- number is an operator.
numberParser :: Parser Double
numberParser =
  let hexadecimal :: Parser Double
      hexadecimal = do
        void (char '0' *> char' 'x')
        digits <- takeWhile1P Nothing isHexDigit
        pure (fromInteger (digitsValue 16 digits))
      -- Either side of the point may be left out, as in @.5@ and @5.@, but
      -- not both.
      decimal :: Parser Double
      decimal = do
        whole <- takeWhileP Nothing isDigit
        fraction <- option "" (char '.' *> takeWhileP Nothing isDigit)
        guard (not (Text.null whole && Text.null fraction))
        exponent' <- option 0 (char' 'e' *> Lexer.signed (pure ()) Lexer.decimal)
        pure (toRealFloat (scientific (digitsValue 10 (Text.concat [whole, fraction])) (exponent' - Text.length fraction)))
      named :: Text -> Double -> Parser Double
      named word value = value <$ try (string' word <* notFollowedBy (satisfy continuesWord))
   in lexeme
        ( choice
            [ try hexadecimal,
              decimal,
              named "nan" (0 / 0),
              named "inf" (1 / 0)
            ]
        )
        <?> "a number"

digitsValue :: Integer -> Text -> Integer
digitsValue base = Text.foldl' (\n digit -> n * base + toInteger (digitToInt digit)) 0

-- | A string in any of the three quotes Prometheus takes, with Go's escapes
-- in the two that read them.
stringParser :: Parser Text
stringParser =
  let quotedWith :: Char -> Parser Text
      quotedWith quote =
        Text.concat <$> (char quote *> manyTill (escapedBytes <|> Text.singleton <$> escapedCharacter quote) (char quote))
      escapedCharacter :: Char -> Parser Char
      escapedCharacter quote =
        choice
          [ char '\\' *> escape quote,
            satisfy (/= '\n')
          ]
      escape :: Char -> Parser Char
      escape quote =
        choice
          [ '\\' <$ char '\\',
            quote <$ char quote,
            '\n' <$ char 'n',
            '\r' <$ char 'r',
            '\t' <$ char 't',
            '\a' <$ char 'a',
            '\b' <$ char 'b',
            '\f' <$ char 'f',
            '\v' <$ char 'v',
            char 'u' *> codePoint 4,
            char 'U' *> codePoint 8
          ]
   in lexeme
        ( choice
            [ quotedWith '"',
              quotedWith '\'',
              Text.pack <$> (char '`' *> manyTill anySingle (char '`'))
            ]
        )
        <?> "a string"

-- | Go refuses an escape for a surrogate, which is not a character.
codePoint :: Int -> Parser Char
codePoint width = do
  digits <- count width (satisfy isHexDigit)
  let n = digitsValue 16 (Text.pack digits)
  if n <= 0x10FFFF && not (n >= 0xD800 && n <= 0xDFFF)
    then pure (chr (fromInteger n))
    else customFailure EscapeNotACharacter

-- | Go reads @\\x@ and octal escapes as bytes rather than as characters, so
-- a character outside ASCII is escaped as the bytes of its UTF-8, and the
-- escapes in a row are read as one piece of UTF-8.
escapedBytes :: Parser Text
escapedBytes = do
  offsetAtStart <- getOffset
  let byte :: Int -> Integer -> (Char -> Bool) -> Parser Word8
      byte width base isDigitOf = do
        digits <- count width (satisfy isDigitOf)
        let n = digitsValue base (Text.pack digits)
        if n <= 0xFF
          then pure (fromInteger n)
          else customFailure EscapeNotACharacter
  let startsByteEscape :: Char -> Bool
      startsByteEscape character = character == 'x' || isOctDigit character
  bytes <-
    some $
      try (char '\\' <* lookAhead (satisfy startsByteEscape))
        *> choice [char 'x' *> byte 2 16 isHexDigit, byte 3 8 isOctDigit]
  case Text.decodeUtf8' (ByteString.pack bytes) of
    Right text -> pure text
    Left _ -> do
      setOffset offsetAtStart
      customFailure EscapedBytesNotUTF8
