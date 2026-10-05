{-# LANGUAGE OverloadedStrings #-}

module PromQL.ExprSpec (spec) where

import PromQL
import Test.Syd

spec :: Spec
spec = do
  describe "the filtering comparisons" $
    it "names the half of the table that drops what does not compare" $
      [isEqualTo, isNotEqualTo, isLessThan, isAtMost, isGreaterThan, isAtLeast]
        `shouldBe` map
          (`Compare` AsAFilter)
          [Equal, NotEqual, LessThan, LessOrEqual, GreaterThan, GreaterOrEqual]

  describe "the calls" $
    it "names each function as the call it applies" $
      let a = Vector (metric (MetricName "a"))
          one = Number 1
          two = Number 2
          window = Over (metric (MetricName "a")) (Minutes 5)
       in [ absolute a,
            roundedUp a,
            roundedDown a,
            roundedTo a Nothing,
            roundedTo a (Just two),
            exponential a,
            naturalLog a,
            log2 a,
            log10 a,
            squareRoot a,
            sign a,
            scalar a,
            constantVector two,
            clampBelow a two,
            clampAbove a two,
            clamp a one two,
            absent a,
            absentOverTime window,
            timestamp a,
            histogramQuantile two a,
            sorted a,
            sortedDescending a
          ]
            `shouldBe` map
              Apply
              [ Absolute a,
                Ceiling a,
                Floor a,
                Round a Nothing,
                Round a (Just two),
                Exponential a,
                NaturalLog a,
                Log2 a,
                Log10 a,
                SquareRoot a,
                Sign a,
                Scalar a,
                ConstantVector two,
                ClampBelow a two,
                ClampAbove a two,
                Clamp a one two,
                Absent a,
                AbsentOverTime window,
                Timestamp a,
                HistogramQuantile two a,
                Sort a,
                SortDescending a
              ]
