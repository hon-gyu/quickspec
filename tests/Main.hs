-- Single test entry point. Aggregates all golden suites:
--
--   end-to-end  -- full QuickSpec pipeline (slow, subprocess per case)
--   component   -- individual QuickSpec.Internal.* modules
--
-- Run subsets via tasty's pattern filter:
--
--   cabal test                                          -- everything
--   cabal test --test-options='-p /component/'          -- just component
--   cabal test --test-options='-p /end-to-end/'         -- just end-to-end
--   cabal test --test-options='-p DecisionTree'         -- one case
--
-- Update goldens with --accept appended to --test-options.

module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import qualified DecisionTree
import qualified EndToEnd

main :: IO ()
main =
  defaultMain $
    testGroup "tests"
      [ EndToEnd.tests
      , testGroup "component"
          [ DecisionTree.tests
          ]
      ]
