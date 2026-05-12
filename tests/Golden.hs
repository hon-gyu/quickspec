-- Golden snapshot tests for the example programs.
--
-- Each example is built as a separate cabal executable
-- (`example-<lowername>`); this test suite execs each binary with a fixed
-- QuickCheck seed and diffs stdout against `tests/golden/<Name>.output`.
--
-- Update snapshots: cabal test golden --test-options=--accept

module Main (main) where

import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Char (toLower)
import System.Environment (getEnvironment)
import System.FilePath ((<.>), (</>))
import System.Process (CreateProcess (..), proc, readCreateProcess)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden (goldenVsString)

quickcheckSeed :: String
quickcheckSeed = "1234"

-- Initial scaffold. To add an example:
--   1. Drop tests/<Name>.hs
--   2. Add an `executable example-<lowername>` stanza in quickspec.cabal
--   3. Add it to `build-tool-depends` of the `golden` test-suite
--   4. Append the name here, then run with --accept to capture the golden
examples :: [String]
examples = ["Arith", "Lists"]

main :: IO ()
main = defaultMain $ testGroup "examples (golden)" (map mkCase examples)

mkCase :: String -> TestTree
mkCase name =
  goldenVsString
    name -- test name
    ("tests" </> "golden" </> name <.> "output") -- File path
    runExample -- IO ByteString -> TestTree
  where
    bin = "example-" ++ map toLower name
    runExample = do
      base <- getEnvironment
      let cp =
            (proc bin [])
              { env = Just (("QUICKCHECK_SEED", quickcheckSeed) : base)
              }
      BL.pack <$> readCreateProcess cp ""
