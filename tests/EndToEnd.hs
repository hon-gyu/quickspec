-- Golden snapshot tests for full QuickSpec end-to-end runs.
--
-- Each example is built as a separate cabal executable
-- (`example-<lowername>`); we exec it with a fixed QuickCheck seed and
-- diff stdout against `tests/golden/<Name>.output`. We go through a
-- subprocess (rather than linking the example in directly) because
-- `quickSpec` drives QuickCheck with global state and prints to
-- stdout — easier to isolate as a child process than to compose
-- multiple runs inside one test binary.
--
-- Update snapshots: cabal test --test-options='-p /end-to-end/ --accept'

module EndToEnd (tests) where

import Data.Char (toLower)
import qualified Data.Text as T
import System.Environment (getEnvironment)
import System.FilePath ((<.>), (</>))
import System.Process (CreateProcess (..), proc, readCreateProcess)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Silver (goldenVsAction)

quickcheckSeed :: String
quickcheckSeed = "1234"

-- To add an example:
--   1. Drop tests/<Name>.hs
--   2. Add an `executable example-<lowername>` stanza in quickspec.cabal
--   3. Add it to `build-tool-depends` of the `tests` test-suite
--   4. Append the name here, then run with --accept to capture the golden
examples :: [String]
examples = ["Arith", "Lists"]

tests :: TestTree
tests = testGroup "end-to-end" (map mkCase examples)

mkCase :: String -> TestTree
mkCase name =
  goldenVsAction
    name
    ("tests" </> "golden" </> "EndToEnd" </> name <.> "output")
    runExample
    T.pack
  where
    bin = "example-" ++ map toLower name
    runExample = do
      base <- getEnvironment
      let cp =
            (proc bin [])
              { env = Just (("QUICKCHECK_SEED", quickcheckSeed) : base)
              }
      readCreateProcess cp ""
