-- Golden test for QuickSpec.Internal.Testing.DecisionTree.
--
-- We model "terms" as small arithmetic expressions over a single
-- variable, and "test cases" as integers. A term's evaluation on a test
-- case is just the integer result (occasionally Nothing to exercise the
-- "untestable" branch).
--
-- `output` is pure, so we hand it straight to tasty-golden — no
-- subprocess needed (cf. EndToEnd, which has to exec a real binary
-- because it drives the full QuickSpec/QuickCheck pipeline).

module DecisionTree (tests) where

import qualified Data.ByteString.Lazy.Char8 as BL
import QuickSpec.Internal.Testing.DecisionTree
import System.FilePath ((<.>), (</>))
import Test.Tasty (TestTree)
import Test.Tasty.Golden (goldenVsString)

tests :: TestTree
tests =
  goldenVsString
    "DecisionTree"
    ("tests" </> "golden" </> "DecisionTree" <.> "output")
    (return (BL.pack output))

data Term
  = Const Int
  | Var
  | Add Term Term
  | Mul Term Term
  | Partial Term -- like its argument, but undefined on test case 0
  deriving (Eq, Show)

evalTerm :: Term -> Int -> Maybe Int
evalTerm (Const n) _ = Just n
evalTerm Var x = Just x
evalTerm (Add a b) x = (+) <$> evalTerm a x <*> evalTerm b x
evalTerm (Mul a b) x = (*) <$> evalTerm a x <*> evalTerm b x
evalTerm (Partial _) 0 = Nothing
evalTerm (Partial t) x = evalTerm t x

testCases :: [Int]
testCases = [1, 2, 3, 0, 7]

-- Terms to insert, in order. Designed to hit each interesting branch:
--   * first insert into empty tree (Distinct, Singleton path)
--   * duplicate of an existing term (EqualTo)
--   * term that splits the tree on a TestCase node (Distinct)
--   * term distinguished only by a later test case (Distinct)
--   * Partial term to exercise the "untestable" eviction branch
terms :: [(String, Term)]
terms =
  [ ("x", Var),
    ("x+0", Add Var (Const 0)), -- equal to Var on all tcs
    ("2*x", Mul (Const 2) Var), -- distinct
    ("x+x", Add Var Var), -- equal to 2*x
    ("x*x", Mul Var Var), -- distinct
    ("x+1", Add Var (Const 1)), -- distinct
    ("partial x", Partial Var) -- exercises Nothing branch
  ]

type DT = DecisionTree Int Int Term

initial :: DT
initial = foldr addTestCase (empty evalTerm) (reverse testCases)

step :: DT -> (String, Term) -> (DT, String)
step dt (label, t) =
  case insert t dt of
    Distinct dt' ->
      (dt', "insert " ++ pad label ++ " -> Distinct")
    EqualTo prev ->
      (dt, "insert " ++ pad label ++ " -> EqualTo (" ++ show prev ++ ")")
  where
    pad s = s ++ replicate (12 - length s) ' '

output :: String
output =
  unlines $
    ["test cases:"]
      ++ map (\t -> "  " ++ show t) testCases
      ++ [ "",
           "insertions:"
         ]
      ++ logs
      ++ [ "",
           "statistics:",
           show (statistics final)
         ]
  where
    (final, logs) = run initial terms []
    run dt [] acc = (dt, reverse acc)
    run dt (x : xs) acc =
      let (dt', line) = step dt x
       in run dt' xs (line : acc)
