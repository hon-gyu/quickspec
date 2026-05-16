{-
TODO:
    - Try all built-in pretty
    - Try Dict
    - Value

Types are represented as Twee's algebra
- `type Type = Term TyCon`
- let QS reuse Twee's unification/matching machinery

Dict:
- comes from the constraints library
- reifies a constraint as a first-class value:
- let QuickSpec treats typeclass-polymorphic functions uniformly

Value:
- type-erased wrapper

Haskell:
- Data.Proxy: a phantom type carrier
- Data.Typeable
- Data.Constraint
    - Dict c is a value that witnesses that constraint c holds.
      Normally constraints are implicit and erased; Dict boxes them
      up so you can store, pass, and return them.

Q:
- what is Labelled in Twee?
-}

module Type where

import Data.Constraint (Dict)
import Data.Functor.Identity
import Data.Proxy
import qualified Data.Text as Text
import QuickSpec.Internal.Type
import System.FilePath ((<.>), (</>))
import Test.Tasty (TestTree)
import Test.Tasty.Silver (goldenVsAction)
import Twee.Base (prettyShow)
import Utils

tests :: TestTree
tests = goldenVsAction "Type" path (return output) Text.pack
  where
    path = "tests" </> "Type" <.> "expected"

output :: String
output =
  unlines
    [ h1 "typeOf ",
      show (typeOf (42 :: Int)),
      show (typeOf 'x'),
      show (typeOf True),
      "",
      h1 "typeRep (via Proxy)",
      show (typeRep (Proxy :: Proxy Int)),
      show (typeRep (Proxy :: Proxy [Int])),
      show (typeRep (Proxy :: Proxy (Int, Bool))) ++ " / " ++ prettyShow (typeRep (Proxy :: Proxy (Int, Bool))),
      show (typeRep (Proxy :: Proxy (Int -> Bool))) ++ " / " ++ prettyShow (typeRep (Proxy :: Proxy (Int -> Bool))),
      "",
      h1 "type variables",
      "typeVar:            " ++ show typeVar,
      "isTypeVar typeVar:  " ++ show (isTypeVar typeVar),
      "isTypeVar Int:      " ++ show (isTypeVar (typeRep (Proxy :: Proxy Int))),
      "",
      h1 "Dict",
      "isDictionary (Dict (Eq Int)):  " ++ show (isDictionary dictEqInt),
      "getDictionary (Dict (Eq Int)): " ++ show (fmap prettyShow (getDictionary dictEqInt)),
      "isDictionary Int:              " ++ show (isDictionary (typeRep (Proxy :: Proxy Int))),
      "",
      h1 "Value",
      "show (toValue (Identity 42)):        " ++ show val42,
      "valueType:                           " ++ prettyShow (valueType val42),
      "fromValue @Int  (matching type):     " ++ show (fromValue val42 :: Maybe (Identity Int)),
      "fromValue @Bool (wrong type):        " ++ show (fromValue val42 :: Maybe (Identity Bool))
    ]
  where
    dictEqInt = typeRep (Proxy :: Proxy (Dict (Eq Int)))
    val42 = toValue (Identity (42 :: Int))
