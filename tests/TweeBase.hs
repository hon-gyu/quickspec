-- Showcase of Twee.Base term operations.
--
-- QuickSpec uses TyCon as its function symbol type, so we do the same here.
-- Types in QuickSpec are Twee terms: `type Type = Term TyCon`.

module TweeBase where

import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Proxy
import qualified Data.Text as Text
import QuickSpec.Internal.Type
import System.FilePath ((<.>), (</>))
import Test.Tasty (TestTree)
import Test.Tasty.Silver (goldenVsAction)
import Twee.Base (prettyShow)
import Twee.Term (Subst, match, substToList, unify)
import Utils

tests :: TestTree
tests = goldenVsAction "TweeBase" path (return output) Text.pack
  where
    path = "tests" </> "TweeBase" <.> "expected"

-- Some terms to play with.
-- Type vars (Twee variables):
x0, x1 :: Type
x0 = typeVar -- A, shown as "x0" raw or "X" pretty
x1 = typeRep (Proxy :: Proxy B) -- B, shown as "x1" raw or "Y" pretty

-- Ground types (Twee constants/apps):
intTy, boolTy, charTy :: Type
intTy = typeRep (Proxy :: Proxy Int)
boolTy = typeRep (Proxy :: Proxy Bool)
charTy = typeRep (Proxy :: Proxy Char)

-- Function types built with arrowType:
x0x1 = arrowType [x0] x1 -- A -> B

x0x0 = arrowType [x0] x0 -- A -> A

intBool = arrowType [intTy] boolTy -- Int -> Bool

intInt = arrowType [intTy] intTy -- Int -> Int

showSubst :: Subst TyCon -> String
showSubst s =
  "{" ++ intercalate ", " (map binding (substToList s)) ++ "}"
  where
    binding (v, t) = prettyShow v ++ " -> " ++ prettyShow t

showMatch :: Type -> Type -> String
showMatch pat t =
  fromMaybe "Nothing" (fmap showSubst (match pat t))

showUnify :: Type -> Type -> String
showUnify t1 t2 =
  fromMaybe "Nothing" (fmap showSubst (unify t1 t2))

output :: String
output =
  unlines
    [ h1 "terms (raw show / prettyShow)",
      row "x0" x0,
      row "x1" x1,
      row "Int" intTy,
      row "Int->Bool" intBool,
      row "A->B" x0x1,
      row "A->A" x0x0,
      "",
      h1 "match pat term  (one-sided: vars in pat bind to subterms of term)",
      "match A     Int          = " ++ showMatch x0 intTy,
      "match A->B  Int->Bool    = " ++ showMatch x0x1 intBool,
      "match A->A  Int->Bool    = " ++ showMatch x0x0 intBool,
      "match Int   A            = " ++ showMatch intTy x0,
      "",
      h1 "unify  (two-sided: vars on either side can bind)",
      "unify A     Int          = " ++ showUnify x0 intTy,
      "unify A->B  Int->Bool    = " ++ showUnify x0x1 intBool,
      "unify A->A  Int->Bool    = " ++ showUnify x0x0 intBool,
      "unify A->B  Int->A       = " ++ showUnify x0x1 (arrowType [intTy] x0),
      "",
      h1 "applying a substitution",
      let Just s = unify x0x1 intBool
       in unlines
            [ "sub = unify (A->B) (Int->Bool) = " ++ showSubst s,
              "apply sub to (A->B):  " ++ prettyShow (typeSubst s x0x1),
              "apply sub to (A->A):  " ++ prettyShow (typeSubst s x0x0),
              "apply sub to Char:    " ++ prettyShow (typeSubst s charTy)
            ]
    ]
  where
    row label t = label ++ ":  show=" ++ show t ++ "  pretty=" ++ prettyShow t
