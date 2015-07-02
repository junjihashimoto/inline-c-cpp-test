{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
import qualified Language.C.Inline.Cpp as CPP
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C
import Language.Haskell.TH

import Foreign hiding (new)
import GHC.Real
import Foreign.C.String
import Foreign.C.Types
import qualified Data.Map as Map
import Data.Monoid

data Hoge

  
C.context $ CPP.cppCtx <> mempty {
  C.ctxTypesTable = Map.fromList [
     (C.TypeName "Hoge", [t|Hoge|])
  ]
}


C.include "<iostream>"
C.include "hoge.h"

class Object o where
  new :: IO (Ptr o)
  delete :: Ptr o -> IO ()

instance Object Hoge where  
  new = [C.exp| Hoge * {new Hoge}|]
  delete ptr = [C.exp| void { delete $(Hoge * ptr) }|]


main :: IO ()
main = do
  let x = 3
  ptr <- new :: IO (Ptr Hoge)
  print 123
  print [C.pure| int { $(Hoge * ptr)->get() }|]
  print 123
  let a = 100
  [C.exp| void { $(Hoge *ptr)->set($(int a))}|]
  print [C.pure| int { $(Hoge * ptr)->get()}|]
  delete ptr
  [C.block| void {
      std::cout << "Hello, world!" << $(int x) << std::endl;
    } |]
