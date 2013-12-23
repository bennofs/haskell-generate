{-# LANGUAGE TemplateHaskell #-}
module Language.Haskell.Generate.TH 
  ( -- | This module provides functions for automagically generating type-safe ExpG definitions from functions. For an example on how to use this, 
    -- you can look at the 'Language.Haskell.Generate.Prelude' module.
    declareFunction
  , declareNamedSymbol
  , declareNamedFunction
  , declareNamedThing
  ) where

import Data.Char
import Language.Haskell.Exts.Syntax hiding (Name)
import Language.Haskell.Generate.Monad hiding (Name)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

-- | Make a ExpG for the given function, using the given name for the definition.
declareNamedFunction :: (Name, String) -> DecsQ
declareNamedFunction (func, name) = declareNamedThing (func, name, 'Ident)

-- | Make a ExpG for some thing, using the given name for the definition. The third tuple element
-- specifies the constructor to use for constructing the Name. This can either be @'Symbol@ (for symbols)
-- or @'Ident@ (for functions).
declareNamedThing :: (Name, String, Name) -> DecsQ
declareNamedThing (thing, name, thingClass) = do
  info <- reify thing
  typ <- case info of
    VarI _ t _ _ -> return t
    ClassOpI _ t _ _ -> return t
    DataConI _ t _ _ -> return t    
    _ -> fail $ "Not a function: " ++ nameBase thing
  md <- maybe (fail "No module name for function!") return $ nameModule thing
  sequence
    [ sigD (mkName name) $ return $ overQuantifiedType (ConT ''ExpG `AppT`) typ
    , funD (mkName name) $ return $ flip (clause []) [] $ normalB 
        [| useValue $(lift md) $ $(conE thingClass) $(lift $ nameBase thing) |]
    ]

  where overQuantifiedType f (ForallT bnds ctx t) = ForallT (map removeKind bnds) ctx $ overQuantifiedType f t
        overQuantifiedType f x = f x

        removeKind :: TyVarBndr -> TyVarBndr
        removeKind (KindedTV n _) = PlainTV n
        removeKind x = x

-- | Declare a symbol, using the given name for the definition.
declareNamedSymbol :: (Name, String) -> DecsQ
declareNamedSymbol (func, name) = declareNamedThing (func, name, 'Symbol)

-- | Declare a function. The name of the definition will be the name of the function with an added apostrophe. (Example: declareFunction 'add generates 
-- a definition with the name add').
declareFunction :: Name -> DecsQ
declareFunction func = declareNamedFunction (func, funcName ++ "'")
  where funcName = case nameBase func of
          (h:t) -> toLower h:t
          x     -> x
