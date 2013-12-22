module Language.Haskell.Generate
  ( -- $intro
    module X
  ) where

import Language.Haskell.Generate.Base as X
import Language.Haskell.Generate.PreludeDef as X

{- $intro
    This library allows you to generate type-correct haskell expressions. To use it, you need to import this module:

> import Language.Haskell.Generate

    This will import the methods for generating haskell expressions and also a list of predefined expressions consisting of functions from the haskell Prelude 
    that you can use to build your own expressions. For example, here is an expression that reads the file 'names':

> readNames :: ExpG (IO String) 
> readNames = readFile' <>$ expr "names"

    First, look at the type of readNames. @'ExpG'@ is a data type provided by haskell-generate for haskell expressions. We want to generate an expression
    that returns an IO String, so the type of readNames is @ExpG (IO String)@. 
    @readFile'@ is an expression for the haskell readFile function. You can use almost any Prelude function in your expression by adding an apostrophe to
    the end of the function name.
    We use (<>$) to generate a expression that applies 'readFile' to the expression "names". @'expr'@ automagically generates a literal expression.

    Now that we have an expression to read the @names@ file, we can generate a module with it:

> myModule :: ModuleG
> myModule = do
>   readNamesDecl <- addDecl (Ident "readNames") readNames
>   return $ Just [exportFun readNamesDecl]

    

> main :: IO ()
> main = do
>   putStrLn $ generateModule myModule "FooBar"

-}
