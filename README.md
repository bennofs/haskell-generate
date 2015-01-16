haskell-generate
================

[![Build Status](https://secure.travis-ci.org/bennofs/haskell-generate.png?branch=master)](http://travis-ci.org/bennofs/haskell-generate)

## Introduction

If you want to generate haskell source code, you could build up haskell-src-exts AST and then pretty print it. But that's easy to screw up, because haskell-src-exts doesn't include tag it's AST with a type. This library aims to fill the gap, adding type information to haskell-src-exts expressions and also managing imports for you.

## Getting started

First, you need to import this library:

```haskell
import Language.Haskell.Generate
```

This module reexports `Language.Haskell.Exts.Syntax`, because haskell-generate builds on top of that.

There are two main types in haskell-generate. The first is the monad `Generate` and the type alias `ExpG`. The `Generate` monad is used to track required imports. It also allows to generate unique names. `ExpG t` is just an action in the `Generate` monad that returns an expression of type `t`.

How do you build expressions? There is a number of predefined expressions for the functions in the
Prelude. This allows you to just use these and combine them to new expressions. For example, let's define a expression that reads a file called "names":

```haskell 
readNamesFile' :: ExpG (IO String)
readNamesFile' = readFile' <>$ expr "names"
```

Here we use `(<>$)` to apply the `readFile'` expression to the string `names`. `readFile'` is one of the expressions already provided by haskell-generate. All expressions that are provided by haskell-generate end with an apostrophe. You can find more of them in the module `Language.Haskell.Generate.PreludeDef`. The `expr` function is used to lift the string `names` into an expression of type `ExpG String`.

Now that we have an expression, we need to bind it to a name in a module. For this job, we use another monad, the `ModuleM` monad. It allows you to bind expressions to names and then generate
a module with those names.

Here's how we generate our module:

```haskell
myModule :: ModuleG
myModule = do
  d <- addDecl (Ident "main") $ applyE2 bind' readNamesFile' putStrLn'
  return $ Just [exportFun d]
```

`ModuleG` is again a type synonym for an action in the `ModuleM` monad. It must either return Nothing (which omits the export list) or an export list. In this case, we export the "main" function, which we previously defined using `addDecl`.

The only thing left to do is to generate the actual source code for the module, for which we
use the `generateModule` function, which takes the module name as an argument:

```haskell
main :: IO ()
main = putStrLn $ generateModule myModule "Main"
```

If you run the program, you'll get the following output:

```haskell
module Main (main) where
import qualified GHC.Base
import qualified System.IO
main
  = (GHC.Base.>>=) (System.IO.readFile ['n', 'a', 'm', 'e', 's'])
      System.IO.putStrLn
```

If you run this code, you'll get the contents of the "names" file. The code is a bit ugly and uses
qualified imports to avoid name clashes, but it works.

## Importing functions

Until now, we've only used the predefined expressions from `Language.Haskell.Generate.PreludeDef`, but often you'll want to use definitions from other modules that you might want to use.

You can do that using the `useValue` from haskell-generate. Let's look at the type of `useValue`:

```haskell
useValue :: String -> Name -> ExpG t
```

`useValue` takes a module name in which the function is defined and the name of the function. It returns an expression of any type you which. This function is unsafe, because it cannot check that the returned type is actually the type of the function. That's why you usually given `useValue` an explicit type signature.

For example, suppose we want to use the function `permutations` from Data.List. We write the following definition for it:

```haskell
permutations' :: ExpG ([a] -> [[a]]) -- Here we given an explicit type for permutations'. This is not checked, so make sure it's actually right!
permutations' = useValue "Data.List" (Ident "permutations") -- "permutations" is an identifier, not a symbol, so we use the "Ident" constructor.
```

## Using TH to automagically import functions

If the function you want to import is already available at compile time, you can use the template haskell code from `Language.Haskell.Generate.TH` to generate the expression definitions. This is the approach we use for the Prelude, as an example.

Using the example from the previous section, we could also import the `permutations` function like this:

```haskell
-- at the top of the file:
{-# LANGUAGE TemplateHaskell #-} -- Enable template haskell
import Data.List (permutations) -- The function needs to be available at compile time

declareFunction 'permutations -- This generates the same code as above, but is more type-safe because you don't have to specify the type yourself.
```


## Contributing

If you have an idea, a question or a bug report, open an issue on github. You can also find me on freenode in the #haskell channel, my nick is bennofs.
