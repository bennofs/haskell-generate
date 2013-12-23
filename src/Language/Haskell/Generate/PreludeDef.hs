{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
module Language.Haskell.Generate.PreludeDef where

import Language.Haskell.Exts.Syntax
import Language.Haskell.Generate.Monad
import Language.Haskell.Generate.TH

--------------------------------------------------------------------------------
-- Basic functions

fmap concat $ mapM declareFunction 
  [ 'maybe
  , 'either
  , 'fst
  , 'snd
  , 'curry
  , 'uncurry
  , 'not
  , 'negate, 'abs, 'signum, 'fromInteger
  , 'quot, 'rem, 'div, 'mod, 'quotRem, 'divMod, 'toInteger
  , 'recip, 'fromRational
  , 'pi, 'exp, 'log, 'sqrt, 'logBase, 'sin, 'cos, 'tan
  , 'asin, 'acos, 'atan, 'sinh, 'cosh, 'tanh, 'asinh, 'acosh, 'atanh
  , 'properFraction, 'truncate, 'round, 'ceiling, 'floor
  , 'floatRadix, 'floatDigits, 'floatRange, 'decodeFloat
  , 'encodeFloat, 'exponent, 'significand, 'scaleFloat, 'isNaN
  , 'isInfinite, 'isDenormalized, 'isIEEE, 'isNegativeZero, 'atan2
  , 'subtract, 'even, 'odd, 'gcd, 'lcm
  , 'fromIntegral, 'realToFrac
  , 'fmap, 'return, 'mapM, 'mapM_
  , 'id, 'const, 'flip, 'until, 'asTypeOf, 'undefined
  , 'map, 'filter, 'head, 'last, 'tail, 'init, 'null, 'length
  , 'reverse, 'foldl, 'foldr, 'foldl1, 'foldr1, 'and, 'or, 'any, 'all, 'sum, 'product
  , 'concat, 'concatMap, 'maximum, 'minimum
  , 'scanl, 'scanr, 'scanl1, 'scanr1
  , 'iterate, 'repeat, 'replicate, 'cycle
  , 'take, 'drop, 'splitAt, 'takeWhile, 'dropWhile, 'span, 'break
  , 'elem, 'notElem, 'lookup
  , 'zip, 'zip3, 'zipWith, 'zipWith3, 'unzip, 'unzip3
  , 'lines, 'words, 'unlines, 'unwords
  , 'read, 'show
  , 'putChar, 'putStr, 'putStrLn, 'print
  , 'getChar, 'getLine, 'getContents, 'interact
  , 'readFile, 'writeFile, 'appendFile, 'readIO, 'readLn
  , 'Just, 'Left, 'Right, 'False, 'True, 'Nothing
  ]

fmap concat $ mapM declareNamedSymbol
  [ ('(.), "dot'")
  , ('(+), "add'")
  , ('(*), "mult'")
  , ('(/), "divide'")
  , ('(**), "floatPow'")
  , ('(>>=), "bind'")
  , ('(>>), "then'")
  , ('(++), "append'")
  , ('(!!), "index'")
  , ('(==), "equal'")
  ]

(<>.) :: ExpG (b -> c) -> ExpG (a -> b) -> ExpG (a -> c)
(<>.) a b = dot' <>$ a <>$ b

tuple0 :: ExpG ()
tuple0 = returnE $ Var $ Special UnitCon

tuple2 :: ExpG (a -> b -> (a,b))
tuple2 = returnE $ Var $ Special $ TupleCon Boxed 2

tuple3 :: ExpG (a -> b -> c -> (a,b,c))
tuple3 = returnE $ Var $ Special $ TupleCon Boxed 3

tuple4 :: ExpG (a -> b -> c -> d -> (a,b,c,d))
tuple4 = returnE $ Var $ Special $ TupleCon Boxed 4

tuple5 :: ExpG (a -> b -> c -> d -> (a,b,c,d,e))
tuple5 = returnE $ Var $ Special $ TupleCon Boxed 5

cons :: ExpG (a -> [a] -> [a])
cons = returnE $ Var $ Special Cons

instance Num t => Num (ExpG t) where
  a + b = add'  <>$ a <>$ b
  a - b = flip' <>$ subtract' <>$ a <>$ b
  a * b = mult' <>$ a <>$ b
  negate a = negate' <>$ a
  abs a    = abs'    <>$ a
  fromInteger a = returnE $ Lit $ Int a
  signum a = signum' <>$ a

