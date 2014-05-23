module IntToString where

import Prelude hiding (takeWhile)
import GHC.Exts

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p xs = build $ \c n -> foldr (takeWhileFB p c n) n xs
{-# INLINE takeWhile' #-}

takeWhile               :: (a -> Bool) -> [a] -> [a]
{-# NOINLINE [1] takeWhile #-} -- We want the RULE to fire first.
takeWhile _ []          =  []
takeWhile p (x:xs)
            | p x       =  x : takeWhile p xs
            | otherwise =  []


takeWhileFB p c n x xs = if p x then x `c` xs else n
{-# INLINE [0] takeWhileFB #-}

{-
"takeWhile/fuse" [~1]
  forall p xs . takeWhile p xs = build $ \c n -> foldr (takeWhileFB p c n) n xs
 -}

{- This rule works only without takeWhile/back, because apparently inlining is
done in the phase after this rule is applied - phase 1. And in that phase, even
build is inlined, preventing fusion from working. But then, why does it work
when the back rule is not there? -}

{-# RULES
"takeWhile/fuseBad" [~1]
  forall p xs . takeWhile p xs = takeWhile' p xs
"takeWhile/back" [0]
  forall p xs . foldr (takeWhileFB p (:) []) [] xs = takeWhile p xs
 #-}


toChar digit = toEnum $ digit + fromEnum '0'


intToString i =
  if i < 0 then
     '-' : digits
   else
     digits
  where
    digits =
      reverse . map (toChar . (`mod` 10)) . takeWhile (/=0) . iterate (`div` 10) . abs $ i
