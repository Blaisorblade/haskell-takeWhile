module IntToString where

import Prelude hiding (takeWhile)
import GHC.Exts

--takeWhile' :: (a -> Bool) -> [a] -> [a]
--takeWhile' p xs = build $ \c n -> foldr (takeWhileFB p c n) n xs
--{-# INLINE takeWhile' #-}

takeWhileFB p c n x xs = if p x then x `c` xs else n
{-# INLINE [0] takeWhileFB #-}

{-# NOINLINE [1] takeWhile #-} -- We want the RULE to fire first.
takeWhile               :: (a -> Bool) -> [a] -> [a]
takeWhile _ []          =  []
takeWhile p (x:xs)
            | p x       =  x : takeWhile p xs
            | otherwise =  []

{-
-- STUPID
"takeWhile/backBad"    [1] forall p xs. takeWhile' p xs = takeWhile p xs
-}

-- Why can't I use, on the RHS, a function I mark with INLINE such as takeWhile' above? If I do that, the final program contains takeWhile. Probably just a phase ordering problem.
{-# RULES
"takeWhile/fuse"    [~1] forall p xs. takeWhile p xs = build $ \c n -> foldr (takeWhileFB p c n) n xs
"takeWhile/back"   [1] forall p xs. foldr (takeWhileFB p (:) []) [] xs = takeWhile p xs
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
