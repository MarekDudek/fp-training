module Lib where

someFunc :: IO ()
someFunc = do
    print r1
    print r2
    print r3
    print r4
    print r5
    print r6
    print r7
    print r8
    print r9



type OnlyType = Int

--type F32 = forall a. a (a, a, a) -> (a, a)


f1:: (a, a, a) -> (a, a)
f1 (x, y, z) = (x, x)

-- x -> i1 -> i2 -> (x,x)

f2:: (a, a, a) -> (a, a)
f2 (x, y, z) = (x, y)
--
-- x -> y -> i2 -> (x,y)

f3:: (a, a, a) -> (a, a)
f3 (x, y, z) = (x, z)

-- z -> ig -

f4:: (a, a, a) -> (a, a)
f4 (x, y, z) = (y, x)

f5:: (a, a, a) -> (a, a)
f5 (x, y, z) = (y, y)

f6:: (a, a, a) -> (a, a)
f6 (x, y, z) = (y, z)

f7:: (a, a, a) -> (a, a)
f7 (x, y, z) = (z, x)

f8:: (a, a, a) -> (a, a)
f8 (x, y, z) = (z, y)

f9:: (a, a, a) -> (a, a)
f9 (x, y, z) = (z, z)

-- functions (a, a, a) -> a
-- 3 possibilities 3^1
-- functions (a, a, a) -> (a, a)
-- all combinations
-- so its n^m przy a[n] -> a[m]
-- 
-- But functions are curried. So
--
-- a[n] -> a[m] is really a -> a -> a -> ... -> a (n+m) times. what if n == 0 or m == 0? provider and consumer? constructor and destructor? is there a difference?
-- 
--
-- these were generic functions, without any knowledge of the type.
-- what if we know about it.

r1 = f1 (1, 2, 3)
r2 = f2 (1, 2, 3)
r3 = f3 (1, 2, 3)
r4 = f4 (1, 2, 3)
r5 = f5 (1, 2, 3)
r6 = f6 (1, 2, 3)
r7 = f7 (1, 2, 3)
r8 = f8 (1, 2, 3)
r9 = f9 (1, 2, 3)


-- functions that take digits to build a decimal number
-- another set of functions of the same signatures or at least similar to track last card game
-- to track salary, raise by percent, allowed thresholds?
-- to randomly walk around
--   random start, within range
--   multiplications and divisions by 2
-- to track some moving signal
--

multBy c n = c * n

double n = multBy 2


