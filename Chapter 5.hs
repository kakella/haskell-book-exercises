module Chapter5 where

nonsense :: Bool -> Integer
nonsense True  = 100
nonsense False = 200

curriedFunction :: Integer -> Bool -> Integer
curriedFunction i b = i + nonsense b

uncurriedFunction :: (Integer, Bool) -> Integer
uncurriedFunction (i, b) = i + nonsense b

anonymousCurried :: Integer -> Bool -> Integer
anonymousCurried = \i b -> i + nonsense b

anonymousNestedCurried :: Integer -> Bool -> Integer
anonymousNestedCurried = \i -> \b -> i + nonsense b

anonymousUncurried :: (Integer, Bool) -> Integer
anonymousUncurried = \(i, b) -> i + nonsense b

-- anonymousNestedUncurried :: Integer -> Bool -> Integer
-- anonymousNestedUncurried not possible


-- f takes input of type t and returns output of type t1 (t -> t1)
-- curry1 takes input of type (t -> t1) and t, to return output of type t1
curry1 :: (t -> t1) -> t -> t1
curry1 f a = f a

-- f takes one input of type (t1, t2) to return output of type t (t1, t2) -> t. This is an uncurried function
-- curry2 takes 3 inputs of types ((t1, t2) -> t), t1, t2 and returns output of type t, i.e. it applies the uncurried function to remaining inputs and returns the result
-- So, curry2 provides a way to apply an uncurried function in a curried manner
-- Eg: fst (1, 2) = 1
--     curry2 fst 1 2 = 1
curry2 :: ((t1, t2) -> t) -> t1 -> t2 -> t
curry2 f a b = f (a, b)

-- f takes one input of type t1 and returns output of type t (t1 -> t)
-- curry3 takes 3 inputs of types (t1 -> t), t1, t1 and returns output of type (t, t)
curry3 :: (t1 -> t) -> t1 -> t1 -> (t, t)
curry3 f a b = (f a, f b)

-- f is a curried function that takes two parameters of types t1 and t2 as input, and returns an output of type t (t1 -> t2 -> t)
-- uncurry2 takes a curried function as input, along with two uncurried parameters i.e. as a tuple (t1, t2), and applies the curried function to the uncurried parameters to return output of type t
-- So, uncurry2 provides a way to apply a curried function to uncurried parameters
-- Eg: (+) 1 2 = 3
--     uncurry2 (+) (1, 2) = 3
uncurry2 :: (t1 -> t2 -> t) -> (t1, t2) -> t
uncurry2 f (a, b) = f a b


-- Currying and uncurrying functions with 3 arguments:
curry4 :: ((t1, t2, t3) -> t) -> t1 -> t2 -> t3 -> t
curry4 f a b c = f (a, b, c)

uncurry4 :: (t1 -> t2 -> t3-> t) -> (t1, t2, t3) -> t
uncurry4 f (a, b, c) = f a b c

-- Exercises
f :: a -> a -> a -> a
f = undefined

x :: Char
x = undefined

g :: a -> b -> c -> b
g = undefined

h :: (Num a, Num b) => a -> b -> b
h = undefined

jackal :: (Ord a, Eq b) => a -> b -> a
jackal = undefined

-- special: Check :type kessel 1 2
kessel :: (Ord a, Num b) => a -> b -> a
kessel = undefined

parametric1 :: a -> a -> a
parametric1 p1 p2 = p1

parametric2 :: a -> a -> a
parametric2 p1 p2 = p2

parametric3 :: a -> b -> b
parametric3 p1 p2 = p2

f' x y = x + y + 3

bigNum = (^) 5 $ 10
