{-|
Module      : UniPattern
Description : Helpers which allow safe partial pattern matching in lambdas
Copyright   : (c) Chris Penner, 2019
License     : BSD3

This library provides helpers for using unipatterns safely; what's a unipattern you ask?

Have you ever wanted to match on a really large expression in-line but don't want to bother pulling out a whole case-statement?

Scrap your case statements with unipattern matches!

For example 'maybeMatch' will detect failed pattern matches and will inject the result into 'Maybe'

>>> maybeMatch (\[a] -> show a) [1, 2, 3]
Nothing
>>> maybeMatch (\[a] -> show a) [1]
Just "1"

Most other operations provide different failure modes; for instance returning the original argument, returning 'empty', or using a provided failure handler.

It turns out this is pretty handy when using scrap-your-boilerplate operations:

> everywhere (mkT (match (\"hidden" -> "found")))

This searches through a generic structure and will map any @"hidden"@ values into @"found"@; and will leave everything else alone.

You can also use this with humble fmap, inside monadic binds, etc.
Anywhere that you really only care about one particular pattern, and want some trivial behaviour for the others.
The following changes all Lefts into Rights, but leaves the Rights alone.

>>> match (\(Left n, x) -> (Right (show n), x)) <$> [(Right "a", 1), (Left 10, 2)]
[(Right "a",1),(Right "10",2)]

Does this library really need to exist? Probably not, but there are times when it's handy to have.

-}
module UniPatterns
    ( maybeMatch
    , onMismatch
    , match
    , orMatch
    , matchM
    , matchAlt
    , matchOneOf
    , (||>)
    ) where

import Data.Maybe
import Control.Exception
import System.IO.Unsafe
import Control.Applicative
import Data.Foldable

-- | Force a value possibly containing a Pattern Match failure. Re-raise all other exceptions
tryMatch :: a -> Maybe a
tryMatch a = (unsafePerformIO $ (return $! Just $! a) `catch` (\(PatternMatchFail _) -> pure Nothing))

-- | Run a function returning 'Nothing' if function fails to pattern match.
--
-- >>> maybeMatch (\[a] -> a + 10) [5]
-- Just 15
-- >>> maybeMatch (\[a] -> a + 10) []
-- Nothing
-- >>> maybeMatch (\[a] -> a + 10) [1, 2, 3]
-- Nothing
maybeMatch :: (a -> b) -> a -> Maybe b
maybeMatch f a = tryMatch (f a)

-- | Run the function any time it pattern matches, otherwise behaves like 'id'
--
-- >>> match (\((x, Right n)) -> (x, Right (n * 100))) ("a", Right 3)
-- ("a",Right 300)
-- >>> match (\((x, Right n)) -> (x, Right (n * 100))) ("a", Left "bad")
-- ("a",Left "bad")
match :: (a -> a) -> (a -> a)
match f a = orMatch a f a

-- | Try to run a function, on pattern match fail run the provided handler instead.
--
-- >>> reverse `onMismatch` (\"needle" -> "Found It") $ "haystack"
-- "kcatsyah"
-- >>> reverse `onMismatch` (\"needle" -> "Found It") $ "needle"
-- "Found It"
onMismatch :: (a -> b) -- ^ This is run on a pattern match failure
           -> (a -> b) -- ^ A function which may fail to pattern match
           -> (a -> b)
onMismatch handleMismatch f a = fromMaybe (handleMismatch a) (maybeMatch f a)

-- | Try to run a function, on pattern match fail return the given value instead
--
-- >>> "default" `orMatch` (\10 -> "found 10") $ 10
-- "found 10"
-- >>> "default" `orMatch` (\10 -> "found 10") $ 3
-- "default"
orMatch :: b -> (a -> b) -> (a -> b)
orMatch def = onMismatch (const def)

-- | Try to run an effectful function, on pattern match fail behave like 'pure'
--
-- >>> matchM (\'a' -> Just 'A') 'a'
-- Just 'A'
-- >>> matchM (\'a' -> Just 'A') 'x'
-- Just 'x'
matchM :: Applicative f => (a -> f a) -> a -> f a
matchM = onMismatch pure

-- | Try to run a function resulting in an 'Alternative', on pattern match fail return 'empty'
--
-- >>> matchAlt (\'a' -> Just 22) 'a'
-- Just 22
-- >>> matchAlt (\'a' -> Just 22) 'x'
-- Nothing
matchAlt :: Alternative f => (a -> f b) -> a -> f b
matchAlt = onMismatch (const empty)


-- | Infix version of @flip 'onMismatch'@
-- It tries the patterns in order from left to right.
--
-- The last handler in the chain MUST be total or the chain could fail with a
-- 'PatternMatchFail'
--
-- >>> (\1 -> "got 1") ||> (\2 -> "got 2") ||> show $ 1
-- "got 1"
-- >>> (\1 -> "got 1") ||> (\1 -> "again") ||> (\2 -> "got 2") ||> show $ 2
-- "got 2"
-- >>> (\1 -> "got 1") ||> (\1 -> "again") ||> (\2 -> "got 2") ||> show $ 3
-- "3"
infixr 9 ||>
(||>) :: (a -> b) -> (a -> b) -> (a -> b)
(||>) = flip onMismatch

-- | Try each unipattern in the list in order until one matchs
-- if none match fall back to the provided handler
--
-- >>> :{
--   let handlers =
--         [ \1 -> "got 1"
--         , \1 -> "this will never run"
--         , \2 -> "got 2"
--         ]
--     :}
--
-- >>> matchOneOf show handlers 1
-- "got 1"
-- >>> matchOneOf show handlers 2
-- "got 2"
-- >>> matchOneOf show handlers 3
-- "3"
matchOneOf :: (a -> b) -- ^ Handler for the failure case
           -> [a -> b] -- ^ List of patterns to try
           -> (a -> b)
matchOneOf handler = foldr' (||>) handler
