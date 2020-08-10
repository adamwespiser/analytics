{-| Description:
The purpose of the module is to repro simple functions
so we can golf down our dependencies, and store common
functions
 -}
module Utils (
  isLeft
  , isRight
) where

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

isRight :: Either a b -> Bool
isRight = not . isLeft
