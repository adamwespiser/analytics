{-| Description:
The purpose of the module is to repro simple functions
so we can golf down our dependencies, and store common
functions
 -}
module Utils (
  isLeft
  , isRight
  , headMay
) where

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

isRight :: Either a b -> Bool
isRight = not . isLeft

headMay :: [a] -> Maybe a
headMay = Prelude.foldr (\x _ -> Just x) Nothing

