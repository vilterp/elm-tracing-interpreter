module Utils exposing (..)
-- where

getMaybe : String -> Maybe a -> a
getMaybe msg maybe =
  case maybe of
    Just x ->
      x

    Nothing ->
      Debug.crash msg


-- TODO: get this into list-extra
-- geez
mapWithIndex : (Int -> a -> b) -> List a -> List b
mapWithIndex f list =
  let
    go idx items =
      case items of
        [] ->
          []

        x::xs ->
          (f idx x) :: (go (idx + 1) xs)
  in
    go 0 list
