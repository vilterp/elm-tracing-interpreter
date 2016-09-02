module Utils exposing (..)


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


-- this is kind of silly
getResult : Result a a -> a
getResult res =
  case res of
    Ok x ->
      x

    Err y ->
      y


-- @#$%!@#
--withAlpha : Color -> Float -> Color
--withAlpha color alpha =
--  case color of
--    HSLA h s l _ ->
--      HSLA h s l alpha

--    RGBA r g b _ ->
--      RGBA r g b alpha
