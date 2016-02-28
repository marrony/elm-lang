import Html exposing (..)

myMap : (a -> b) -> List a -> List b
myMap fn list =
  case list of
  [] -> []
  head :: tail -> fn head :: myMap fn tail

myFoldl : (a -> b -> b) -> b -> List a -> b
myFoldl fn ini list =
  case list of
  [] -> ini
  head :: tail -> myFoldl fn (head `fn` ini) tail

myFoldr : (a -> b -> b) -> b -> List a -> b
myFoldr fn ini list =
  case list of
  [] -> ini
  head :: tail -> head `fn` (myFoldr fn ini tail)

test : List Int
test = myMap (\x -> x*2) [1..5]

test2 : Int
test2 = myFoldl (\a b -> a + b) 0 [1..5]

test3 : List Int
test3 = myFoldl (::) [] [1,2,3]

main : Html
main = Html.text (toString test3)
