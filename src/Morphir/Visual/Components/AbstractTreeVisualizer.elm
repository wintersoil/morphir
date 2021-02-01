import List
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)

absString: String
absString = "AOB -> Add -> [ AVL (Variable) -> F1 , ADB -> Divide -> [ AVL (Variable) -> F2 , AVL (Apply): AVL (Apply): AVL (Reference) -> Morphir.SDK:Basics:multiply __ AVL (Variable) -> F3 __ AVL (Variable) -> F4 ] ]"

chopWord : List Int -> Int -> Int -> String -> List String -> List String
chopWord indices beg end stringy listy=
  if List.length indices > 0 then
    chopWord (List.drop 1 indices) (end + 2) (Maybe.withDefault 0 (List.head (List.drop 1 indices))) stringy (listy ++ [(String.slice (beg) end stringy)])
  else
    (listy ++ [(String.slice beg (String.length stringy) stringy)])


createTreeFromString : String -> List (List String)
createTreeFromString inputString =
  chopWord (String.indexes "," absString) 0 (Maybe.withDefault 0 (List.head (String.indexes "," absString))) inputString []
  |> createFurtherTree []
  
createFurtherTree: List String -> List String -> List (List String)
createFurtherTree finalList listy =
  (List.map (\step -> String.split "->" step) listy)
  
drawTree: List (List String) -> List (Html msg) -> List (List (Html msg))
drawTree listAll htmlAggregation =
  List.indexedMap (\indO elem -> List.indexedMap (\indI elemInner -> div [Html.Attributes.style "font-family" "consolas", Html.Attributes.style "padding" "10px 20px", Html.Attributes.style "position" "absolute", Html.Attributes.style "top" ((String.fromInt (400 + ((indO + indI) * 60))) ++ "px"), Html.Attributes.style "left" ((String.fromInt (30 + (indO * 180))) ++ "px")] [Html.text elemInner] ) elem ) listAll

displayTextTree: List (List String) -> List (Html msg)
displayTextTree listAll=
  (List.map (\i -> 
                        div
                        [
                        Html.Attributes.style "font-family" "consolas",
                        Html.Attributes.style "background-color" "rgb(96, 202, 245)",
                        Html.Attributes.style "color" "#111",
                        Html.Attributes.style "margin-bottom" "5px",
                        Html.Attributes.style "padding" "10px 20px"]
                        ((List.map (\j -> 
                        div
                        [] [Html.text j]) i))
                      ) listAll)

positionsX: List Int
positionsX = [30, 100, 200]

main =
  div [] [(svg
    [ Svg.Attributes.width "1200"
    , Svg.Attributes.height "1200"
    , viewBox "0 0 1200 1200"
    ,Html.Attributes.style "position" "absolute"
    ]
    [
    polygon [ fill "#6ec0ff", points "70,425 70,465 75,465 75,425" ] [],
     polygon [ fill "#6ec0ff", points "70,485 70,525 75,525 75,485" ] [],
     polygon [ fill "#6ec0ff", points "70,550 70,590 75,590 75,550" ] []
    ]),
  div [] [div [] (displayTextTree (createTreeFromString absString)), div [] (List.map (\p -> div [] p) (drawTree (createTreeFromString absString) []) )] ]