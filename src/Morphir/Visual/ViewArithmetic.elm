module Morphir.Visual.ViewArithmetic exposing (..)

import Dict exposing (Dict)
import Element exposing (Element, centerX, column, fill, padding, paddingEach, rgb, row, spacing, text, width)
import Element.Border as Border
import Element.Font exposing (alignRight, center, justify)
import Morphir.Graph.GraphViz.AST exposing (Attribute(..))
import Morphir.IR.FQName exposing (FQName(..))
import Morphir.IR.Name as Name exposing (Name)
import Morphir.IR.Path as Path exposing (Path)
import Morphir.IR.Type exposing (Type)
import Morphir.IR.Value as Value exposing (RawValue, TypedValue, Value)
import Morphir.Visual.Components.AritmeticExpressions exposing (ArithmeticOperator(..), ArithmeticOperatorTree(..))
import Morphir.Visual.Context exposing (Context)
import Morphir.Visual.ViewLiteral as ViewLiteral


view : (TypedValue -> Element msg) -> ArithmeticOperatorTree -> Element msg
view viewValue arithmeticOperatorTree =
    case arithmeticOperatorTree of
        ArithmeticValueLeaf typedValue ->
            viewValue typedValue

        ArithmeticDivisionBranch arithmeticOperatorTree1 arithmeticOperatorTree2 ->
            case arithmeticOperatorTree1 of
                ArithmeticValueLeaf typedValue1 ->
                    case arithmeticOperatorTree2 of
                        ArithmeticValueLeaf typedValue2 ->
                            column [ centerX, width Element.fill ]
                                [ row [ centerX, width Element.fill ]
                                    [ row
                                        [ width Element.fill
                                        , spacing 5
                                        , Border.color (rgb 0 0.7 0)
                                        , paddingEach { left = 0, top = 0, right = 0, bottom = 4 }
                                        , centerX
                                        ]
                                        [ viewValue typedValue1
                                        ]
                                    ]
                                , row
                                    [ centerX
                                    , width Element.fill
                                    , Border.solid
                                    , Border.widthEach { bottom = 0, left = 0, right = 0, top = 1 }
                                    , paddingEach { left = 0, bottom = 0, right = 0, top = 10 }
                                    ]
                                    [ viewValue typedValue2
                                    ]
                                ]

                        _ ->
                            Element.none

                _ ->
                    Element.none

        ArithmeticOperatorBranch arithmeticOperator arithmeticOperatorTrees ->
            case arithmeticOperator of
                _ ->
                    let
                        separator =
                            row
                                [ spacing 5
                                , width Element.fill
                                , centerX
                                ]
                                [ text (Maybe.withDefault "" (Dict.get (functionNameHelper arithmeticOperator) inlineBinaryOperators))
                                ]
                    in
                    arithmeticOperatorTrees
                        |> List.map
                            (view viewValue)
                        |> List.indexedMap
                            (\i b ->
                                if dropInPrecedence arithmeticOperatorTrees i 0 (currentPrecedence (functionName arithmeticOperator)) arithmeticOperator && riseInPrecedence arithmeticOperatorTrees i 0 (currentPrecedence (functionName arithmeticOperator)) arithmeticOperator && i < List.length arithmeticOperatorTrees - 1 then
                                    row
                                        [ padding 2
                                        , spacing 5
                                        , centerX
                                        ]
                                        [ text "(", b, text ")", separator ]

                                else if dropInPrecedence arithmeticOperatorTrees i 0 (currentPrecedence (functionName arithmeticOperator)) arithmeticOperator then
                                    row
                                        [ padding 2
                                        , spacing 5
                                        , centerX
                                        ]
                                        [ text "(", b, text ")" ]

                                else if riseInPrecedence arithmeticOperatorTrees i 0 (currentPrecedence (functionName arithmeticOperator)) arithmeticOperator && i < List.length arithmeticOperatorTrees - 1 then
                                    row
                                        [ padding 2
                                        , spacing 5
                                        , centerX
                                        ]
                                        [ b, separator ]

                                else if riseInPrecedence arithmeticOperatorTrees i 0 (currentPrecedence (functionName arithmeticOperator)) arithmeticOperator then
                                    row
                                        [ padding 5
                                        , spacing 5
                                        , centerX
                                        ]
                                        [ text "(", b, text ")" ]

                                else if i < List.length arithmeticOperatorTrees - 1 then
                                    row
                                        [ padding 5
                                        , spacing 5
                                        , centerX
                                        ]
                                        [ b, separator ]

                                else
                                    row
                                        [ padding 5
                                        , spacing 5
                                        , centerX
                                        ]
                                        [ b ]
                            )
                        |> Element.row [ spacing 5, width Element.fill, centerX ]


inlineBinaryOperators : Dict String String
inlineBinaryOperators =
    Dict.fromList
        [ ( "Basics.equal", "=" )
        , ( "Basics.lessThan", "<" )
        , ( "Basics.lessThanOrEqual", "<=" )
        , ( "Basics.greaterThan", ">" )
        , ( "Basics.greaterThanOrEqual", ">=" )
        , ( "Add", "+" )
        , ( "Subtract", "-" )
        , ( "Multiply", "*" )
        ]


dropInPrecedence : List ArithmeticOperatorTree -> Int -> Int -> Int -> ArithmeticOperator -> Bool
dropInPrecedence arithmeticOperatorTrees index currentPointer currentPrecedenceValue previousOperator =
    if currentPointer < index then
        dropInPrecedence (List.drop 1 arithmeticOperatorTrees) index (currentPointer + 1) currentPrecedenceValue previousOperator

    else
        case List.head arithmeticOperatorTrees of
            Just a ->
                case a of
                    ArithmeticOperatorBranch arithmeticOperator arithmeticOperatorTrees1 ->
                        case arithmeticOperator of
                            _ ->
                                if currentPrecedence (functionName arithmeticOperator) > currentPrecedence (functionName previousOperator) then
                                    True

                                else
                                    False

                    ArithmeticValueLeaf typedValue ->
                        dropInPrecedence (List.drop 1 arithmeticOperatorTrees) index (currentPointer + 1) currentPrecedenceValue previousOperator

                    ArithmeticDivisionBranch arithmeticOperatorTree arithmeticOperatorTree1 ->
                        if currentPrecedence "Basics.divide" < currentPrecedence (functionName previousOperator) then
                            True

                        else
                            False

            Nothing ->
                False


riseInPrecedence : List ArithmeticOperatorTree -> Int -> Int -> Int -> ArithmeticOperator -> Bool
riseInPrecedence arithmeticOperatorTrees index currentPointer currentPrecedenceValue previousOperator =
    if currentPointer < index then
        riseInPrecedence (List.drop 1 arithmeticOperatorTrees) index (currentPointer + 1) currentPrecedenceValue previousOperator

    else
        case List.head arithmeticOperatorTrees of
            Just a ->
                case a of
                    ArithmeticOperatorBranch arithmeticOperator arithmeticOperatorTrees1 ->
                        case arithmeticOperator of
                            _ ->
                                if currentPrecedence (functionName arithmeticOperator) > currentPrecedence (functionName previousOperator) then
                                    True

                                else
                                    False

                    ArithmeticValueLeaf typedValue ->
                        riseInPrecedence (List.drop 1 arithmeticOperatorTrees) index (currentPointer + 1) currentPrecedenceValue previousOperator

                    ArithmeticDivisionBranch arithmeticOperatorTree arithmeticOperatorTree1 ->
                        if currentPrecedence "Basics.divide" > currentPrecedence (functionName previousOperator) then
                            True

                        else
                            False

            Nothing ->
                False


functionName : ArithmeticOperator -> String
functionName ao =
    case ao of
        Add ->
            "Basics.add"

        Subtract ->
            "Basics.subtract"

        Multiply ->
            "Basics.multiply"


functionNameHelper : ArithmeticOperator -> String
functionNameHelper ao =
    case ao of
        Add ->
            "Add"

        Subtract ->
            "Subtract"

        Multiply ->
            "Multiply"


currentPrecedence : String -> Int
currentPrecedence operatorName =
    case operatorName of
        "Basics.add" ->
            1

        "Basics.subtract" ->
            1

        "Basics.multiply" ->
            2

        "Basics.divide" ->
            3

        _ ->
            0
