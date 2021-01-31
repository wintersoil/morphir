module Morphir.Visual.ViewBoolOperatorTree exposing (..)

import Element exposing (Attribute, Element, centerX, centerY, fill, height, none, padding, spacing, width)
import Element.Border
import Element.Font as Font
import Morphir.IR.Value exposing (TypedValue)
import Morphir.Visual.BoolOperatorTree exposing (BoolOperator(..), BoolOperatorTree(..))


view : (TypedValue -> Element msg) -> BoolOperatorTree -> Element msg
view viewValue boolOperatorTree =
    viewTreeNode viewValue Vertical boolOperatorTree


viewTreeNode : (TypedValue -> Element msg) -> LayoutDirection -> BoolOperatorTree -> Element msg
viewTreeNode viewValue direction boolOperatorTree =
    case boolOperatorTree of
        BoolOperatorBranch operator values ->
            let
                separator : Element msg
                separator =
                    case direction of
                        Horizontal ->
                            let
                                verticalLine : Element msg
                                verticalLine =
                                    Element.row [ centerX, height fill ]
                                        [ Element.el
                                            [ Element.Border.widthEach
                                                { top = 0
                                                , left = 1
                                                , right = 0
                                                , bottom = 0
                                                }
                                            , height fill
                                            ]
                                            none
                                        , Element.el
                                            [ height fill
                                            ]
                                            none
                                        ]
                            in
                            Element.column [ centerY, height fill ] [ verticalLine, Element.el [ Font.size 12, padding 5, Font.bold ] (Element.text (operator |> operatorToString)), verticalLine ]

                        Vertical ->
                            let
                                horizontalLine : Element msg
                                horizontalLine =
                                    Element.column [ width fill ]
                                        [ Element.el
                                            [ Element.Border.widthEach
                                                { top = 0
                                                , left = 0
                                                , right = 0
                                                , bottom = 1
                                                }
                                            , width fill
                                            ]
                                            none
                                        , Element.el
                                            [ width fill
                                            ]
                                            none
                                        ]
                            in
                            Element.row [ centerX, width fill ] [ horizontalLine, Element.el [ Font.size 12, padding 5, Font.bold ] (Element.text (operator |> operatorToString)), horizontalLine ]

                layout : List (Element msg) -> Element msg
                layout elems =
                    case direction of
                        Horizontal ->
                            Element.row [ centerX ] elems

                        Vertical ->
                            Element.column [ centerY ] elems
            in
            values
                |> List.map
                    (viewTreeNode viewValue
                        (flipLayoutDirection direction)
                    )
                |> List.map
                    (Element.el
                        [ padding 8
                        , spacing 8
                        , centerX
                        ]
                    )
                |> List.intersperse separator
                |> layout

        BoolValueLeaf value ->
            viewValue value


type LayoutDirection
    = Horizontal
    | Vertical


flipLayoutDirection : LayoutDirection -> LayoutDirection
flipLayoutDirection direction =
    case direction of
        Horizontal ->
            Vertical

        Vertical ->
            Horizontal


operatorToString : BoolOperator -> String
operatorToString operator =
    case operator of
        Or ->
            "OR"

        And ->
            "AND"
