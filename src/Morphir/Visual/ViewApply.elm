module Morphir.Visual.ViewApply exposing (view)

import Dict exposing (Dict)
import Element exposing (Element, centerX, column, fill, moveRight, padding, paddingEach, rgb, row, spacing, text, width, wrappedRow)
import Element.Border as Border
import Element.Font exposing (center)
import Morphir.IR.FQName exposing (FQName(..))
import Morphir.IR.Name as Name
import Morphir.IR.Path as Path
import Morphir.IR.Type exposing (Type)
import Morphir.IR.Value as Value exposing (Value)
import Morphir.Visual.Common exposing (nameToText)



-- extract the type from functionValue
-- use helper function value.valueAttribute


view : (Value ta (Type ta) -> Element msg) -> Value ta (Type ta) -> List (Value ta (Type ta)) -> Element msg
view viewValue functionValue argValues =
    case ( functionValue, argValues ) of
        ( Value.Reference _ (FQName _ _ (("is" :: _) as localName)), [ argValue ] ) ->
            row
                [ width fill
                , spacing 10
                , centerX
                ]
                [ viewValue argValue
                , text (nameToText localName)
                ]

        -- possibly binary operator
        -- this view is specific to functions dealing with numbers
        ( Value.Reference _ (FQName [ [ "morphir" ], [ "s", "d", "k" ] ] moduleName localName), [ argValue1, argValue2 ] ) ->
            let
                functionName : String
                functionName =
                    String.join "."
                        [ moduleName |> Path.toString Name.toTitleCase "."
                        , localName |> Name.toCamelCase
                        ]
            in
            Maybe.map
                (\functionText ->
                    row
                        [ spacing 5
                        , centerX
                        , width fill
                        ]
                        [ row
                            [ spacing 6
                            , centerX
                            ]
                            [ column [] [ viewValue argValue1 ]
                            , column [] [ text functionText ]
                            , column [] [ viewValue argValue2 ]
                            ]
                        ]
                )
                (Dict.get functionName inlineBinaryOperators)
                |> Maybe.withDefault
                    (column
                        [ spacing 10, centerX, width fill ]
                        [ viewValue functionValue
                        , column
                            [ paddingEach { left = 10, right = 0, top = 0, bottom = 0 }
                            , spacing 10
                            , width fill
                            , centerX
                            ]
                            (argValues
                                |> List.map viewValue
                            )
                        ]
                    )

        _ ->
            column
                [ spacing 10, centerX, width fill ]
                [ viewValue functionValue
                , column
                    [ moveRight 10
                    , spacing 10
                    , width fill
                    , centerX
                    ]
                    (argValues
                        |> List.map viewValue
                    )
                ]


inlineBinaryOperators : Dict String String
inlineBinaryOperators =
    Dict.fromList
        [ ( "Basics.equal", "=" )
        , ( "Basics.lessThan", "<" )
        , ( "Basics.lessThanOrEqual", "<=" )
        , ( "Basics.greaterThan", ">" )
        , ( "Basics.greaterThanOrEqual", ">=" )
        , ( "Basics.add", "+" )
        , ( "Basics.subtract", "-" )
        , ( "Basics.multiply", "*" )
        , ( "Basics.divide", "/" )
        ]
