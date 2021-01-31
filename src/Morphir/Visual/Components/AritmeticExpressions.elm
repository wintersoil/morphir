module Morphir.Visual.Components.AritmeticExpressions exposing (..)

import Morphir.IR.FQName exposing (FQName(..))
import Morphir.IR.Name as Name exposing (Name)
import Morphir.IR.Path as Path exposing (Path)
import Morphir.IR.Value as Value exposing (TypedValue)


type ArithmeticOperatorTree
    = ArithmeticOperatorBranch ArithmeticOperator (List ArithmeticOperatorTree)
    | ArithmeticValueLeaf TypedValue
    | ArithmeticDivisionBranch ArithmeticOperatorTree ArithmeticOperatorTree


type ArithmeticOperator
    = Add
    | Subtract
    | Multiply


fromArithmeticTypedValue : TypedValue -> ArithmeticOperatorTree
fromArithmeticTypedValue typedValue =
    case typedValue of
        Value.Apply _ fun arg ->
            let
                ( function, args ) =
                    Value.uncurryApply fun arg
            in
            case ( function, args ) of
                ( Value.Reference _ (FQName _ moduleName localName), [ arg1, arg2 ] ) ->
                    let
                        operatorName : String
                        operatorName =
                            functionName moduleName localName
                    in
                    case operatorName of
                        "Basics.add" ->
                            ArithmeticOperatorBranch Add (helperArithmeticTreeBuilderRecursion typedValue operatorName)

                        "Basics.subtract" ->
                            ArithmeticOperatorBranch Subtract (helperArithmeticTreeBuilderRecursion typedValue operatorName)

                        "Basics.divide" ->
                            ArithmeticDivisionBranch (ArithmeticValueLeaf arg1) (ArithmeticValueLeaf arg2)

                        "Basics.multiply" ->
                            ArithmeticOperatorBranch Multiply (helperArithmeticTreeBuilderRecursion typedValue operatorName)

                        _ ->
                            ArithmeticValueLeaf typedValue

                _ ->
                    ArithmeticValueLeaf typedValue

        _ ->
            ArithmeticValueLeaf typedValue


helperArithmeticTreeBuilderRecursion : TypedValue -> String -> List ArithmeticOperatorTree
helperArithmeticTreeBuilderRecursion value operatorName =
    case value of
        Value.Apply _ fun arg ->
            let
                ( function, args ) =
                    Value.uncurryApply fun arg
            in
            case ( function, args ) of
                ( Value.Reference _ (FQName _ moduleName localName), [ arg1, arg2 ] ) ->
                    if (functionName moduleName localName) == operatorName then
                        helperArithmeticTreeBuilderRecursion arg1 operatorName ++ helperArithmeticTreeBuilderRecursion arg2 operatorName

                    else
                        [ fromArithmeticTypedValue value ]

                _ ->
                    [ ArithmeticValueLeaf value ]

        _ ->
            [ ArithmeticValueLeaf value ]


functionName : Path -> Name -> String
functionName moduleName localName =
    String.join "."
        [ moduleName |> Path.toString Name.toTitleCase "."
        , localName |> Name.toCamelCase
        ]


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
