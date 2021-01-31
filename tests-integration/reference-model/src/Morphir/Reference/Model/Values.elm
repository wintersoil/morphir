module Morphir.Reference.Model.Values exposing (..)

import Morphir.Reference.Model.Types as Types exposing (Custom(..), FooBarBazRecord)
import String exposing (fromInt)


lawOfGravitation : Float -> Float -> Float
lawOfGravitation f1 f2 =
    f1 + f2


lawOfGravitation2 : Float -> Float -> Float -> Float
lawOfGravitation2 f1 f2 f3 =
    f1 + f2 / f3


lawOfGravitation3 : Float -> Float -> Float -> Float
lawOfGravitation3 f1 f2 f3 =
    f1 + f2 / f3 + 10 * 3


lawOfGravitation4 : Float -> Float -> Float -> Float -> Float
lawOfGravitation4 f1 f2 f3 f4 =
    f1 + f2 / f3 * f4


lawOfGravitation5 : Float -> Float -> Float -> Float -> Float -> Float
lawOfGravitation5 f1 f2 f3 f4 f5 =
    f1 + (f2 * f5) / f3 * f4


lawOfGravitation6 : Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float
lawOfGravitation6 f1 f2 f3 f4 f5 f6 f7 =
    f1 * f2 * f5 + f6 / f3 * f4 + f7


lawOfGravitation7 : Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float
lawOfGravitation7 f1 f2 f3 f4 f5 f6 f7 =
    f1 * f2 + f5 + f6 / f3 + f4 + f7


lawOfGravitation8 : Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float
lawOfGravitation8 f1 f2 f3 f4 f5 f6 f7 =
    f1 + f2 * f5 + f6 + f3 / f4 + f7


lawOfGravitation9 : Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float
lawOfGravitation9 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 =
    f1 + f2 * f5 + f6 + f3 / f4 + f7 + f8 + f9 + f10


lawOfGravitation10 : Float -> Float -> Float -> Float -> Float
lawOfGravitation10 g m1 m2 r =
    (g * m1 * m2) / (r * r)


lawOfGravitation11 : Float -> Float -> Float -> Float -> Float
lawOfGravitation11 g m1 m2 r =
    g * m1 * m2 * r / r


lawOfGravitation12 : Float -> Float -> Float -> Float -> Float
lawOfGravitation12 g m1 m2 r =
    g * m1 * m2 / r * r


basicLiteralBool : Bool
basicLiteralBool =
    True


basicLiteralChar : Char
basicLiteralChar =
    'Z'


basicLiteralString : String
basicLiteralString =
    "foo bar"


basicLiteralInt : Int
basicLiteralInt =
    42


basicLiteralFloat : Float
basicLiteralFloat =
    3.14


basicConstructor1 : Custom
basicConstructor1 =
    CustomNoArg


basicConstructor2 : Custom
basicConstructor2 =
    CustomOneArg False


basicConstructor3 : Custom
basicConstructor3 =
    CustomTwoArg "Baz" 12345


basicRecordConstructor : FooBarBazRecord
basicRecordConstructor =
    FooBarBazRecord "foo" True 42


basicTuple2 : ( Int, String )
basicTuple2 =
    ( 13, "Tuple Two" )


basicTuple3 : ( Bool, Int, Bool )
basicTuple3 =
    ( True, 14, False )


basicListEmpty : List Int
basicListEmpty =
    []


basicListOne : List String
basicListOne =
    [ "single element" ]


basicListMany : List Char
basicListMany =
    [ 'a', 'b', 'c', 'd' ]


basicRecordEmpty : {}
basicRecordEmpty =
    {}


basicRecordOne : { foo : String }
basicRecordOne =
    { foo = "bar"
    }


basicRecordMany : { foo : String, bar : Bool, baz : Int }
basicRecordMany =
    { foo = "bar"
    , bar = False
    , baz = 15
    }


basicField : { foo : String } -> String
basicField rec =
    rec.foo


basicFieldFunction : { foo : String } -> String
basicFieldFunction =
    .foo


basicLetDefinition : Int
basicLetDefinition =
    let
        a : Int
        a =
            1

        b : Int
        b =
            a

        d : Int -> Int
        d i =
            i
    in
    d b


basicLetRecursion : Int
basicLetRecursion =
    let
        a : Int -> Int
        a i =
            b (i - 1)

        b : Int -> Int
        b i =
            if i < 0 then
                0

            else
                a i
    in
    a 10


basicDestructure : Int
basicDestructure =
    let
        ( a, b ) =
            ( 1, 2 )
    in
    b


basicIfThenElse : Int -> Int -> String
basicIfThenElse a b =
    if a < b then
        "Less"

    else
        "Greater or equal"


basicIfThenElse2 : String -> String
basicIfThenElse2 ball =
    let
        a : Int
        a =
            1
    in
    if ball /= "ball" then
        ball

    else
        "Bye"


basicIfThenElse3 : Bool -> String
basicIfThenElse3 boolValueVariable =
    if boolValueVariable then
        "bool"

    else
        "Bye"


booleanExpressions : Bool -> Bool -> Bool -> Bool
booleanExpressions ball cat dog =
    ball && cat || dog


booleanExpressions2 : Bool -> Bool -> Bool -> Bool -> Bool
booleanExpressions2 a b c d =
    d && booleanExpressions a b c


booleanExpressions3 : Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool
booleanExpressions3 a b c d e f =
    a || b && booleanExpressions2 c d e f


booleanExpressions4 : Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool
booleanExpressions4 a b c d e f g h =
    a || b && booleanExpressions3 c d e f g h


type alias FruitAction =
    { fruitType : String
    , amount : Float
    }


noHarvest : List FruitAction
noHarvest =
    []


basicIfThenElse4 : Float -> Float -> Float -> Float -> Float -> List FruitAction
basicIfThenElse4 greenApple redApple amberApple greenPear redPear =
    if greenApple == 0 || redApple == 0 || greenApple == redApple then
        noHarvest

    else
        let
            redGreenApple : Float
            redGreenApple =
                redApple + greenApple
        in
        if redGreenApple > 0 then
            let
                appleAmount : Float
                appleAmount =
                    10
            in
            [ { fruitType = "apple", amount = appleAmount }
            , { fruitType = "pear", amount = 5 }
            ]

        else
            let
                appleAmount : Float
                appleAmount =
                    15

                bananaAmount : Float
                bananaAmount =
                    158.3
            in
            [ { fruitType = "apple", amount = appleAmount }
            , { fruitType = "pear", amount = 5 }
            , FruitAction "banana" bananaAmount
            ]


basicPatternMatchWildcard : String -> Int
basicPatternMatchWildcard s =
    case s of
        _ ->
            1


basicUpdateRecord : FooBarBazRecord -> FooBarBazRecord
basicUpdateRecord rec =
    { rec
        | baz = rec.baz + 1
    }


basicUnit : ()
basicUnit =
    ()


sdkBasicsValues : List Bool
sdkBasicsValues =
    [ 4 + 3 == 7
    , 4 - 3 == 1
    , 4 * 2.5 == 10
    , 10 / 4 == 2.5
    , 11 // 5 == 2
    , 2 ^ 3 == 8
    , toFloat 2 == 2.0
    , round 2.5 == 3
    , floor 2.78 == 2
    , ceiling 2.13 == 3
    , truncate 2.56 == 2
    , 1 == 1
    , 1 /= 2
    , (1 < 2) == True
    , (1 > 2) == False
    , (1 <= 2) == True
    , (1 >= 2) == False
    , max 1 2 == 2
    , min 1 2 == 1
    ]


sdkMaybeValues : List Bool
sdkMaybeValues =
    [ Maybe.andThen (always Nothing) Nothing == Nothing
    , Maybe.map fromInt (Just 42) == Just "42"
    , Maybe.map2 (\a b -> [ a, b ]) (Just 1) (Just 2) == Just [ 1.5, 2 ]
    , Maybe.map3 (\a b c -> [ a, b, c ]) (Just 1) (Just 2) (Just 3) == Just [ 1, 2.3, 3 ]
    , Maybe.map4 (\a b c d -> [ a, b, c, d ]) (Just 1) (Just 2) (Just 3) (Just 4) == Just [ 1, 2, 3.4, 4 ]
    , Maybe.map5 (\a b c d e -> [ a, b, c, d, e ]) (Just 1) (Just 2) (Just 3) (Just 4) (Just 5) == Just [ 1, 2, 3, 4.5, 5 ]
    , Maybe.withDefault 13 Nothing == 13.2
    ]


fieldFunctionAsArg : List FooBarBazRecord -> List String
fieldFunctionAsArg list =
    list
        |> List.filter (\x -> x.bar)
        |> List.map .foo


functionToMethod1 : Int
functionToMethod1 =
    Types.customToInt CustomNoArg


functionToMethod2 : Int
functionToMethod2 =
    Types.customToInt2 False (CustomOneArg True)


functionToMethod3 : String
functionToMethod3 =
    Types.fooBarBazToString (FooBarBazRecord "foo" False 43)
