module Morphir.Value.InterpreterTests exposing (..)

import Dict
import Expect
import Morphir.IR.FQName exposing (fqn)
import Morphir.IR.Literal exposing (Literal(..))
import Morphir.IR.SDK as SDK
import Morphir.IR.Value as Value
import Morphir.Value.Interpreter exposing (Reference(..), evaluate)
import Test exposing (Test, describe, test)


evaluateValueTests : Test
evaluateValueTests =
    let
        refs =
            SDK.nativeFunctions
                |> Dict.map
                    (\_ fun ->
                        NativeReference fun
                    )

        check desc input expectedOutput =
            test desc
                (\_ ->
                    evaluate refs input
                        |> Expect.equal
                            (Ok expectedOutput)
                )
    in
    describe "evaluateValue"
        [ check "True = True"
            (Value.Literal () (BoolLiteral True))
            (Value.Literal () (BoolLiteral True))
        , check "not True == False"
            (Value.Apply ()
                (Value.Reference () (fqn "Morphir.SDK" "Basics" "not"))
                (Value.Literal () (BoolLiteral True))
            )
            (Value.Literal () (BoolLiteral False))
        , check "True && False == False"
            (Value.Apply ()
                (Value.Apply ()
                    (Value.Reference () (fqn "Morphir.SDK" "Basics" "and"))
                    (Value.Literal () (BoolLiteral True))
                )
                (Value.Literal () (BoolLiteral False))
            )
            (Value.Literal () (BoolLiteral False))
        , check "False && True == False"
            (Value.Apply ()
                (Value.Apply ()
                    (Value.Reference () (fqn "Morphir.SDK" "Basics" "and"))
                    (Value.Literal () (BoolLiteral False))
                )
                (Value.Literal () (BoolLiteral True))
            )
            (Value.Literal () (BoolLiteral False))
        , check "ceiling 1.2 == 2"
                    (Value.Apply ()
                        (Value.Reference () (fqn "Morphir.SDK" "Basics" "ceiling"))
                        (Value.Literal () (FloatLiteral 1.2))
                    )
                    (Value.Literal () (IntLiteral 2))
        , check "ceiling 1.0 == 1"
                            (Value.Apply ()
                                (Value.Reference () (fqn "Morphir.SDK" "Basics" "ceiling"))
                                (Value.Literal () (FloatLiteral 1.0))
                            )
                            (Value.Literal () (IntLiteral 1))
        , check "ceiling 1.5 == 2"
                            (Value.Apply ()
                                (Value.Reference () (fqn "Morphir.SDK" "Basics" "ceiling"))
                                (Value.Literal () (FloatLiteral 1.5))
                            )
                            (Value.Literal () (IntLiteral 2))
        , check "ceiling 1.8 == 2"
                            (Value.Apply ()
                                (Value.Reference () (fqn "Morphir.SDK" "Basics" "ceiling"))
                                (Value.Literal () (FloatLiteral 1.8))
                            )
                            (Value.Literal () (IntLiteral 2))
        , check "ceiling -1.2 == -1"
                            (Value.Apply ()
                                (Value.Reference () (fqn "Morphir.SDK" "Basics" "ceiling"))
                                (Value.Literal () (FloatLiteral -1.2))
                            )
                            (Value.Literal () (IntLiteral -1))
        , check "ceiling -1.5 == -1"
                            (Value.Apply ()
                                (Value.Reference () (fqn "Morphir.SDK" "Basics" "ceiling"))
                                (Value.Literal () (FloatLiteral -1.5))
                            )
                            (Value.Literal () (IntLiteral -1))
        , check "ceiling -1.8 == -1"
                            (Value.Apply ()
                                (Value.Reference () (fqn "Morphir.SDK" "Basics" "ceiling"))
                                (Value.Literal () (FloatLiteral -1.8))
                            )
                            (Value.Literal () (IntLiteral -1))
        , check "floor 1.2 == 1"
                    (Value.Apply ()
                        (Value.Reference () (fqn "Morphir.SDK" "Basics" "floor"))
                        (Value.Literal () (FloatLiteral 1.2))
                    )
                    (Value.Literal () (IntLiteral 1))
        , check "floor 1.0 == 1"
                            (Value.Apply ()
                                (Value.Reference () (fqn "Morphir.SDK" "Basics" "floor"))
                                (Value.Literal () (FloatLiteral 1.0))
                            )
                            (Value.Literal () (IntLiteral 1))
        , check "floor 1.5 == 1"
                            (Value.Apply ()
                                (Value.Reference () (fqn "Morphir.SDK" "Basics" "floor"))
                                (Value.Literal () (FloatLiteral 1.5))
                            )
                            (Value.Literal () (IntLiteral 1))
        , check "floor 1.8 == 1"
                            (Value.Apply ()
                                (Value.Reference () (fqn "Morphir.SDK" "Basics" "floor"))
                                (Value.Literal () (FloatLiteral 1.8))
                            )
                            (Value.Literal () (IntLiteral 1))
        , check "floor -1.2 == -2"
                            (Value.Apply ()
                                (Value.Reference () (fqn "Morphir.SDK" "Basics" "floor"))
                                (Value.Literal () (FloatLiteral -1.2))
                            )
                            (Value.Literal () (IntLiteral -2))
        , check "floor -1.5 == -2"
                            (Value.Apply ()
                                (Value.Reference () (fqn "Morphir.SDK" "Basics" "floor"))
                                (Value.Literal () (FloatLiteral -1.5))
                            )
                            (Value.Literal () (IntLiteral -2))
        , check "floor -1.8 == -2"
                    (Value.Apply ()
                        (Value.Reference () (fqn "Morphir.SDK" "Basics" "floor"))
                        (Value.Literal () (FloatLiteral -1.8))
                    )
                    (Value.Literal () (IntLiteral -2))
        , check "round 1.0 == 1"
                    (Value.Apply ()
                        (Value.Reference () (fqn "Morphir.SDK" "Basics" "round"))
                        (Value.Literal () (FloatLiteral 1.0))
                    )
                    (Value.Literal () (IntLiteral 1))
        , check "round 1.2 == 1"
                            (Value.Apply ()
                                (Value.Reference () (fqn "Morphir.SDK" "Basics" "round"))
                                (Value.Literal () (FloatLiteral 1.2))
                            )
                            (Value.Literal () (IntLiteral 1))
        , check "round 1.5 == 2"
                            (Value.Apply ()
                                (Value.Reference () (fqn "Morphir.SDK" "Basics" "round"))
                                (Value.Literal () (FloatLiteral 1.5))
                            )
                            (Value.Literal () (IntLiteral 2))
        , check "round 1.8 == 2"
                            (Value.Apply ()
                                (Value.Reference () (fqn "Morphir.SDK" "Basics" "round"))
                                (Value.Literal () (FloatLiteral 1.8))
                            )
                            (Value.Literal () (IntLiteral 2))
        , check "round -1.2 == -1"
                            (Value.Apply ()
                                (Value.Reference () (fqn "Morphir.SDK" "Basics" "round"))
                                (Value.Literal () (FloatLiteral -1.2))
                            )
                            (Value.Literal () (IntLiteral -1))
        , check "round -1.5 == -1"
                            (Value.Apply ()
                                (Value.Reference () (fqn "Morphir.SDK" "Basics" "round"))
                                (Value.Literal () (FloatLiteral -1.5))
                            )
                            (Value.Literal () (IntLiteral -1))
        , check "round -1.8 == -2"
                    (Value.Apply ()
                        (Value.Reference () (fqn "Morphir.SDK" "Basics" "round"))
                        (Value.Literal () (FloatLiteral -1.8))
                    )
                    (Value.Literal () (IntLiteral -2))
        , check "truncate 1.0 == 1"
                    (Value.Apply ()
                        (Value.Reference () (fqn "Morphir.SDK" "Basics" "truncate"))
                        (Value.Literal () (FloatLiteral 1.0))
                    )
                    (Value.Literal () (IntLiteral 1))
        , check "truncate 1.2 == 1"
                            (Value.Apply ()
                                (Value.Reference () (fqn "Morphir.SDK" "Basics" "truncate"))
                                (Value.Literal () (FloatLiteral 1.2))
                            )
                            (Value.Literal () (IntLiteral 1))
        , check "truncate 1.5 == 1"
                            (Value.Apply ()
                                (Value.Reference () (fqn "Morphir.SDK" "Basics" "truncate"))
                                (Value.Literal () (FloatLiteral 1.5))
                            )
                            (Value.Literal () (IntLiteral 1))
        , check "truncate 1.8 == 1"
                            (Value.Apply ()
                                (Value.Reference () (fqn "Morphir.SDK" "Basics" "truncate"))
                                (Value.Literal () (FloatLiteral 1.8))
                            )
                            (Value.Literal () (IntLiteral 1))
        , check "truncate -1.2 == -1"
                            (Value.Apply ()
                                (Value.Reference () (fqn "Morphir.SDK" "Basics" "truncate"))
                                (Value.Literal () (FloatLiteral -1.2))
                            )
                            (Value.Literal () (IntLiteral -1))
        , check "truncate -1.5 == -1"
                            (Value.Apply ()
                                (Value.Reference () (fqn "Morphir.SDK" "Basics" "truncate"))
                                (Value.Literal () (FloatLiteral -1.5))
                            )
                            (Value.Literal () (IntLiteral -1))
        , check "truncate -1.8 == -1"
                            (Value.Apply ()
                                (Value.Reference () (fqn "Morphir.SDK" "Basics" "truncate"))
                                (Value.Literal () (FloatLiteral -1.8))
                            )
                            (Value.Literal () (IntLiteral -1))
        , check "power 3 ^ 3 == 27"
                            (Value.Apply ()
                                (Value.Apply ()
                                    (Value.Reference () (fqn "Morphir.SDK" "Basics" "power"))
                                    (Value.Literal () (IntLiteral 3))
                                )
                                (Value.Literal () (IntLiteral 3))
                            )
                            (Value.Literal () (IntLiteral 27))
        , check "truncate 2.3 ^ 5.4 == 48.36215533063266"
                            (Value.Apply ()
                                (Value.Apply ()
                                    (Value.Reference () (fqn "Morphir.SDK" "Basics" "power"))
                                    (Value.Literal () (FloatLiteral 2.3))
                                )
                                (Value.Literal () (FloatLiteral 5.4))
                            )
                            (Value.Literal () (FloatLiteral 48.36215533063266))
        ]
