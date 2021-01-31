module Morphir.Type.Infer.Codec exposing (..)

import Json.Decode as Decode
import Json.Encode as Encode
import Morphir.IR.FQName.Codec exposing (encodeFQName)
import Morphir.IR.Name.Codec exposing (decodeName, encodeName)
import Morphir.Type.Class.Codec exposing (encodeClass)
import Morphir.Type.Infer exposing (TypeError(..), UnificationError(..), ValueTypeError(..))
import Morphir.Type.MetaType.Codec exposing (encodeMetaType)
import Morphir.Type.MetaTypeMapping exposing (LookupError(..))


encodeValueTypeError : ValueTypeError -> Encode.Value
encodeValueTypeError (ValueTypeError valueName typeError) =
    Encode.list identity
        [ Encode.string "value_type_error"
        , encodeName valueName
        , encodeTypeError typeError
        ]


decodeValueTypeError : Decode.Decoder ValueTypeError
decodeValueTypeError =
    Decode.map2 ValueTypeError
        (Decode.index 1 decodeName)
        -- TODO: implement
        (Decode.succeed (TypeErrors []))


encodeTypeError : TypeError -> Encode.Value
encodeTypeError typeError =
    case typeError of
        TypeErrors typeErrors ->
            Encode.list identity
                [ Encode.string "type_errors"
                , Encode.list encodeTypeError typeErrors
                ]

        ClassConstraintViolation metaType class ->
            Encode.list identity
                [ Encode.string "class_constraint_violation"
                , encodeMetaType metaType
                , encodeClass class
                ]

        LookupError lookupError ->
            Encode.list identity
                [ Encode.string "lookup_error"
                , encodeLookupError lookupError
                ]

        UnknownError message ->
            Encode.list identity
                [ Encode.string "unknown_error"
                , Encode.string message
                ]

        CouldNotUnify unificationError metaType1 metaType2 ->
            Encode.list identity
                [ Encode.string "could_not_unify"
                , encodeUnificationError unificationError
                , encodeMetaType metaType1
                , encodeMetaType metaType2
                ]


encodeLookupError : LookupError -> Encode.Value
encodeLookupError lookupError =
    case lookupError of
        CouldNotFindConstructor fQName ->
            Encode.list identity
                [ Encode.string "could_not_find_constructor"
                , encodeFQName fQName
                ]

        CouldNotFindValue fQName ->
            Encode.list identity
                [ Encode.string "could_not_find_value"
                , encodeFQName fQName
                ]

        CouldNotFindAlias fQName ->
            Encode.list identity
                [ Encode.string "could_not_find_alias"
                , encodeFQName fQName
                ]

        ExpectedAlias fQName ->
            Encode.list identity
                [ Encode.string "expected_alias"
                , encodeFQName fQName
                ]


encodeUnificationError : UnificationError -> Encode.Value
encodeUnificationError unificationError =
    case unificationError of
        NoUnificationRule ->
            Encode.list identity
                [ Encode.string "no_unification_rule"
                ]

        TuplesOfDifferentSize ->
            Encode.list identity
                [ Encode.string "tuples_of_different_size"
                ]

        RefMismatch ->
            Encode.list identity
                [ Encode.string "ref_mismatch"
                ]

        FieldMismatch ->
            Encode.list identity
                [ Encode.string "field_mismatch"
                ]
