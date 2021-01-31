{-
   Copyright 2020 Morgan Stanley

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
-}


module Morphir.IR.SDK.Maybe exposing (..)

import Dict
import Morphir.IR.Documented exposing (Documented)
import Morphir.IR.Module as Module exposing (ModuleName)
import Morphir.IR.Name as Name exposing (Name)
import Morphir.IR.Path as Path exposing (Path)
import Morphir.IR.SDK.Common exposing (tFun, tVar, toFQName, vSpec)
import Morphir.IR.Type as Type exposing (Specification(..), Type(..))
import Morphir.IR.Value as Value


moduleName : ModuleName
moduleName =
    Path.fromString "Maybe"


moduleSpec : Module.Specification ()
moduleSpec =
    { types =
        Dict.fromList
            [ ( Name.fromString "Maybe"
              , CustomTypeSpecification [ Name.fromString "a" ]
                    [ Type.Constructor (Name.fromString "Just") [ ( [ "value" ], Type.Variable () (Name.fromString "a") ) ]
                    , Type.Constructor (Name.fromString "Nothing") []
                    ]
                    |> Documented "Type that represents an optional value."
              )
            ]
    , values =
        Dict.fromList
            [ vSpec "andThen"
                [ ( "f", tFun [ tVar "a" ] (maybeType () (tVar "b")) )
                , ( "maybe", maybeType () (tVar "a") )
                ]
                (maybeType () (tVar "b"))
            , vSpec "map"
                [ ( "f", tFun [ tVar "a" ] (tVar "b") )
                , ( "maybe", maybeType () (tVar "a") )
                ]
                (maybeType () (tVar "b"))
            , vSpec "map2"
                [ ( "f", tFun [ tVar "a", tVar "b" ] (tVar "r") )
                , ( "maybe1", maybeType () (tVar "a") )
                , ( "maybe2", maybeType () (tVar "b") )
                ]
                (maybeType () (tVar "r"))
            , vSpec "map3"
                [ ( "f", tFun [ tVar "a", tVar "b", tVar "c" ] (tVar "r") )
                , ( "maybe1", maybeType () (tVar "a") )
                , ( "maybe2", maybeType () (tVar "b") )
                , ( "maybe3", maybeType () (tVar "c") )
                ]
                (maybeType () (tVar "r"))
            , vSpec "map4"
                [ ( "f", tFun [ tVar "a", tVar "b", tVar "c", tVar "d" ] (tVar "r") )
                , ( "maybe1", maybeType () (tVar "a") )
                , ( "maybe2", maybeType () (tVar "b") )
                , ( "maybe3", maybeType () (tVar "c") )
                , ( "maybe4", maybeType () (tVar "d") )
                ]
                (maybeType () (tVar "r"))
            , vSpec "map5"
                [ ( "f", tFun [ tVar "a", tVar "b", tVar "c", tVar "d", tVar "e" ] (tVar "r") )
                , ( "maybe1", maybeType () (tVar "a") )
                , ( "maybe2", maybeType () (tVar "b") )
                , ( "maybe3", maybeType () (tVar "c") )
                , ( "maybe4", maybeType () (tVar "d") )
                , ( "maybe5", maybeType () (tVar "e") )
                ]
                (maybeType () (tVar "r"))
            , vSpec "withDefault"
                [ ( "default", tVar "a" )
                , ( "maybe", maybeType () (tVar "a") )
                ]
                (tVar "a")
            ]
    }


maybeType : a -> Type a -> Type a
maybeType attributes itemType =
    Reference attributes (toFQName moduleName "Maybe") [ itemType ]
