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


module Morphir.IR.Documented exposing (..)

{-| Tools to assign documentation to nodes in the IR.
-}


{-| Type that represents a documented value.
-}
type alias Documented a =
    { doc : String
    , value : a
    }


map : (a -> b) -> Documented a -> Documented b
map f d =
    Documented d.doc (f d.value)
