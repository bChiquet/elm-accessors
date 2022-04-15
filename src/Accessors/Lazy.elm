module Accessors.Lazy exposing
  (get, set, over)

{-| Lazy versions of set, over.

These actions check that the old and the new version are different before writing. 
They are useful when used together with `Html.lazy`, because it uses reference
equality for complex structures. Therefore, using lazy `set` and `over` will
not prevent `Html.lazy` from doing its work.

get is also reexported for convenience.

@docs get, set, over
-}

import Accessors.Internal exposing (Relation(..), id)
import Accessors as Strict

{-| The get function takes:
* An accessor,
* A datastructure with type `super`
and returns the value accessed by that combinator.
```
get (foo << bar) myRecord 
```
-}
get : (Relation sub sub sub -> Relation super sub wrap) -> super -> wrap
get = Strict.get


{-|The set function takes:
* An accessor, 
* A value of the type `sub`,
* A datastructure with type `super`
and it returns the data structure, with the accessible field changed to the set value.
The structure is changed only if the new field is different from the old one.
```
set (foo << bar) "Hi!" myRecord
```
-}
set : (Relation sub sub sub -> Relation super sub wrap) -> sub -> super -> super
set accessor value s =
  let newSuper = Strict.set accessor value s
  in if get accessor newSuper /= get accessor s
     then newSuper
     else s

{-|The over function takes:
* An accessor, 
* A function `(sub -> sub)`,
* A datastructure with type `super`
and it returns the data structure, with the accessible field changed by applying
the function to the existing value.
The structure is changed only if the new field is different from the old one.
```
over (foo << qux) ((+) 1) myRecord
```
-}
over : (Relation sub sub sub -> Relation super sub wrap)
    -> (sub -> sub)
    -> super
    -> super
over accessor change s = 
  let newSuper = Strict.over accessor change s
  in if get accessor newSuper /= get accessor s
     then newSuper
     else s
