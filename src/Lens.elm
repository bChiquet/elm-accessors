module Lens exposing (..)

import Base exposing (Lens, makeOneToOne)



-- bar : Lens { a | bar : b } b { a | bar : b }
-- bar : Base.Optic b attrView b -> Base.Optic { a | bar : b } attrView { c | bar : b }


bar =
    makeOneToOne ".bar" .bar (\fn rec -> { rec | bar = fn rec.bar })



-- foo : Lens { a | foo : b } b { a | foo : b }
-- foo : Base.Optic b attrView b -> Base.Optic { a | foo : b } attrView { c | foo : b }


foo =
    makeOneToOne ".foo" .foo (\fn rec -> { rec | foo = fn rec.foo })



-- qux : Lens { a | qux : b } b { a | qux : b }
-- qux : Base.Optic b attrView b -> Base.Optic { a | qux : b } attrView { c | qux : b }


qux =
    makeOneToOne ".qux" .qux (\fn rec -> { rec | qux = fn rec.qux })


{-| name : Lens { a | name : b } b { a | name : b }
-}



-- -- name : Base.Optic b attrView b -> Base.Optic { a | name : b } attrView { c | name : b }
-- name : Base.Optic b attrView b -> Base.Optic { a | name : b } attrView { c | name : b }


name =
    makeOneToOne ".name" .name (\fn rec -> { rec | name = fn rec.name })



-- age : Lens { a | age : b } b { a | age : b }
-- age : Base.Optic b attrView b -> Base.Optic { a | age : b } attrView { c | age : b }


age =
    makeOneToOne ".age" .age (\fn rec -> { rec | age = fn rec.age })



-- email : Lens { a | email : b } b { a | email : b }
-- email : Base.Optic b attrView b -> Base.Optic { a | email : b } attrView { c | email : b }


email =
    makeOneToOne ".email" .email (\fn rec -> { rec | email = fn rec.email })



-- stuff : Lens { a | stuff : b } b { a | stuff : b }
-- stuff : Base.Optic b attrView b -> Base.Optic { a | stuff : b } attrView { c | stuff : b }


stuff =
    makeOneToOne ".stuff" .stuff (\fn rec -> { rec | stuff = fn rec.stuff })



-- things : Lens { a | things : b } b { a | things : b }
-- things : Base.Optic b attrView b -> Base.Optic { a | things : b } attrView { c | things : b }


things =
    makeOneToOne ".things" .things (\fn rec -> { rec | things = fn rec.things })



-- info : Lens { a | info : b } b { a | info : b }
-- info : Base.Optic b attrView b -> Base.Optic { a | info : b } attrView { c | info : b }


info =
    makeOneToOne ".info" .info (\fn rec -> { rec | info = fn rec.info })
