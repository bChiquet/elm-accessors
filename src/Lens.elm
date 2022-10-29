module Lens exposing (..)

import Accessors exposing (Lens, makeOneToOne)


bar : Lens { record | bar : attribute } transformed attribute built
bar =
    makeOneToOne ".bar" .bar (\fn rec -> { rec | bar = fn rec.bar })


foo : Lens { record | foo : attribute } transformed attribute built
foo =
    makeOneToOne ".foo" .foo (\fn rec -> { rec | foo = fn rec.foo })


qux : Lens { record | qux : attribute } transformed attribute built
qux =
    makeOneToOne ".qux" .qux (\fn rec -> { rec | qux = fn rec.qux })


name : Lens { record | name : attribute } transformed attribute built
name =
    makeOneToOne ".name" .name (\fn rec -> { rec | name = fn rec.name })


age : Lens { record | age : attribute } transformed attribute built
age =
    makeOneToOne ".age" .age (\fn rec -> { rec | age = fn rec.age })


email : Lens { record | email : attribute } transformed attribute built
email =
    makeOneToOne ".email" .email (\fn rec -> { rec | email = fn rec.email })


stuff : Lens { record | stuff : attribute } transformed attribute built
stuff =
    makeOneToOne ".stuff" .stuff (\fn rec -> { rec | stuff = fn rec.stuff })


things : Lens { record | things : attribute } transformed attribute built
things =
    makeOneToOne ".things" .things (\fn rec -> { rec | things = fn rec.things })


info : Lens { record | info : attribute } transformed attribute built
info =
    makeOneToOne ".info" .info (\fn rec -> { rec | info = fn rec.info })
