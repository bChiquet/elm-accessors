module Test.Accessors.Record exposing (age, bar, email, foo, info, name, qux, stuff, things)

import Accessors exposing (Lens, Relation, makeOneToOne_)


bar : Lens { record | bar : attribute } transformed attribute built
bar =
    makeOneToOne_ ".bar" .bar (\fn rec -> { rec | bar = fn rec.bar })


foo : Lens { record | foo : attribute } transformed attribute built
foo =
    makeOneToOne_ ".foo" .foo (\fn rec -> { rec | foo = fn rec.foo })


qux : Lens { record | qux : attribute } transformed attribute built
qux =
    makeOneToOne_ ".qux" .qux (\fn rec -> { rec | qux = fn rec.qux })


name : Lens { record | name : attribute } transformed attribute built
name =
    makeOneToOne_ ".name" .name (\fn rec -> { rec | name = fn rec.name })


age : Lens { record | age : attribute } transformed attribute built
age =
    makeOneToOne_ ".age" .age (\fn rec -> { rec | age = fn rec.age })


email : Lens { record | email : attribute } transformed attribute built
email =
    makeOneToOne_ ".email" .email (\fn rec -> { rec | email = fn rec.email })


stuff : Lens { record | stuff : attribute } transformed attribute built
stuff =
    makeOneToOne_ ".stuff" .stuff (\fn rec -> { rec | stuff = fn rec.stuff })


things : Lens { record | things : attribute } transformed attribute built
things =
    makeOneToOne_ ".things" .things (\fn rec -> { rec | things = fn rec.things })


info : Lens { record | info : attribute } transformed attribute built
info =
    makeOneToOne_ ".info" .info (\fn rec -> { rec | info = fn rec.info })
