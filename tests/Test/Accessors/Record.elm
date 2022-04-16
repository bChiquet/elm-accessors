module Test.Accessors.Record exposing (age, bar, email, foo, info, name, qux, stuff, things)

import Accessors exposing (Relation, makeOneToOne)


bar : Relation b reachable wrap -> Relation { a | bar : b } reachable wrap
bar =
    makeOneToOne ".bar" .bar (\fn rec -> { rec | bar = fn rec.bar })


foo : Relation b reachable wrap -> Relation { a | foo : b } reachable wrap
foo =
    makeOneToOne ".foo" .foo (\fn rec -> { rec | foo = fn rec.foo })


qux : Relation b reachable wrap -> Relation { a | qux : b } reachable wrap
qux =
    makeOneToOne ".qux" .qux (\fn rec -> { rec | qux = fn rec.qux })


name : Relation b reachable wrap -> Relation { a | name : b } reachable wrap
name =
    makeOneToOne ".name" .name (\fn rec -> { rec | name = fn rec.name })


age : Relation b reachable wrap -> Relation { a | age : b } reachable wrap
age =
    makeOneToOne ".age" .age (\fn rec -> { rec | age = fn rec.age })


email : Relation b reachable wrap -> Relation { a | email : b } reachable wrap
email =
    makeOneToOne ".email" .email (\fn rec -> { rec | email = fn rec.email })


stuff : Relation b reachable wrap -> Relation { a | stuff : b } reachable wrap
stuff =
    makeOneToOne ".stuff" .stuff (\fn rec -> { rec | stuff = fn rec.stuff })


things : Relation b reachable wrap -> Relation { a | things : b } reachable wrap
things =
    makeOneToOne ".things" .things (\fn rec -> { rec | things = fn rec.things })


info : Relation b reachable wrap -> Relation { a | info : b } reachable wrap
info =
    makeOneToOne ".info" .info (\fn rec -> { rec | info = fn rec.info })
