module Test.Accessors.Record exposing (age, bar, email, foo, info, name, qux, stuff, things)

import Accessors exposing (Relation, makeOneToOne_)


bar : Relation b reachable wrap -> Relation { a | bar : b } reachable wrap
bar =
    makeOneToOne_ ".bar" .bar (\fn rec -> { rec | bar = fn rec.bar })


foo : Relation b reachable wrap -> Relation { a | foo : b } reachable wrap
foo =
    makeOneToOne_ ".foo" .foo (\fn rec -> { rec | foo = fn rec.foo })


qux : Relation b reachable wrap -> Relation { a | qux : b } reachable wrap
qux =
    makeOneToOne_ ".qux" .qux (\fn rec -> { rec | qux = fn rec.qux })


name : Relation b reachable wrap -> Relation { a | name : b } reachable wrap
name =
    makeOneToOne_ ".name" .name (\fn rec -> { rec | name = fn rec.name })


age : Relation b reachable wrap -> Relation { a | age : b } reachable wrap
age =
    makeOneToOne_ ".age" .age (\fn rec -> { rec | age = fn rec.age })


email : Relation b reachable wrap -> Relation { a | email : b } reachable wrap
email =
    makeOneToOne_ ".email" .email (\fn rec -> { rec | email = fn rec.email })


stuff : Relation b reachable wrap -> Relation { a | stuff : b } reachable wrap
stuff =
    makeOneToOne_ ".stuff" .stuff (\fn rec -> { rec | stuff = fn rec.stuff })


things : Relation b reachable wrap -> Relation { a | things : b } reachable wrap
things =
    makeOneToOne_ ".things" .things (\fn rec -> { rec | things = fn rec.things })


info : Relation b reachable wrap -> Relation { a | info : b } reachable wrap
info =
    makeOneToOne_ ".info" .info (\fn rec -> { rec | info = fn rec.info })
