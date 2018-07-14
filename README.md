The Accessors library
=====================

This library provides a way to manipulate nested datastructures without handling
the unpacking, repacking and conditional checks that may occur along the way.

# Using accessor combinators

Using combinators works in two parts:
first you compose a series of combinators in order to describe which data you
want to access.

```elm
let myAccessor = (recordFoo << onEach << recordBar)
```

Then you use an access function to determine which kind of operation you want to
do.

```elm
myData = {foo = [ {bar = 3}
                , {bar = 2}
                , {bar = 0}
                ]
         }

getter   = get myAccessor myData          -- returns [3, 2, 0]
setter   = set myAccessor 2 myData        -- returns {foo = [{bar = 3}, {bar = 2}, {bar = 0}] }
transorm = over myAccessor (*2) myData    -- returns {foo = [{bar = 6}, {bar = 4}, {bar = 0}] }
```

# Writing your own Accessor combinator

See the example of record accessor in src/Accessors.elm

# Contribute

build

```elm make```

run tests 

```elm test```

If you write new accessor combinators that rely on elm-lang datastructures, I'll be
happy to review and merge. Please include tests for your combinators.
