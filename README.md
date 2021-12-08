The Accessors library
=====================

This library provides a way to describe relations between a container and its
content, and use that description to manipulate arbitrary data more easily.

# Build your relations 

There are two kinds of relations between a container and its content: 1:1
relations (e.g. a record and its field) and 1:n relations (e.g. a `List` can
contain 0-n elements, a `Maybe` can contain 0-1 elements).

For 1:1 relations, the `makeOneToOne` function will let you build an accessor
by describing how to get the sub-element from the super-element, and how to map
a function over it. For instance, with a record:

```elm
recordFoo =
  makeOneToOne
    .foo
    (\change record -> {record | foo = change record.foo})

recordBar =
  makeOneToOne
    .bar
    (\change record -> {record | bar = change record.bar})
```

1:n relations are more complex in terms of abstraction, but they are usually
very easy to implement:

```elm
onEach = 
  makeOneToN
    List.map
    List.map

try = 
  makeOneToN
    Maybe.map
    Maybe.map
```

# Combine your relations

Accessors can be composed easily to describe relations:

```elm
myData = { foo = [ {bar = 3}
                 , {bar = 2}
                 , {bar = 0}
                 ]
         }

myAccessor = recordFoo << onEach << recordBar
```

# Manipulate your data easily

Then you use an action function to determine which kind of operation you want to
do on your data using the accessor

```elm
getter   = get  myAccessor myData
  -- returns [3, 2, 0]

setter   = set  myAccessor 2 myData
  -- returns {foo = [{bar = 2}, {bar = 2}, {bar = 2}]}

transform = over myAccessor (\n -> n*2) myData
  -- returns {foo = [{bar = 6}, {bar = 4}, {bar = 0}]}
```

# Type-safe and reusable

Applying an accessor on non-matching data structures will yield nice
compile-time errors: 

```elm
fail = (recordFoo << recordFoo) myData

--The 2nd argument to `get` is not what I expect:
--
--293| fail = get (recordFoo << recordFoo) myData
--                                         ^^^^^^
--This `myData` value is a:
--
--    { foo : List { bar : number } }
--
--But `get` needs the 2nd argument to be:
--
--    { foo : { a | foo : c } }
```

Any accessor you make can be composed with any other accessor to match your new
data structures: 

```elm
myOtherData = {bar = Just [1, 3, 2]}

halfWay = try << onEach
myOtherAccessor = recordBar << halfWay

getter = get  myOtherAccessor myOtherData
  -- returns Just [1, 3, 2]
```
# Play with it in Ellie

[Ellie default code with accessors](https://ellie-app.com/4wHNCxgft87a1). 

# Contribute

build

```elm make```

run tests

`elm-test`

or 

`elm-test-rs`

If you write new accessor combinators that rely on common library datas, I'll be
happy to review and merge. Please include tests for your combinators.
