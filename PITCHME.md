## @color[GoldenRod](why) embrace 
## functional programming?

---
## before we jump to that...

---
## What should we @color[IndianRed](expect)
## from a fp workshop?

---
## It's a bit like 
## @color[GoldenRod](learning) to program @color[IndianRed](again)

---
## @color[IndianRed](do not judge)
## what you will see 
## @color[GoldenRod](with the eyes of)
## what you know today

---
## take this workashop as
## a @color[GoldenRod](place) and @color[GoldenRod](time)
## to @color[IndianRed](experiment) and @color[IndianRed](fail)

---
## @color[GoldenRod](why) embrace 
## functional programming?

---
## @color[GoldenRod](composition)

---
<img src="assets/drboolean.png" >

---
## How do we @color[GoldenRod](solve) problems? 

---
> We decompose bigger problems into smaller problems. <br />
> If the smaller problems are still too big, we decompose them further, and so on. <br />
> Finally, we write code that solves all the small problems.

---
> And then comes the essence of programming: <br />
> we compose those pieces of code to create solutions to larger problems.

---
> Decomposition wouldn’t make sense if we weren’t able to put the pieces back together. <br />
> - Bartosz Milewski

---
## In Complex System 
we dedicate a @color[IndianRed](significant) portion of code and effort to @color[GoldenRod](compose) pieces togheter

---
## @color[GoldenRod](who) is the number one @color[IndianRed](enemy) of composition?

---
## side-effects

---
### un@color[GoldenRod](composable)
```scala
val toS : Int => String = n => {
  appendAll("log.txt", "some content")
  n.toString
}
```

---
### un@color[GoldenRod](composable)
```scala
val list = collection.mutable.ListBuffer[Int]()

val toS : Int => String = n => {
  list += n
  if (list.size < 42) n.toString
  else "Yo!"
}
```

---
## Side-effects are a @color[IndianRed](complexity source)
- hide inputs and outputs
- destroy testability
- destroy composability

---
## Functional Programming
### is about @color[IndianRed](eliminating) or @color[GoldenRod](controlling) side-effects

---
## How?
see “function” as the @color[IndianRed](mathematical) one:
- @color[GoldenRod](Total): it must yield a value for every possible input
- @color[GoldenRod](Deterministic): it must yield the same value for the same input
- @color[GoldenRod](Pure): it’s only effect must be the computation of its return value

---
## We earn back
### all functions become @color[GoldenRod](referentially transparent)

---
### An @color[GoldenRod](expression can be replaced)
### with its corresponding value 
### @color[IndianRed](without changing) the program's behavior

---
### These two programs are @color[GoldenRod](equivalent)
```scala
val y = foo(x)
val z = y + y
```

```scala
val z = foo(x) + foo(x)
```

---
### with @color[IndianRed](referential transparency) functions are:
- easier to @color[GoldenRod](reason)
- easier to @color[GoldenRod](compose)
- easier to @color[GoldenRod](refactor)
- easier to @color[GoldenRod](test)

---
## @color[GoldenRod](pssss...)
mathematicians do @color[IndianRed](refactor) their "code" since long before us

`\[
x(y + z) = (xy) + (xz) 
\]`