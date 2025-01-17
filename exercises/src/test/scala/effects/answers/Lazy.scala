package exercises.answers

class LazyTests extends munit.FunSuite {

  case class Lazy[A](value: () => A) {
    def map[B](f: A => B): Lazy[B] =
      Lazy.pure(() => f(value()))

    def flatMap[B](f: A => Lazy[B]): Lazy[B] =
      Lazy.pure(() => f(value()).value())
  }

  object Lazy {
    def pure[A](a: () => A): Lazy[A] = Lazy(a)
  }

  def expensiveComputation() =
    6 + 4

  def increment(x: Int): Int =
    x + 1

  def reversedString(x: Int): Lazy[String] =
    Lazy.pure(() => x.toString.reverse)

  test("lift a value into a container") {
    val c = Lazy
      .pure(expensiveComputation _)

    assertEquals(c.value(), 10)
  }

  test("chain not container-aware functions") {
    val c = Lazy
      .pure(expensiveComputation _)
      .map(increment)

    assertEquals(c.value(), 11)
  }

  test("chain container-aware functions") {
    val c = Lazy
      .pure(expensiveComputation _)
      .flatMap(reversedString)

    assertEquals(c.value(), "01")
  }
}
