package exercises

/*
 * The effects seen so far are all useful for
 * handling errors. Each effect has different
 * properties and fit different needs:
 * no error info, closed error type and open
 * error type. Apart from that error effects
 * embrace one of the following strategies:
 * - dynamic: don't expose the error type e.g. Try[A]
 * - static: expose the error type e.g. Either[E, A]
 */

class ErrorHandlingTests extends munit.FunSuite {

  /*
   * TODO: remove ignores and
   *       implements functions marked with `???`
   *
   * ADD YOUR CODE HERE INSIDE THE TEST OBJECT
   */

  test("should throw demo") {
    class DummyException extends RuntimeException("DUMMY")
    def test(): String = throw new DummyException

    intercept[DummyException] {
      test();
    }
    ()
  }

  test("Option - dynamic style".ignore) {
    def compute(value: Int): Option[Int] =
      if (value > 0) Some(value * 2)
      else None

    val value = compute(-10)

    // TODO: ingore(remove me, run test watch why fails and make it green")
    value.get;
  }

  test("Try - dynamic style".ignore) {
    import scala.util.{ Failure, Success, Try }

    def compute(value: Int): Try[Int] =
      if (value > 0) Success(value * 2)
      else Failure(new Exception("invalid number"))

    val value = compute(-10)

    // TODO: ingore(remove me, run test watch why fails and make it green")
    value.get;
  }

  test("Either - static style".ignore) {
    sealed trait ComputeError
    case class InvalidNumber(n: Int) extends ComputeError

    def compute(value: Int): Either[ComputeError, Int] =
      if (value > 0) Right(value * 2)
      else Left(InvalidNumber(value))

    val value = compute(-10)

    // TODO: ingore(remove me, run test watch why fails and make it green")
    // value.get;
  }

  test("Future - dynamic style".ignore) {
    import scala.concurrent._
    import scala.concurrent.duration._
    import scala.concurrent.ExecutionContext.Implicits.global

    def compute(value: Int): Future[Int] =
      if (value > 0) Future.successful(value * 2)
      else Future.failed(new Exception("invalid number"))

    val value = compute(-10)

    // TODO: ingore(remove me, run test watch why fails and make it green")
    Await.result(value, 2.seconds); ()
  }

  test("IO - dynamic style".ignore) {
    import cats._
    import cats.effect._

    def compute(value: Int): IO[Int] =
      if (value > 0) IO(value * 2)
      else IO(throw new Exception("invalid number"))

    val value = compute(-10)

    // TODO: ingore(remove me, run test watch why fails and make it green")
    value.unsafeRunSync();
  }

  test("convert from Either to Option".ignore) {
    sealed trait AppError
    case class BadParam() extends AppError

    // TODO: ingore(implement convert function")
    def convert[E, A](e: Either[E, A]): Option[A] =
      ???

    assertEquals(convert(Right("foo")), Some("foo"))
    assertEquals(convert(Left(BadParam)), None)
  }

  test("convert from Try to Either".ignore) {
    import scala.util.{ Failure, Success, Try }

    case class BadParamException() extends RuntimeException("bad param")

    // TODO: ingore(implement convert function")
    def convert[A](t: Try[A]): Either[Throwable, A] =
      ???

    assertEquals(convert(Success("foo")), Right("foo"))
    assertEquals(convert(Failure(BadParamException())), Left(BadParamException()))
  }
}
