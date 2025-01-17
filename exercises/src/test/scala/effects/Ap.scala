package exercises

class ApTests extends munit.FunSuite {

  /*
   * TODO: Follow the instruction in the ignores
   *
   * ADD YOUR CODE HERE INSIDE THE TEST OBJECT
   */

  import cats.implicits._

  case class Item(name: String, qty: Int)

  def checkName(value: String): Option[String] =
    if (!value.isEmpty) Some(value)
    else None

  def checkQty(qty: String): Option[Int] =
    if (qty.matches("^[0-9]+$")) Some(qty.toInt)
    else None

  def createItem(name: String, qty: String): Option[Item] =
    (checkName(name), checkQty(qty)).mapN(Item.apply)

  test("valid creation".ignore) {
    val item = createItem("foo", "100")
    // TODO: ingore(write the assert")
  }

  test("invalid creation (name)".ignore) {
    val item = createItem("", "100")
    // TODO: ingore(write the assert")
  }

  test("invalid creation (qty)".ignore) {
    val item = createItem("foo", "asd")
    // TODO: ingore(write the assert")
  }

  test("invalid creation (both)".ignore) {
    val item = createItem("", "asd")
    // TODO: ingore(write the assert")
  }
}
