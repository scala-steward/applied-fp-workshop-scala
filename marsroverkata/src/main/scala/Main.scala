package marsroverkata

object Main extends App {

  import cats._, cats.data._, cats.implicits._, cats.effect._
  import scala.util._

  case class Planet(x: Int, y: Int)
  case class Rover(x: Int, y: Int)
  sealed trait Command
  case class Move() extends Command

  final case class AppError(errors: NonEmptyList[String])
      extends Exception("Error list: " + errors.toList.mkString(", "))

  val rp  = "5x4"
  val rr  = "1,2"
  val rcs = "RFFBL"

//   def execute(p: Planet, r: Rover, cs: List[Command]): Rover = Rover(5, 2)
  def execute(p: Planet, r: Rover, cs: List[Command]): Either[Rover, Rover] = Rover(5, 2).asRight[Rover]
//   def execute(p: Planet, r: Rover, cs: List[Command]): Either[Rover, Rover] = Rover(5, 2).asLeft[Rover]

  def render(r: Rover): String    = "normal: " + r
  def renderHit(r: Rover): String = "hit: " + r

  def parsePlanet(s: String): ValidatedNel[String, Planet] = Planet(5, 4).validNel
//   def parsePlanet(s: String): ValidatedNel[String, Planet] = "bad planet".invalidNel
  def parseRover(s: String): ValidatedNel[String, Rover] = Rover(1, 2).validNel
//   def parseRover(s: String): ValidatedNel[String, Rover]            = "bad rover".invalidNel
  def parseCommands(s: String): ValidatedNel[String, List[Command]] = List(Move()).validNel

  def readPlanet(): IO[String] = IO.pure(rp)
//   def readRover(): IO[String]  = IO(throw new RuntimeException("boooom!"))
  def readRover(): IO[String]    = IO.pure(rr)
  def readCommands(): IO[String] = IO.pure(rcs)

  val r3 = (readPlanet(), readRover(), readCommands()).tupled.map {
    case (p, r, cs) =>
      (parsePlanet(p), parseRover(r), parseCommands(cs))
        .mapN(execute)
        .toEither
        .map(_.fold(renderHit, render))
  }

  val r6 = r3.flatMap(e => IO.fromEither(e.leftMap(AppError)))
  val r7 = r6.attempt
  val r8 = r7.flatMap(handle)

  println(r8.unsafeRunSync())

  def handle(e: Either[Throwable, String]): IO[Unit] =
    e.fold(err, ok)

  def ok(r: String): IO[Unit] = IO {
    println("ok: " + r)
  }

  def err(t: Throwable): IO[Unit] = IO {
    println("fail: " + t.getMessage)
  }
}
