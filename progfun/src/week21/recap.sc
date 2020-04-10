val f: String => String = { case "ping" => "pong"}


val pf: PartialFunction[String, String] = { case "ping" => "pong"}
pf.isDefinedAt("1")
pf.isDefinedAt("ping")

val pfl: PartialFunction[List[Int], String] = {
  case Nil => "one"
  case x :: y :: rest => "two"
}

pfl(List(1,2,3))

List(1,2,3) flatMap (x => List("x: " + x))

List(1,2,3) map (x => List("x: " + x))

val c = 10

for (x <- List(1,2,3); y <- List(4,5,6)) yield {
  "x: " + x + ", " + "y: " + y + ", " + "c: " + c
}


case class Book(title: String, authors: List[String])

val books = Set(
  Book(title = "Structure and interpratation of computer programs",
    authors = List("Abelson, Halard", "Sussman, Jerald J." )),
  Book(title = "Introduction to Functional Programming",
    authors = List("Bird, Richard", "Walder, Phil")),
  Book(title = "Effective Java",
    authors = List("Bloch, Joshua")),
  Book(title = "Java Puzzlers",
    authors = List("Bloch, Joshua", "Gafter, Niel")),
  Book(title = "Programming in scala",
    authors = List("Bloch, Joshua", "Odersky, Martin", "Spoon, Lex", "Venners, Bill"))
)

for (b <- books; a <- b.authors if a startsWith "Bird")
  yield b.title

books flatMap (b => b.authors withFilter (a => a startsWith "Bird") map (_ => b.title))

for (b <- books if b.title.indexOf("Program") >= 0)
  yield b.title


for {
  b1 <- books
  b2 <- books
  if b1.title < b2.title
  a1 <- b1.authors
  a2 <- b2.authors
  if a1 == a2
}
  yield a1