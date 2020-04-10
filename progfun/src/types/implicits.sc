case class Context[T](message: String)

def printContextAwared[T](x: T)(implicit ctx: Context[T]) = println(s"${ctx.message}: $x")

implicit val ctxInt = Context[Int]("This is Integer")
implicit val ctxStr = Context[String]("This is String")

printContextAwared(1)
printContextAwared("string")