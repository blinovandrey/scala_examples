import week03.{Rational, Hello, List, Nil, Cons}


object scratch {
  new Rational(1,2)
  def error(msg: String) = throw new Error(msg)
//  error("hello")

  val x = null
  val y: String = x

  if (true) 1 else false

  def singleton[T](elem: T) = new Cons[T](elem, new Nil[T])

  singleton[Int](1)

  def nth[T](n: Int, xs: List[T]): T = {
    if (xs.isEmpty) throw new IndexOutOfBoundsException
    else if (n == 0) xs.head
    else nth(n - 1, xs.tail)
  }

  val list = new Cons(1, new Cons(2, new Cons(3, new Nil)))

  nth(2, list)
  nth(-1, list)

}

