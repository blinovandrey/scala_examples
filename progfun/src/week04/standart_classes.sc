abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat = new Succ(this)
  def + (that: Nat): Nat
  def - (that: Nat): Nat
}

object Zero extends Nat {
  override def isZero: Boolean = true

  override def predecessor: Nat = throw new Error("0.predecessor")

  override def + (that: Nat): Nat = that

  override def - (that: Nat): Nat = if (that.isZero) this else throw new Error("negative number")
}

class Succ(n: Nat) extends Nat {
  override def isZero: Boolean = false

  override def predecessor: Nat = n

  override def +(that: Nat): Nat = new Succ(n + that)

  override def -(that: Nat): Nat = if (that.isZero) this else n - that.predecessor
}

val x = "5"
x.isInstanceOf[Int]

trait Te {
  def show: String = "show"
}
case class Test(x1: Int, x2: Int) extends Te
case class Test2(x3: String) extends Te

def test(e: Te) = e match {
  case Test(_, _) => "test"
  case Test2(_) => "test2"
}

val te: Te = new Te {}
val test = Test2("1")

test(test)


trait Expr
case class Number(n: Int) extends Expr
case class Sum(e1: Expr, e2: Expr) extends Expr
case class Prod(e1: Expr, e2: Expr) extends Expr

def show(e: Expr): String = e match {
  case Number(x) => x.toString
  case Sum(l, r) => show(l) + " + " + show(r)
}


show(Sum(Number(1), Number(44)))

def id(x: AnyVal) = x

val chars: List[(Char, Int)] = List(('a', 2), ('b', 4))

chars.contains(('a', 1))

val c1 = ('a', 1) :: ('b', 2) :: Nil

chars.last

def init[T](xs: List[T]): List[T] = xs match {
  case List() => throw new Error("init of empty list")
  case List(x) => List()
  case y :: ys => y :: init(ys)
}

def concat[T](xs: List[T], ys: List[T]): List[T] = xs match {
  case List() => ys
  case z :: zs => z :: concat(zs, ys)
}

def reverse[T](xs: List[T]): List[T] = xs match {
  case List() => xs
  case y :: ys => reverse(ys) ++ List(y)
}

def removeAt[T](n: Int, xs: List[T]): List[T] = (xs take n) ::: (xs drop n+1)

val pair = ("answer", 42)

def msort(xs: List[Int]): List[Int] = {
  val n = xs.length / 2
  if (n == 0) xs
  else {
    def merge(xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match {
      case (Nil, ys) => ys
      case (xs, Nil) => xs
      case (x :: xs1, y :: ys1) =>
        if (x < y) x :: merge(xs1, ys)
        else y :: merge(xs, ys1)
    }
    val (fst, scd) = xs.splitAt(n)
    merge(msort(fst), msort(scd))
  }
}

def pmsort[T](xs: List[T])(lt: (T, T) => Boolean): List[T] = {
  val n = xs.length / 2
  if (n == 0) xs
  else {
    def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
      case (Nil, ys) => ys
      case (xs, Nil) => xs
      case (x :: xs1, y :: ys1) =>
        if (lt(x, y)) x :: merge(xs1, ys)
        else y :: merge(xs, ys1)
    }
    val (fst, scd) = xs.splitAt(n)
    merge(pmsort(fst)(lt), pmsort(scd)(lt))
  }
}


def omsort[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
  val n = xs.length / 2
  if (n == 0) xs
  else {
    def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
      case (Nil, ys) => ys
      case (xs, Nil) => xs
      case (x :: xs1, y :: ys1) =>
        if (ord.lt(x, y)) x :: merge(xs1, ys)
        else y :: merge(xs, ys1)
    }
    val (fst, scd) = xs.splitAt(n)
    merge(omsort(fst), omsort(scd))
  }
}


val nums = List(2, -4, 5, 7, 1)
msort(nums)

val fruits = List("apple", "pineapple", "orange", "banana")
pmsort(fruits)((x: String, y: String) => x.compareTo(y) < 0)

omsort(nums)
omsort(fruits)

def squareList1(xs: List[Int]): List[Int] =
  xs match {
    case Nil => xs
    case y :: ys => y * y :: squareList1(ys)
  }

def squareList2(xs: List[Int]): List[Int] =
  xs.map(x => x * x)

squareList1(nums)
squareList1(nums)


nums.filter(x => x > 0)
nums.filterNot(x => x > 0)
nums.partition(x => x > 0)

nums.takeWhile(x => x > 0)
nums.dropWhile(x => x > 0)
nums.span(x => x > 0)

val data = List('a', 'a', 'a', 'b', 'b', 'c', 'c', 'a')

def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case x :: xs1 =>
    val (first, rest) = xs.span(y => y == x)
    first :: pack(rest)
}

pack(data)


def encode[T](xs: List[T]): List[(T, Int)] = {
  pack(xs) map (ys => (ys.head, ys.length))
}

encode(data)


def sum(xs: List[Int]): Int = (0 :: xs) reduceLeft (_ + _)

sum(nums)

def sumfold(xs: List[Int]): Int = xs.foldLeft(0)(_ + _)

sum(nums ::: List(1,2,3))

def mapFun[T, U](xs: List[T], f: T => U): List[U] =
  (xs foldRight List[U]())((y: T, ys: List[U]) => f(y) :: ys)

mapFun[Int, Int](nums, x => x * x)

def lengthFun[T](xs: List[T]): Int =
  (xs foldRight 0)((_ :T, acc: Int) => acc + 1)

lengthFun[Int](nums)

