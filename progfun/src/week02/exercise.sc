
object exercise {
  def factorial(n: Int): Long = {
    def loop(acc: Long, n: Int): Long = {
      if (n == 0) acc
      else loop(acc * n, n - 1)
    }

    loop(1, n)
  }

  //  factorial(18)

  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  //  pascal(1,3)

  def balance(chars: List[Char]): Boolean = {

    def loop(chars: List[Char], open: Int): Boolean = {
      if (chars.isEmpty) open == 0
      else if (chars.head == '(') loop(chars.tail, open + 1)
      else if (chars.head == ')') open > 0 && loop(chars.tail, open - 1)
      else loop(chars.tail, open)
    }

    loop(chars, 0)

  }

//  balance(":)".toList)

  def countChange(money: Int, coins: List[Int]): Int = {
      if (money < 0 || coins.isEmpty ) 0
      else if (money == 0 ) 1
      else countChange(money, coins.tail) + countChange(money - coins.head, coins)
  }

//  countChange(100, List(1,2,3,4,5))
}

def anon = (x: Int) => x * x * x

def sumF(f: Int => Int, a: Int, b: Int): Int = {
  if (a > b) 0
  else f(a) + sumF(f, a + 1, b)
}

def sumTail(f: Int => Int, a: Int, b: Int): Int = {
  def loop(a:Int, acc:Int): Int ={
    if (a > b) acc
    else loop(a + 1, f(a) + acc)
  }
  loop(a, 0)
}
sumTail(anon, 1, 5)
sumTail(x => x * x, 10, 20)
sumF(anon, 1, 5)

sumF(x => x * x, 10, 20)

1 + 8 + 27 + 64 + 125

def sum(f: Int => Int)(a: Int)(b:Int): Int = {
  if (a > b) 0 else f(a) + sum(f)(a + 1)(b)
}

sum(x=>x * x)(10)(20)


def product(f: Int => Int)(a: Int)(b: Int): Int ={
  if (a > b) 1 else f(a) * product(f)(a + 1)(b)
}

product(x => x * x)(3)(4)

product(x => x)(1)(10)

def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b:Int): Int = {
  if (a > b) zero
  else combine(f(a), mapReduce(f, combine, zero)(a + 1, b))
}

mapReduce(x => x * x, (x, y) => x * y, 1)(3,4)


