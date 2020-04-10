type Set = Int => Boolean
def singletonSet(elem: Int): Set = (x: Int) => elem == x
def contains(s: Set, elem: Int): Boolean = s(elem)



val s = singletonSet(1)
contains(s, 2)

def union(s: Set, t: Set): Set = (x: Int) => contains(s, x) || contains(t, x)

val t = singletonSet(2)

val u = union(s, t)

contains(u, 3)

def filter(s: Set, p: Int => Boolean): Set = (x: Int) => contains(s, x) && contains(p, x)
def p = (x: Int) => x > 1

val f = filter(u,p)
contains(f, 2)


val bound = 1000

def pr(s: Set): String = {
  val xs = for (i <- -bound to bound if contains(s, i)) yield i
  xs.mkString("{", ",", "}")
}

/**
  * Prints the contents of a set on the console.
  */
def printSet(_s: Set) {
  println(pr(_s))
}

/**
  * Returns whether all bounded integers within `s` satisfy `p`.
  */
def forall(s: Set, p: Int => Boolean): Boolean = {
  def iter(a: Int): Boolean = {
    if (a > bound) true
    else if (contains(s,a) && !contains(filter(s, p), a)) false
    else iter(a + 1)
  }
  iter(-bound)
}

forall(x => -bound <= x && x < bound, x => x > -999)

def exists(s: Set, p: Int => Boolean): Boolean = {
  forall(s, p)
}

exists(x => -bound < x && x < bound, x => x > -999)

val ss = (x: Int) => -bound < x && x < bound

def map(s: Set, f: Int => Int): Set = {
  def iter(a: Int, acc: Set): Set = {
    if (a > bound) acc
    else if (contains(s, a)) {
      val union_acc = union(acc, singletonSet(f(a)))
      iter(a + 1, union_acc)}
    else iter(a + 1, acc)
  }
  iter(-bound, x => false)
}

val test = (x: Int) => x > 0
val testm = map(test, x => x - 50)
printSet(test)
printSet(testm)
