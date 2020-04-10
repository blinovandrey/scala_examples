val nums = Vector(1,2,3,-88)
val people = Vector("Bob", "James", "Peter")
val x = 0

println(x +: nums)
println(nums :+ x)

val xs = Array(1,2,3,44)
xs.map(_ * 2)

val s = "Hello World"
s.filter(_.isUpper)

val r = 1 to 10 by 3
val ru = 1 until 10 by 3
r.map(println)
ru.map(println)

r.exists(_ >= 10)
r.forall(_ >= 10)

(r zip ru).last

val fmap = r.flatMap(x => List(1, x * x))

def scalarProduct(xs: Vector[Double], ys: Vector[Double]): Double = {
  (xs zip ys).map(xy => xy._1 * xy._2).sum
}

//def scalarProduct(xs: Vector[Double], ys: Vector[Double]): Double = {
//  (xs zip ys).map{case (x, y) => x * y}.sum
//}

def scalarProductFor(xs: Vector[Double], ys: Vector[Double]): Double = {
  (for ((x, y) <- xs zip ys) yield x * y).sum
}

def isPrime(n: Int) = (2 until n).forall(x => n % x != 0)

val n = 7

(1 until n) flatMap (i => (1 until i) map (j=> (i,j))) filter (pair => isPrime(pair._1 + pair._2))

for {
  i <- 1 until n
  j <- 1 until i
  if isPrime(i + j)
}
  yield (i, j)

val romanNumerals = Map('I' -> 1, 'V' -> 5, 'X' -> 10)
val capitalOfCountry = Map("US" -> "Washington", "Switzerland" -> "Bern")

romanNumerals('I')
romanNumerals.get('Y')
capitalOfCountry.get("US")

val fruit = List("apple", "pear", "orange", "pineapple")
fruit.sortWith(_.length > _.length)
fruit.sorted
fruit.sortBy(_.tail)

fruit.groupBy(_.head)('p')