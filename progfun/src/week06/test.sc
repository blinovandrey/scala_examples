val w = "abac"

w.groupBy(_.toLower).toList.map(x => (x._1, x._2.length)).sortBy(_._1)

val list = List(('a', 2), ('b', 2))

type occ = List[(Char, Int)]

val x = List(('a', 1), ('d', 1), ('l', 1), ('r', 3))
val y = List(('r', 1), ('a', 1))



val xMap = x.toMap


def f(map: Map[Char, Int], item: (Char, Int)) = {
  val (k, v) = item
  if (map(k) > 1) map.updated(k, map(k) - 1) else map - k
}

y.foldLeft(x.toMap)(f)

val xs = List(2,3)

xs.map(x=> x * 4)
