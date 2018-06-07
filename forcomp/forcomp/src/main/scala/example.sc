def comb(s: String): Set[String] = {
  println("s: " + s)
  if(s.isEmpty) Set("")
  else {
//    (for {
//      i <- 1 to s.length
//      _= println("i: " + i)
//      t <- List(s.take(i))
//      _= println("t: " + t)
//      d <- comb(s.drop(i))
//      _= println("d: " + d)
//    } yield t :: d :: Nil).toSet.flatten
    val h = s.head
    (for {
      t <- comb(s.tail)
    } yield {
      Set(t, (h + t))
    }).flatten
  }
}

comb("a")
comb("ab")
comb("icy")

List("a") ++ List("")
"a" :: "" :: Nil
println(List(1,2,3).toString)


//    (for {
//      i <- 0 to s.length
//    } yield {
//      s.combinations(i).toSet
//    }).toSet.flatten

val m = Map((1, "a"))
m + ((2, "b"))

List(4) ++ List(List(1,2),List(3), List())

val l = List(List(1,2),List(3), List())
l.map(4 +: _)