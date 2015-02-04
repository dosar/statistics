import shapeless.HList

val a = HList(1, "2", (3, 2))
a(1)
val s: Seq[Int] = List(1,2,3)