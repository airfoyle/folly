object Test extends App
{
  import PosIntMap._

  type foo[A] = Map[A, Int]

  def shortClassName(x: AnyRef) = x.getClass.getSimpleName

  val m1:PosIntMap[String] = PosIntMap("a" -> 1, "b" -> 10)
  println(s"m1 = $m1 [Class: ${shortClassName(m1)}]")
  val m2 = m1 + ("a" -> 2)
  println(s"Adding (a -> 2) --> $m2")
  val m3 = m2 + ("b" -> 0)
  println(s"Then adding (b -> 0) --> $m3")
  val m4 = m2.map{ case(k,v) => (k, v+1) }
  println(s"If instead add 1 to each entry --> $m4 [Class: ${shortClassName(m4)}]")
  val m4b = m2.map{ case(k,v) => (k, v+1) }(canBuildFrom[String])
  println(s"Alternative version: ${m4b}")
  val m5 = m2.map{ case(k,v) => (k, "foo" + v) }
  println(s"If instead add foo to each entry --> $m5 [Class: ${shortClassName(m5)}]")
  val m6 = PosIntMap("aa" -> 11, "cc" -> 12)
  val m7 = m2 ++ m6
  println("\nm7 = " + m7)
  var i = 50
  var mx:PosIntMap[String] = m2.asInstanceOf[PosIntMap[String]]
  while (i < 60)
    {
      mx = (mx + (("foo" + i) -> (i+1))).asInstanceOf[PosIntMap[String]]
      println("mx = " + mx)
      i += 1
    }
  println("")
}



////  val a1 = Vector[Array[Int]](Array(1,2,4), Array(3,6,9))
////           // (10,9,8,7,6,5,4,3,2,1)
////  println("a1(1)(2) = " + a1(1)(2))
////  a1(1)(2) = -9
////  println("Now a1(1)(2) = " + a1(1)(2))

