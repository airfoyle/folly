object EnumTest extends App
{
  object WeekDay extends Enumeration {
    type WeekDay = Value
    val Mon, Tue, Wed, Thu, Fri, Sat, Sun = Value
  }
  import WeekDay._

  println("Day 2 = " + WeekDay(2))
////  println("Day 2 = " + withName("Mon"))

}
