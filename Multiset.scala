import scala.collection.immutable.{ListMap, Map}
import java.lang.IllegalArgumentException

class Multiset[A](val u: ListMap[A, Int])
    extends IntMap[A](u)
{
  def this() = this(new ListMap[A, Int]())

  def this(kv: (A,Int)) = this(ListMap[A, Int](kv))

  def addInt(k: A, v: Int): Multiset[A] =
    if (v > 0)
      new Multiset(underlying + (k -> v))
    else if (v < 0)
      throw new IllegalArgumentException("Adding entry with negative integer " +
                                         (k,v) + " to " + this)
    else if (isDefinedAt(k))
      // Setting to 0 requires erasing existing mapping
      new Multiset(underlying - k)
    else this // Not present, leave alone

  def numberOf(x: A): Int = this.getOrElse(x, 0)

  override def get(k: A) = Some(underlying.numberOf(k))

  override def +[B >: Int](kv: (A, B)) =
    kv match
      {
        case (key, v: Int) =>
          (if (v == 0)
             super.-(key)
           else super.+(kv))
        case _ => super.+(kv)
      }

  override def entryPlus(k: A, n: Int): Multiset[A] =
  {
    val adjustedN = max(n, -(numberOf(k)));
    super.entryPlus(k, n).asInstanceOf[Multiset[A]]
  }

  override def tail:Multiset[A] = super.tail.asInstanceOf[Multiset[A]]

  /** Returns the multiset union of this and other.
    */
  def setPlus(other: MultiSet[A]): MultiSet[A] =
    {
      def numEither(k: A): Int = numberOf(k) + other.numberOf(k)

      val keysThis: Set[A] = keySet;
      val keysOther: Set[A] = other.keySet;

      new MultiSet(new IntMap(keysThis.map(k => (k -> numEither(k)))
                              ++ keysOther.map(k => (k -> numEither(k)))))
    }

  def setMinus(other: MultiSet[A]): MultiSet[A] =
    {
      def numThis(k: A): Int = numberOf(k)

      def numOther(k: A): Int = other.numberOf(k)

      val keysThis: Set[A] = keySet;
      new MultiSet(new IntMap(keysThis.map(k => (k ->
                                                 max(0,
                                                     (numThis(k)
                                                      - numOther(k)))))))
    }



  override def toString(): String =
    "Multiset{" + this.map{ case (k, i: Int) =>
                              if (i > 0)
                                i.toString + "*" + k.toString 
                              else k.toString
                          }.
                    mkString(", ") + 
            "}"
}
// -- This really ought to be augmented with union, intersection, and
// such, if those ops prove useful.


object Multiset
{

  def fromMap[A](underlying: ListMap[A, Int]) =
    new Multiset(underlying.filter
                  (kv =>
                    if (kv._2 < 0)
                      throw new IllegalArgumentException
                                     ("Creating Multiset with negative number "
                                      + kv._2 + " of instances of " + kv._1)
                    else kv._2 > 0))
}
