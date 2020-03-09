abstract class IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(other: IntSet): IntSet
}


class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  def contains(x: Int): Boolean =
    if(x < elem) left contains x
    else if(x > elem) right contains x
    else true

  def incl(x: Int): IntSet =
    if(x < elem) new NonEmpty(x, left incl x, right)
    else if(x > elem) new NonEmpty(elem, left, right incl x)
    else this

  override def toString = "{" + left + elem + right + "}"

  def union(other: IntSet): IntSet =
    ((left union right) union other) incl elem
}

object Empty extends IntSet {
  def contains(x: Int): Boolean = false
  def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)
  override def toString = "."

  def union(other: IntSet): IntSet = other
}

val t1 = new NonEmpty(3, Empty, Empty)
val t2 = t1 incl 4

object scratch {

  def error(msg: String) = throw new Error(msg)
}

if(true) 1 else false


def nth[T](x: Int, list: List[T]) = {
  list(x)
}

nth[Int](3, List(1, 2, 3, 4))

nth[String](1, List("hello", "hola", "halo", "cool"))