import week3._

object test {
  def nth[T](x: Int, list: List[T]): T = {
    if(x == 0) list.head
    else nth(x - 1, list.tail)
  }
}

val  y = new Cons(1, new Cons(2, new Cons(3, new Nil)))

test.nth[Int](3, List(1, 2, 3, 4))

test.nth[String](1, List("hello", "hola", "halo", "cool"))

test.nth(2, y)