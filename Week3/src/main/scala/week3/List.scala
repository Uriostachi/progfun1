package week3

trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
}

class Cons[T](val head: T, val tail: List[t]) extends List[t] {
  def isEmpty = false
}

class Nil[T] extends List {
  def isEmpty: Boolean = true
  def head: Nothing = throw NoSuchElementExeption("Nil.head")
  def tail: Nothing = throw NoSuchElementExeption("Nil.head")
}