val l1: List[Int] = List(0, 1, 2, 3, 4)

val l2: List[Int] = List(5, 6, 7, 8, 9)

def last[T](xs: List[T]): T = xs match {
  case List() => throw new Error("last of empty list")
  case List(x) => x
  case y :: ys => last(ys)
}

last(l1)
last(l2)

// Omega n

def init[T](xs: List[T]): List[T]  = xs match {
  case List() => throw  new Error("Init of empty List")
  case List(x) => List()
  case y :: ys => y :: init(ys)
}

init(l1)
init(l2)

def concat[T](xs: List[T], ys: List[T]): List[T] = xs match {
  case List() => ys
  case z :: zs => z :: concat(zs, ys)
}

concat(l1, l2)
l1 ::: l2

def reverse[T](xs: List[T]): List[T] = xs match {
  case List() => xs
  case y :: ys => reverse(ys) ++ List(y)
}

reverse(l1)
reverse(l2)

def removeAt[T](xs: List[T], n: Int): List[T] = (xs take n) ::: (xs drop n + 1)

removeAt(l1, 2)

def flatten(xs: List[Any]): List[Any] = xs match {
  case ele: List[Any] => flatten(xs.tail) ::: ele
  case item: Any => item :: flatten(xs.tail)
}

flatten(List(List(1, 1), 2, List(3, List(5, 8))))

// Merge Sort

def msort(xs: List[Int]): List[Int] = {
  val n = xs.length/2
  if(n == 0) xs
  else {
    def merge(xs: List[Int], ys: List[Int]) = ???
    val (fst, snd) = xs splitAt n
    merge(msort(fst), msort(snd))
  }
}