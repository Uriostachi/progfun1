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
  case Nil => Nil
  case (y: List[_]) :: ys => flatten(y) ::: flatten(ys)
  case z :: zs => z :: flatten(zs)
}

flatten(List(List(1, List(11, 25)), 2, List(3, List(5, 8))))

// Merge Sort

def msort(xs: List[Int]): List[Int] = {
  val n = xs.length/2
  if(n == 0) xs
  else {
    def merge(xs: List[Int], ys: List[Int]): List[Int] = xs match {
      case Nil => ys
      case x :: xs1 =>
        ys match {
          case Nil => xs
          case y :: ys1 => {
            if (x < y) x :: merge(xs1, ys)
            else y :: merge(xs, ys1)
          }
        }
    }

    val (fst, snd) = xs splitAt n
    merge(msort(fst), msort(snd))
  }
}

msort(List(5, 3, 6, 9, 7, 2))

def msortPair(xs: List[Int]): List[Int] = {
  val n = xs.length/2
  if(n == 0) xs
  else {
    def merge(xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match {
      case (Nil, _) => ys
      case (_, Nil) => xs
      case (x :: xs1, y :: ys1) => {
        if(x < y) x :: merge(xs1, ys)
        else y :: merge(xs, ys1)
      }
    }

    val (fst, snd) = xs splitAt n
    merge(msortPair(fst), msortPair(snd))
  }
}

msortPair(List(5, 3, 6, 9, -7, 2))


def msortGeneral[T](xs: List[T])(implicit lt: (T, T) => Boolean ): List[T] = {
  val n = xs.length/2
  if(n == 0) xs
  else {
    def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
      case (Nil, _) => ys
      case (_, Nil) => xs
      case (x :: xs1, y :: ys1) => {
        if(lt(x, y)) x :: merge(xs1, ys)
        else y :: merge(xs, ys1)
      }
    }

  val (fst, snd) = xs splitAt n
  merge(msortGeneral(fst), msortGeneral(snd))
  }
}

val alpha: List[Char] = List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z')
val l3 = List('a', 'e', 'z', 'h', 'r', 'j')
def alphaOrder(x: Char, y: Char): Boolean = if (alpha.indexOf(x) < alpha.indexOf(y)) true else false

msortGeneral(l3)(alphaOrder)

def squareList(xs: List[Int]): List[Int] = xs match {
  case Nil => Nil
  case y :: ys => y * y :: squareList(ys)
}

def mapSquareList(xs: List[Int]): List[Int] = xs.map(x => x * x)

squareList(l1)
mapSquareList(l2)

val duplicates = List("a", "a", "a", "b", "c", "c", "a")

def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case x :: xs1 =>
    val (first, rest) = xs span (y => y == x)
    first :: pack(rest)
}

pack(duplicates)

def encode[T](xs: List[T]): List[(T, Int)] = {
  pack(xs) map (ys => (ys.head, ys.length))
}

encode(duplicates)

def mapFun[T, U](xs: List[T], f: T => U): List[U] =
  (xs foldRight List[U]())((x, list) => f(x) :: list)

def twice(x: Int): Int = x * 2

mapFun(l1, twice)

def lengthFun[T](xs: List[T]): Int =
  (xs foldRight 0)((_, sum) => sum + 1)

lengthFun(l1)
