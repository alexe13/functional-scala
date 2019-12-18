package chapter3

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]


object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(list: List[Double]): Double = list match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case Cons(_, xs) => xs
  }

  def setHead[A](list: List[A], newHead: A): List[A] = list match {
    case Nil => Nil
    case Cons(_, xs) => Cons(newHead, xs)
  }

  def drop[A](list: List[A], n: Int): List[A] = list match {
    case Nil => Nil
    case Cons(x, xs) => if (n > 0) drop(xs, n - 1) else Cons(x, xs)
  }

  def dropWhile[A](list: List[A], predicate: A => Boolean): List[A] = list match {
    case Nil => Nil
    case Cons(x, xs) => if (predicate(x)) dropWhile(xs, predicate) else Cons(x, xs)
  }

  def init[A](list: List[A]): List[A] =
    list match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
    }

  def main(args: Array[String]): Unit = {
    val intList = List(3, -3, 5, -5, 7)
    println(List.sum(intList)) //7
    println(List.tail(intList)) // -3, 5, -5, 7
    println(List.setHead(intList, 66)) // 66, -3, 5, -5, 7
    println(List.drop(intList, 3)) // -5 , 7
    println(List.dropWhile(intList, (i : Int) => i < 0)) // 3, 5, 7
    println(List.init(intList)) //3, -3, 5, -5
  }
}
