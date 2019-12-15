package chapter2

import scala.annotation.tailrec

object Fibonacci {

  def main(args: Array[String]): Unit = {
    println(fib(10))
  }


  def fib(n: Int): Int = {
    @tailrec
    def go(n: Int, a: Int, b: Int): Int ={
      if (n == 0) a
      else if (n == 1) b
      else go(n-1, b, a + b)
    }
    go(n, 0, 1)
  }
}