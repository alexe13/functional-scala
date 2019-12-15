package chapter2

object IsSorted {

  def main(args: Array[String]): Unit = {
    val array = Array(-1,2,3,4,5)
    println(isSorted(array, (first: Int, second: Int) => first > second))
  }


  def isSorted[A](arr: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @scala.annotation.tailrec
    def loop(n: Int, curr: A, prevElem: A) : Boolean = {
      if (n >= arr.length) true
      else if (!ordered(curr, prevElem)) false
      else loop(n+1, arr(n), arr(n-1))
    }
    loop(1, arr(1), arr(0))
  }
}
