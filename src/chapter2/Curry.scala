package chapter2

object Curry {

  def main(args: Array[String]): Unit = {
    val sum = (i1: Int, i2: Int) => i1 + i2
    val plus10 = curry(sum).apply(10)
    println(plus10(2)) //12
  }


  def curry[A, B, C](f: (A, B) => C): A => B => C = {
    a: A => b: B => f(a, b)
  }

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b) //f(a).apply(b)
  }

}
