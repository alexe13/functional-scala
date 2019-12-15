package chapter2

object Compose {

  def main(args: Array[String]): Unit = {
    val plus10 = (i: Int) => i + 10
    val double = (i: Int) => i * 2
    println(compose(plus10, double).apply(2)) //14
  }

  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    a => f(g(a))
  }

}
