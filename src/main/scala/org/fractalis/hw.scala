object Hi {
  def main(args: Array[String]) = println("Hi!")

  def sort(xs: Array[Int]): Array[Int] = {
    if (xs.length < 1) xs
    else {
      val pivot = xs(xs.length/2)
      Array.concat(
        sort( xs filter (pivot >)),
        xs filter (pivot ==),
        sort( xs filter (pivot <)))
    }
  }

  def squareDouble(x: Double): Double = x*x

  def sumOfMultiples3And5(): Integer = {
    (1 to 999) filter { x => (x%3 == 0 || x%5 == 0) } reduce {_+_}
  }

  def sumOfEvenFibonacci(): Integer = {
    val Phi = 1.61803398
    val fibonacci_seq = 34
    def f = (x:Integer) => Math.pow(Phi,x) / (Phi+2)

    (1 to fibonacci_seq) filter { x => f(x)%2 == 0 } reduce {_+_}
  }

  def findMin(xs: List[Int] ): Int = {
    xs reduce { (x,y) => if (x<y) { x }  else { y } }
  }
}
