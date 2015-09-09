package cho.sort

/**
 * @author batangr00t@daum.net
 */
object insertion {

  // ***************** insertion_sort1 ******************
  // x  : integer 
  // xs : sorted list
  def insert1(x: Int, xs: List[Int]): List[Int] = xs match {
    case Nil => List(x)
    case y :: ys1 => {
      if (x <= y) x :: xs
      else y :: insert1(x, ys1)
    }
  }

  def insertionSort1(xs: List[Int]): List[Int] = xs match {
    case Nil     => Nil
    case y :: ys => insert1(y, insertionSort1(ys))
  }

  // ***************** insertion_sort2 ******************
  // x  : integer 
  // xs : sorted list
  // 속도는 insertionSort1보다 느림
  def insert2(x: Int, xs: List[Int]): List[Int] = {
    @scala.annotation.tailrec
    def loop(x: Int, xs: List[Int], left: List[Int]): List[Int] = xs match {
      case Nil => left ::: List(x)
      case y :: ys1 => {
        if (x <= y) left ::: x :: xs
        else loop(x, ys1, left ::: List(y))
      }
    }
    
    loop( x, xs, Nil)
  }

  def insertionSort2(xs: List[Int]): List[Int] = xs match {
    case Nil     => Nil
    case y :: ys => insert2(y, insertionSort2(ys))
  }
  
  // ***************** insertion_sort3 ******************
  // x  : integer 
  // xs : sorted list
  def insert3(x: Int, xs: List[Int]): List[Int] = xs match {
    case Nil => List(x)
    case ys => 
      val (left, right) = ys.partition(_ < x )
      left:::x::right            
  }

  def insertionSort3(xs: List[Int]): List[Int] = xs match {
    case Nil     => Nil
    case y :: ys => insert3(y, insertionSort3(ys))
  }
  
}