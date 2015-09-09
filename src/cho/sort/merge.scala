package cho.sort

/**
 * @author batangr00t@daum.net
 */
object merge {

  // ***************** merge_sort1 ******************
  
  // ***************** merge_sort2 ******************
  def merge2(xs: List[Int], ys: List[Int]): List[Int] = {
    @scala.annotation.tailrec
    def loop(xs: List[Int], ys: List[Int], zs: List[Int]): List[Int] = {
      (xs, ys) match {
        case (Nil, Nil)      => zs
        case (x :: xs1, Nil) => loop(xs1, Nil, zs:::List(x))
        case (Nil, y :: ys1) => loop(Nil, ys1, zs:::List(y))
        case (x :: xs1, y :: ys1) => {
          if (x < y) loop(xs1, ys, zs:::List(x))
          else loop(xs, ys1, zs:::List(y))
        }
      }
    }

    loop(xs, ys, Nil)
  }
  
  def mergeSort2(xs: List[Int]): List[Int] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
      val (fst, snd) = xs.splitAt(n)
      merge2(mergeSort2(fst), mergeSort2(snd))
    }
  }  

  // ***************** merge_sort3 ******************
  // more faster than mergeSort2
  def merge3(xs: List[Int], ys: List[Int]): List[Int] = {
    @scala.annotation.tailrec
    def loop(xs: List[Int], ys: List[Int], zs: List[Int]): List[Int] = {
      (xs, ys) match {
        case (Nil, Nil)      => zs
        case (x :: xs1, Nil) => loop(xs1, Nil, x::zs)
        case (Nil, y :: ys1) => loop(Nil, ys1, y::zs)
        case (x :: xs1, y :: ys1) => {
          if (x < y) loop(xs1, ys, x::zs)
          else loop(xs, ys1, y::zs)
        }
      }
    }

    loop(xs, ys, Nil).reverse
  }

  def mergeSort3(xs: List[Int]): List[Int] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
      val (fst, snd) = xs.splitAt(n)
      merge3(mergeSort3(fst), mergeSort3(snd))
    }
  }   
}