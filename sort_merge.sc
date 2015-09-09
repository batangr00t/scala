
/**
 * @author batangr00t@daum.net
 */
 
object sort_merge {

/* 이것도 가능하나 20배 느림
  def merge(xs: List[Int], ys: List[Int]): List[Int] = {
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
  */

  def merge(xs: List[Int], ys: List[Int]): List[Int] = {
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
  }                                               //> merge: (xs: List[Int], ys: List[Int])List[Int]

  def msort(xs: List[Int]): List[Int] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
      val (fst, snd) = xs.splitAt(n)
      merge(msort(fst), msort(snd))
    }
  }                                               //> msort: (xs: List[Int])List[Int]

  val list = List.fill(100)(scala.util.Random.nextInt(100))
                                                  //> list  : List[Int] = List(88, 73, 45, 78, 77, 58, 47, 97, 83, 32, 82, 16, 47
                                                  //| , 83, 40, 93, 50, 7, 71, 93, 91, 27, 93, 70, 57, 32, 74, 53, 58, 40, 62, 36
                                                  //| , 30, 70, 9, 2, 71, 26, 48, 79, 73, 58, 85, 3, 58, 21, 47, 96, 99, 89, 35, 
                                                  //| 39, 43, 60, 31, 77, 10, 99, 9, 39, 21, 2, 59, 36, 21, 41, 3, 39, 76, 5, 30,
                                                  //|  43, 41, 48, 88, 32, 41, 41, 91, 33, 88, 31, 39, 21, 63, 21, 38, 92, 90, 76
                                                  //| , 24, 21, 66, 34, 7, 68, 92, 18, 58, 33)
  cho.util.time(msort(list))                      //> 13.846072 msec
                                                  //| List(2, 2, 3, 3, 5, 7, 7, 9, 9, 10, 16, 18, 21, 21, 21, 21, 21, 21, 24, 26,
                                                  //|  27, 30, 30, 31, 31, 32, 32, 32, 33, 33, 34, 35, 36, 36, 38, 39, 39, 39, 39
                                                  //| , 40, 40, 41, 41, 41, 41, 43, 43, 45, 47, 47, 47, 48, 48, 50, 53, 57, 58, 5
                                                  //| 8, 58, 58, 58, 59, 60, 62, 63, 66, 68, 70, 70, 71, 71, 73, 73, 74, 76, 76, 
                                                  //| 77, 77, 78, 79, 82, 83, 83, 85, 88, 88, 88, 89, 90, 91, 91, 92, 92, 93, 93,
                                                  //|  93, 96, 97, 99, 99)
}