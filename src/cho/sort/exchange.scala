package cho.sort

/**
 * @author batangr00t@daum.net
 */
object exchange {
  
  //----------------------- quickSort1 ----------------------------
  //@scala.annotation.tailrec
  def bisect1(c: Int, xs: List[Int], under: List[Int], upper: List[Int]): (List[Int], List[Int]) = {
    xs match {
      case Nil => (under, upper)
      case x :: xs1 => {
        if (x < c) bisect1(c, xs1, x :: under, upper)
        else bisect1(c, xs1, under, x :: upper)
      }
    }
  }
  // quickSort1이 quickSort2 elpased time의 1~2배정도
  def quickSort1(xs: List[Int]): List[Int] = {
    xs match {
      case Nil => Nil
      case x :: xs1 => {
        val (under, upper) = bisect1(x, xs1, Nil, Nil)
        quickSort1(under) ::: x :: quickSort1(upper)
      }
    }
  }

  //----------------------- quickSort2 ----------------------------
  //@scala.annotation.tailrec
  def under(c: Int, xs: List[Int], zs: List[Int]): List[Int] = {
    xs match {
      case Nil      => zs
      case x :: xs1 => if (x < c) under(c, xs1, x :: zs) else under(c, xs1, zs)

    }
  }

  //@scala.annotation.tailrec
  def upper(c: Int, xs: List[Int], zs: List[Int]): List[Int] = {
    xs match {
      case Nil      => zs
      case x :: xs1 => if (x >= c) upper(c, xs1, x :: zs) else upper(c, xs1, zs)

    }
  }

  def quickSort2(xs: List[Int]): List[Int] = {
    //println(xs)
    xs match {
      case Nil => Nil
      case x :: xs1 => {
        quickSort2(under(x, xs1, Nil)) ::: x :: quickSort2(upper(x, xs1, Nil))
      }
    }
  }
}