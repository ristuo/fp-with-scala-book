sealed trait List[+A] {
  def foldRight[B](z: B)(f: (A,B) => B): B = {
    List.foldRight(this, z)(f)
  }

  def foldLeft[B](z: B)(f: (B, A) => B): B = {
    List.foldLeft(this, z)(f)
  }
  
  def length = {
    List.length(this)
  }
}
case object Nil extends List[Nothing] {
  override def toString() = ""
}
case class Cons[+A](head: A, tail: List[A]) extends List[A] {
  override def toString() = head.toString + " " + tail.toString
}

object List {
  def apply[A](as: A*): List[A] = {  
    if (as.isEmpty) {
      Nil
    } else {
      Cons(as.head, apply(as.tail: _*))
    }
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A,B) => B): B = {
    as match {
      case Nil => z
      case Cons(head, tail) => f(head, foldRight(tail, z)(f))
    }
  }

  def length[A](listy: List[A]): Int = {
    foldRight(listy, 0)((_, i) => i + 1)
  }

  @annotation.tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match { 
      case Nil => z
      case Cons(head, tail) => {
        foldLeft(tail, f(z, head))(f)
      }
    }
  }
}

object Exercise3_2 {
  def tail[A](as: List[A]): List[A] = {
    as match {
      case  Nil => Nil
      case as: Cons[A] => as.tail
    }
  }
}

object Exercise3_3 {
  def setHead[A](newHead: A, as: List[A]): List[A] = {
    as match { 
      case Nil => List(newHead)
      case as: Cons[A] => Cons(newHead, as.tail)
    }
  }
}

object Exercise3_4 {
  def drop[A](listy: List[A], n: Int): List[A] = {
    def dropCount(listy: List[A], n: Int, nDropped: Int): List[A] = {
      if (nDropped == n) {
        listy
      } else {
        listy match {
          case Nil => Nil
          case Cons(head, tail) => dropCount(tail, n, nDropped + 1)
        }
      }
    }
    dropCount(listy, n, 0)
  }
}

object Exercise3_5 {
  def dropWhile[A](listy: List[A], f: A => Boolean): List[A] = {
    listy match {
      case Nil => Nil
      case Cons(head, tail) => if (f(head)) dropWhile(tail, f) else listy
    }
  }
}

object Exercise3_6 {
  def init[A](listy: List[A]): List[A] = {
    listy match {
      case Nil => Nil
      case Cons(head, tail) => {
        tail match {
          case Nil => Nil
          case Cons(nextHead, nextTail) => {
            Cons(head, init(tail))
          }
        }
      }
    }
  }
}

object Exercise3_11 {
  def sum[T](listy: List[T])(implicit num: Numeric[T]): T = {
    listy.foldLeft(num.zero)((a: T,b: T) => num.plus(a,b))
  }

  def product[T](listy: List[T])(implicit num: Numeric[T]): T = {
    listy.foldLeft(num.one)(num.times(_,_))
  }

  def length[T](listy: List[T]): Int = {
    listy.foldLeft(0)( (i, _) => i + 1)
  }
}

object Exercise3_12 {
  def reverse[T](listy: List[T]): List[T] = {
    def reverseAccumulated[T](res: List[T], listy: List[T]): List[T] = { 
      listy match {
        case Nil => res
        case Cons(head, tail) => {
            reverseAccumulated(Cons(head, res), tail)
          }
        }
      }
    reverseAccumulated(Nil, listy) 
  }
  def reverseWithFold[T](listy: List[T]): List[T] = {
    listy.foldLeft(Nil: List[T])( (a: List[T], b: T) => Cons(b, a))
  }
}
