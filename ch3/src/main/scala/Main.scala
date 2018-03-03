sealed trait List[+A] {
  def foldRight[B](z: B)(f: (A,B) => B): B = {
    List.foldRight(this, z)(f)
  }

  def foldLeft[B](z: B)(f: (B, A) => B): B = {
    List.foldLeft(this, z)(f)
  }

  def setHead[B >: A](a: B): List[B] = {
    Exercise3_3.setHead(a, this)
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
      case as: Cons[A] => Cons(newHead, as)
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

object Exercise3_14 {
  def append[A](l: List[A], a: A): List[A] = {
    l.foldRight[List[A]](List[A](a))((b: A, j: List[A]) => Cons[A](b, j))
  }
}

object Exercise3_15 {
  def flatten[A](l: List[List[A]]): List[A] = {
    l.foldRight[List[A]](Nil)((l1: List[A], l2: List[A]) => {
      l1.foldRight(l2)((a, listy) => listy.setHead(a))                    
    })
  }
}

object Exercise3_16 {
  def f(a: List[Int]): List[Int] = {
    a match {
      case Nil => Nil
      case Cons(h, t) => Cons(h + 1, f(t))
    }
  }
}

object Exercise3_17 {
  def f(a: List[Double]): List[String] = {
    a match {
      case Nil => Nil
      case Cons(h,t) => Cons(h.toString, f(t))
    }
  }
}

object Exercise3_18 {
  def map[A,B](l: List[A])(f: A => B): List[B] = {
    l match {
      case Nil => Nil
      case Cons(h, t) => Cons(f(h), map(t)(f))
    }
  }
}

object Exercise3_19 {
  def filter[A](l: List[A])(f: A => Boolean): List[A] = {
    l match {
      case Nil => Nil
      case Cons(h, t) => if (f(h)) Cons(h, filter(t)(f)) else filter(t)(f) 
    }
  }
}

object Exercise3_20 {
  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = {
    Exercise3_15.flatten(Exercise3_18.map(l)(f))
  }
}

object Exercise3_21 {
  def filter[A](l: List[A])(f: A => Boolean): List[A] = {
    Exercise3_20.flatMap(l)((a: A) => {
      if (f(a)) List(a) else Nil
    })
  }
}

object Exercise3_22 {
  def add(l1: List[Int], l2: List[Int]): List[Int] = {
    (l1, l2) match { 
      case (Nil, Nil) => Nil
      case (x, Nil) => x
      case (Nil, x) => x
      case (Cons(a, t1), Cons(b, t2)) => Cons(a + b, add(t1, t2))
    }
  }
}

object Exercise3_23 {
  def zipWith[A](l1: List[A], 
                 l2: List[A]
                )(f: (A, A) => A): List[A] = {
    (l1, l2) match { 
      case (Nil, Nil) => Nil
      case (x, Nil) => x
      case (Nil, x) => x
      case (Cons(a, t1), Cons(b, t2)) => Cons(f(a, b), zipWith(t1, t2)(f))
    }
  }
}
