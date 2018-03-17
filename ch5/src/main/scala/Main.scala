sealed trait Stream[+A] {
  def toList(): List[A] = {
    this match {
      case Empty => Nil
      case Cons(h, t) => {
        h() :: t().toList
      }
    }
  }
  def take(n: Long): List[A] = {
    def takeCounting(n: Long, s: Stream[A], count: Long): List[A] = {
      if (count >= n) {
        Nil
      } else {
        s match {
          case Empty => Nil
          case Cons(h, t) => {
            h() :: takeCounting(n, t(), count + 1)
          }
        }
      }
    }
    takeCounting(n, this, 0)
  }

  def drop(n: Long): Stream[A] = {
    def dropAcc(n: Long, acc: Stream[A], seenSoFar: Long): Stream[A] = {    
      acc match { 
        case Empty => Empty
        case Cons(head, tail) => {
          if (seenSoFar >= n) {
            acc 
          } else {
            dropAcc(n, tail(), seenSoFar + 1)
          }
        }
      }
    }
    dropAcc(n, this, 0)
  }

  def takeWhile(f: A => Boolean): Stream[A] = {
    def takeWhileAcc(f: A => Boolean, acc: Stream[A]): Stream[A] = {
      acc match {
        case Empty => Empty
        case Cons(h, t) => {
          if (f(h())) {
            Stream.cons(h(), takeWhileAcc(f, t())) 
          } else {
            acc
          }
        }
      }
    }
    takeWhileAcc(f, Empty)
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = {
    this match {
      case Empty => z
      case Cons(head, tail) => {
        f(head(), tail().foldRight(z)(f))
      }
    }
  }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t1: () => Stream[A]) extends Stream[A]
object Stream {
  def cons[A](hd: => A, t1: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = t1
    Cons(() => head, () => tail)
  }
}

object Exercise5_4 {
  def forAll[A](p: A => Boolean, s: Stream[A]): Boolean = {
    s match {
      case Empty => true
      case Cons(head, tail) => {
        if (p(head())) {
          forAll(p, tail())
        } else {
          false
        }
      }
    }
  }
}

object Exercise5_5 {
  def takeWhile[A](s: Stream[A], p: A => Boolean): Stream[A] = {
    val combine: (A, => Stream[A]) => Stream[A] = {
      (v, acc) => {
        if (p(v)) {
          println(v)
          Stream.cons(v, acc)
        } else {
          println(v)
          Empty
        }
      }
    }
    val start: Stream[A] = Empty
    s.foldRight(start)(combine)
  }
}

object Exercise5_6 {
  def headOption[A](s: Stream[A]): Option[A] = {
    val start: Option[A] = None
    val f: (A, => Option[A]) => Option[A] = (a, acc) => Some(a)
    s.foldRight(start)(f)
  }
}

object Exercise5_7 {
  def map[A, B](s: Stream[A])(f: A => B): Stream[B] = {
    val acc: Stream[B] = Empty
    val g: (A, => Stream[B]) => Stream[B] = (a, sb) => {
      Stream.cons(f(a), sb)
    }
    s.foldRight(acc)(g)
  }

  def filter[A](s: Stream[A])(p: A => Boolean): Stream[A] = {
    val acc: Stream[A] = Empty
    val f: (A, => Stream[A]) => Stream[A] = (a,s) => {
      if (p(a)) {
        Stream.cons(a, s)
      } else {
        Empty
      }
    }
    s.foldRight(acc)(f)
  }

  def append[A](s: Stream[A], a: => A): Stream[A] = {
    val f: (A, => Stream[A]) => Stream[A] = (a, s) => {
      Stream.cons(a, s)
    }
    val start: Stream[A] = Stream.cons(a, Empty)
    start.foldRight(s)(f)
  }
}

object Exercise5_8 {
  def constant[A](a: A): Stream[A] = {
    Stream.cons(a, constant(a))
  }
}

object Exercise5_9 {
  def from(n: Int): Stream[Int] = {
    Stream.cons(n, from(n + 1))
  }
}

object Exercise5_10 {
  def fibs(): Stream[Long] = {
    def fibby(f_1: Long, f_2: Long): Stream[Long] = {
      val f_3 = f_1 + f_2
      Stream.cons(f_3, fibby(f_2, f_3))
    }
    Stream.cons(0, Stream.cons(1, fibby(0, 1)))
  }
}
