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

object Exercise5_11 {
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case None => Empty
      case Some((a,s1)) => Stream.cons(a, unfold(s1)(f))
    }
  }
}

object Exercise5_12 {
  import Exercise5_11.unfold
  def ones(): Stream[Int] = {
    unfold(())( Unit => Some(1, ()))
  }
  def constant[A](a: A): Stream[A] = {
    val z: Stream[A] = Empty
    unfold(z)( (x: Stream[A]) => Some((a, x)))
  }
  def from(a: Int): Stream[Int] = {
    val z: Int = a
    val f: Int => Option[(Int, Int)] = i => {
      Some(i, i + 1)
    }
    unfold(z)(f)
  }
  def fibs(): Stream[Long] = {
    val ij = (0L, 1L)
    val f: ((Long, Long)) => Option[(Long, (Long, Long))] = { case(i,j) => {
      val next = i + j
      Some(next, (next, i))
    }}
    Stream.cons(0, unfold(ij)(f))
  }
}

object Exercise5_13 {
  import Exercise5_11.unfold
  def map[A,B](s: Stream[A])(f: A => B): Stream[B] = {
    unfold(s)((x: Stream[A]) => {
      x match {
        case Empty => None
        case Cons(h, t) => Some((f(h()), t()))
      }
    })
  }
  def take[A](stream: Stream[A], n: Int): Stream[A] = {
    val state: (Int, Stream[A]) = (0, stream)
    unfold(state){ case(i, s) => {
      if (i >= n) { 
        None
      } else {
        s match {
          case Empty => None
          case Cons(h, t) => Some((h(), (i + 1, t())))
        }
      }
    }}
  }
  def takeWhile[A](s: Stream[A])(p: A => Boolean): Stream[A] = {
    unfold(s)(x => { 
      x match {
        case Empty => None
        case Cons(h, t) => {
          if (p(h())) {
            Some(h(), t())
          } else {
            None
          }
        }
      }
    })
  }
  def zipWith[A,B,C](s1: Stream[A], s2: Stream[B])(f: (A, B) => C): Stream[C] = {
    val state: (Stream[A], Stream[B]) = (s1, s2)
    val g: ((Stream[A], Stream[B])) => Option[(C, (Stream[A], Stream[B]))] = streams => {
      streams match {
        case (Empty, _) => None
        case (_, Empty) => None
        case (Cons(h1, t1), Cons(h2, t2)) => {
          Some(f(h1(), h2()), (t1(), t2()))
        }
      }
    }
    unfold(state)(g)
  }
  def zipAll[A,B](s1: Stream[A], s2: Stream[B]): Stream[(Option[A], Option[B])] = {
    type State = (Stream[A], Stream[B])
    type Result = (Option[A], Option[B])
    val state = (s1, s2)
    val g: State => Option[(Result, State)] = streams => {
      streams match {
        case (Cons(h, t), Empty) => Some(((Some(h()), None), (t(), Empty)))
        case (Empty, Cons(h, t)) => Some(((None, Some(h())), (Empty, t())))
        case (Empty, Empty) => None
        case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
      }
    }
    unfold(state)(g)
  }
}

object Exercise5_14 {
  import Exercise5_13.zipWith
  import Exercise5_4.forAll
  def startsWith[A](s1: Stream[A], s2: Stream[A]): Boolean = {
    forAll((x: Boolean) => x, zipWith(s1, s2)((a,b) => a == b))
  }
}

object Exercise5_15 {
  import Exercise5_11.unfold
  def tails[A](stream: Stream[A]): Stream[Stream[A]] = {
    val res = unfold(stream)(s => {
      s match {
        case Empty => None
        case Cons(h, t) => Some(t(), t()) 
      }
    })
    Stream.cons(stream, res)
  }
}
