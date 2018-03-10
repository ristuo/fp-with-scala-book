trait Option[+A] {
  def map[B](f: A => B): Option[B] = {
    this match {
      case None => None
      case Some(a) => Some(f(a))
    }
  }

  def getOrElse[B >: A](default: => B): B = {
    this match {
      case None => default
      case Some(a) => a
    }
  }

  def flatMap[B](f: A => Option[B]): Option[B] = {
    this.map(f).getOrElse(None)
  }

  def orElse[B >: A](op: => Option[B]): Option[B] = {
    op match {
      case None => op
      case Some(a) => Some(a)
    }
  }

  def filter(f: A => Boolean): Option[A] = {
    this.flatMap((a: A) => if (f(a)) Some(a) else None)
  }
}
case class Some[A](val get: A) extends Option[A]
case object None extends Option[Nothing]

object Exercise4_2 {
  def variance(s: Seq[Double]): Option[Double] = {
    val length = s.size 
    if (length < 2) {
      None
    } else {
      val sum = s.sum
      val mean = sum / length.toDouble
      Some(s.map(a => math.pow(a - mean, 2)).sum / (length - 1))
    }
  }
}

object Exercise4_3 {
  def map2[A,B,C](op1: Option[A], 
                  op2: Option[B]
                 )(f: (A, B) => C): Option[C] = {
    op1.flatMap((a: A) => { 
      op2.map((b: B) => {
        f(a,b)
      })
    })
  }
}

object Exercise4_4 {
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    @annotation.tailrec
    def appendValue[A](acc: Some[List[A]], 
                       l: List[Option[A]]
                      ): Option[List[A]] = {
      l match {
        case Nil => acc
        case head :: tail => {
          head match { 
            case Some(v) => appendValue(Some(v :: acc.get), tail)
            case None => None
          }
        }
      }
    }
    appendValue(Some[List[A]](Nil), a)
  }
}



object Exercise4_5 {
  def Try[A](a: => A): Option[A] = {
    try { 
      Some(a) 
    } catch {
      case e: Exception => None
    }
  }
  def traverse[A,B](a: List[A])(f: A => B): Option[List[B]] = {
    val g: A => Option[B] = a => Try(f(a))
    @annotation.tailrec
    def appendValue(acc: Some[List[B]], 
                    l: List[A]
                   ): Option[List[B]] = {
      l match {
        case Nil => acc
        case v :: tail => {
          g(v) match {
            case Some(u) => appendValue(Some(u :: acc.get), tail)
            case None => None
          }
        }
      }
    }
    appendValue(Some[List[B]](Nil), a)
  }
}

trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = {
    this match {
      case Left(v) => Left[E](v)
      case Right(v) => Right[B](f(v))
    }
  }
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = {
    this match { 
      case Left(e) => Left(e)
      case Right(v) => f(v)
    }
  }
  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = {
    this match {
      case Right(_) => this
      case Left(_) => b
    }
  }
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A,B) => C): Either[EE, C] = {
    this.flatMap(a => {
      b.map(f(a, _))
    })
  }
}
case class Right[+A](value: A) extends Either[Nothing, A]
case class Left[+E](value: E) extends Either[E, Nothing]

object Exercise4_7 {
   def sequence[E, A](a: List[Either[E, A]]): Either[E, List[A]] = {
    @annotation.tailrec
    def appendValue[A](acc: Right[List[A]], 
                       l: List[Either[E, A]]
                      ): Either[E, List[A]] = {
      l match {
        case Nil => acc
        case head :: tail => {
          head match { 
            case Right(v) => appendValue(Right(v :: acc.value), tail)
            case Left(e) => Left(e)
          }
        }
      }
    }
    appendValue(Right[List[A]](Nil), a)
  } 
  def traverse[E,A,B](a: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    @annotation.tailrec
    def appendValue(acc: Right[List[B]], 
                    l: List[A]
                   ): Either[E, List[B]] = {
      l match {
        case Nil => acc
        case v :: tail => {
          f(v) match {
            case Right(u) => appendValue(Right(u :: acc.value), tail)
            case Left(e) => Left(e) 
          }
        }
      }
    }
    appendValue(Right[List[B]](Nil), a)
  }
}
