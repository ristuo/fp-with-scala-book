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
case class Some[A](get: A) extends Option[A]
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
    def appendValue[A](acc: Option[List[A]], 
                       l: List[Option[A]]
                      ): Option[List[A]] = {
      l match {
        case Nil => acc
        case Some(v) :: tail => appendValue(Some(v :: acc.get), tail)
        case None :: tail => None
      }
    }
    appendValue(Some(Nil), a)
  }
}

def Try[A](a: => A): Option[A] = {
  try { 
    Some(a) 
  } catch {
    case e: Exception => None
  }
}

object Exercise4_5 {
  def traverse[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    @annotation.tailrec
    def appendValue(acc: Option[List[B]], 
                    l: List[A]
                   ): Option[List[B]] = {
      l match {
        case Nil => acc
        case v :: tail => {
          f(v) match {
            case Some(u) => appendValue(Some(u :: acc.get), tail)
            case None => None
          }
        }
      }
    }
    appendValue(Some(Nil), a)
  }
}








