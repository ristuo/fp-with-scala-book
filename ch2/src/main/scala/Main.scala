object Exercise2_2 {
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean) = {
    @annotation.tailrec
    def sortty(i: Int, 
                  previous: A, 
                  ordered: (A, A) => Boolean, 
                  as: Array[A]): Boolean = {
      i match {
        case i if i == as.length => true
        case _ => ordered(previous, as(i)) && sortty(i + 1, as(i), ordered, as)
      }
    }
    as.length match {
      case 0 => true
      case _ => sortty(1, as(0), ordered, as)
    }
  }
}

object Exercise2_3 {
  def curry[A,B,C](f: (A,B) => C): A => (B => C) = {
    a: A => {
      b: B => f(a,b)
    }
  }
}

object Exercise2_4 {
  def uncurry[A,B,C](f: A => B => C): (A,B) => C = {
    (a: A, b: B) => f(a)(b)
  }
}

object Exercise2_5 {
  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    a: A => f(g(a))
  }
}

object Main {
  def main(args: Array[String]) {
    
  }
}
