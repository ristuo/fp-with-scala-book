trait RNG {
  def nextInt: (Int, RNG)
}
case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = { 
    val a = 0x5DEECE66DL
    val c = 0xBL
    val m = 0xFFFFFFFFFFFFL 
    val nextSeed = (a * seed + c) & m
    val nextRNG = SimpleRNG(nextSeed)
    ((nextSeed >>> 16).toInt, nextRNG)
  }
}

object Exercise6_1 {
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, nextRng) = rng.nextInt
    (n >>> 1, nextRng)
  }
}

object Exercise6_2 {
  import Exercise6_1.nonNegativeInt
  def double(rng: RNG): (Double, RNG) = {
    val (x, nextRNG) = nonNegativeInt(rng)
    (x.toDouble / Int.MaxValue.toDouble, nextRNG)
  }
}

object Exercise6_3 {
  import Exercise6_2.double
  def intDouble(rng: RNG): ((Int, Double), RNG) = { 
    val (int1, rng1) = rng.nextInt
    val (double1, rng2) = double(rng1)
    ((int1, double1), rng2) 
  }
  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, x), rng1) = intDouble(rng) 
    ((x, i), rng1)
  }
  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (double1, rng1) = double(rng)
    val (double2, rng2) = double(rng1)
    val (double3, rng3) = double(rng2)
    ((double1, double2, double3), rng3)
  }
}

object Exercise6_4 {
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def intsAccu(acc: (List[Int], RNG), 
                 i: Int, 
                 count: Int): (List[Int], RNG) = {
      if (i >= count) {
        acc
      } else {
        val (nextInt, nextRNG) = acc._2.nextInt
        intsAccu((nextInt :: acc._1, nextRNG), i + 1, count)
      }
    }
    intsAccu((Nil, rng), 0, count)
  }
}


object RndUtil {
  type Rand[+A] = RNG => (A, RNG)
  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = {
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }
  }
}

object Exercise6_5 {
  import RndUtil.map    
  import RndUtil.Rand
  import Exercise6_1.nonNegativeInt
  def double(rng: RNG) = {
    val f: Int => Double = _.toDouble / Int.MaxValue.toDouble
    map((rng: RNG) => nonNegativeInt(rng))(f)
  }
}

object Exercise6_6 {
  import RndUtil.Rand
  import RndUtil.map
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, nextRNG) = ra(rng)
      val (b, finalRNG) = rb(rng)
      (f(a,b), finalRNG)
    }
  }
}

object Exercise6_7 {
  import RndUtil.Rand
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    def seqAcc(acc: List[A], l: List[Rand[A]])(rng: RNG): (List[A], RNG) = {
      l match { 
        case Nil => (acc, rng)
        case h :: tail => {
          val (rand, s2) = h(rng)
          seqAcc(rand :: acc, tail)(s2)
        }
      }
    }
    rng => seqAcc(Nil, fs)(rng)
  }
  
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    val rands = List.fill(count)((x: RNG) => x.nextInt)
    sequence(rands)(rng) 
  }
}

object Exercise6_8 {
  import RndUtil.Rand
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    (rng: RNG) => {
      val (a, nextState) = f(rng)
      g(a)(nextState)
    }
  }
}

object Exercise6_9 {
  import RndUtil.Rand
  import Exercise6_8.flatMap
  def map[A,B](f: Rand[A])(g: A => B): Rand[B] = { 
    val func: A => Rand[B] = {
      (a: A) => {
        rng => {
          (g(a), rng)
        }
      }
    }
    flatMap(f)(func)
  }

  def map2[A,B,C](f: Rand[A], g: Rand[B])(h: (A,B) => C): Rand[C] = {
    flatMap(f)((a: A) => flatMap(g)((b: B) => (rng: RNG) => (h(a,b), rng)))
  }
}

object Exercise6_10 {
  case class State[S, +A](run: S => (A, S)) {
    def map[B](f: A => B): State[S, B] = {
      val run: S => (B, S) = s => {  
        val (a, nextState) = this.run(s)
        (f(a), nextState)
      }
      State(run)
    }
    def flatMap[B](f: A => State[S, B]): State[S, B] = {
      val run: S => (B, S) = s => {
        val (a, nextState) = this.run(s)
        f(a).run(nextState)
      }
      State(run)
    }
    def map2[B,C](f: State[S, B])(g: (A,B) => C): State[S,C] = {
      this.flatMap(a => {
        f.flatMap(b => {
          State((s: S) => (g(a,b), s))
        })
      })
    }
  }
  object State {
    def unit[S, A](a: A): State[S, A] = State((s: S) => (a,s))
    def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = {
      def seqAcc(acc: List[A], l: List[State[S, A]])(s: S): (List[A], S) = {
        l match { 
          case Nil => (acc, s)
          case head :: tail => {
            val (a, s2) = head.run(s)
            seqAcc(a :: acc, tail)(s2)
          }
        }
      }
      State(s => seqAcc(Nil, fs)(s))
    }
  }
}

object Exercise6_11 {
  import Exercise6_10.State
  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input
  case class Machine(val locked: Boolean, val candies: Int, val coins: Int)
  // what this teaches me about state monad I do not know
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = { 
    def handleInput(m: Machine, in: Input): Machine = {
      if (m.candies <= 0) {
        m
      } else {
        in match {
          case Coin => {
            if (m.locked) {
              Machine(false, m.candies, m.coins + 1)
            } else {
              m
            }
          }
          case Turn => {
            if (m.locked) {
              m
            } else {
              Machine(true, m.candies - 1, m.coins)
            }
          }
        }
      }
    }
    State((m: Machine) => {
      val res = inputs.foldLeft(m)(handleInput)
      ((res.coins, res.candies), res)
    })
  }
  def example = {
    val start = Machine(true, 5, 10)
    val inputs = List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)
    simulateMachine(inputs).run(start)
  }
}
