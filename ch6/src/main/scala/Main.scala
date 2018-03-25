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

object Exercise6_4 {
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

type Rand[+A] = RNG => (A, RNG)

object RndUtil {
  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = {
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }
  }
}

object Exercise6_5 {
  import RndUtil.map    
  import Exercise6_1.nonNegativeInt
  def double(rng: RNG) = {
    val f: Int => Double = _.toDouble / Int.MaxValue.toDouble
    map((rng: RNG) => nonNegativeInt(rng))(f)
  }
}

object Exercise6_6 {
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
