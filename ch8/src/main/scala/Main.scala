import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

object Exercise8_1 {
  val intListGen = Gen.listOf(Gen.choose(0,100))
  val sameInts = Gen.listOf(Gen.choose(1,1))
  val prop1 = forAll(intListGen)(ns => ns.reverse.sum == ns.sum)
  val prop2 = forAll(sameInts)(ns => ns.sum == ns.length)
}

object Exercise8_2 {
  val intListGen = Gen.listOf(Gen.choose(0,100))
  val maxTest: List[Int] => Boolean = ns => {
    if (ns.length == 0) {
      true
    } else {
      val maximum = ns.max
      ns.map(_ <= maximum).foldLeft(true)(_ && _)
    }
  }
  val maxProp = forAll(intListGen)(maxTest)
}

trait Prop {
  def check: Boolean
  def &&(p: Prop) = this.check && p.check
}

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

case class Gen[A](sample: State[RNG, A])

object Exericse8_4 {
  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    val intervalLength = stopExclusive - start
    val res = (rng: RNG) => {
      val (rnd, nextRng) = rng.nextInt
      ((rnd % intervalLength) + start, nextRng)
    }
    Gen(State(res))
  }
}
