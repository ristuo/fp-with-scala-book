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

case class Generator[+A](sample: State[RNG, A]) {
  def listOfN(n: Int): Generator[List[A]] = {
    val fs: List[State[RNG, A]] = List.fill(n)(this.sample)
    val res: State[RNG, List[A]] = State.sequence(fs)
    Generator(res)
  }
  def flatMap[B](f: A => Generator[B]): Generator[B] = {
    Generator(this.sample.flatMap((a: A) => f(a).sample))
  }
  def unsized: NewProp.SGen[A] = {
    NewProp.SGen((_: Int) => this)
  }
}

object Exercise8_4 {
  def choose(start: Int, stopExclusive: Int): Generator[Int] = {
    val intervalLength = stopExclusive - start
    val res = (rng: RNG) => {
      val (rnd, nextRng) = rng.nextInt
      ((rnd % intervalLength) + start, nextRng)
    }
    Generator(State(res))
  }
}

object Exercise8_5 {
  def unit[A](a: => A): Generator[A] = Generator[A](State.unit(a))
  def boolean: Generator[Boolean] = {
    val res = (rng: RNG) => {
      val (i, nextRNG) = rng.nextInt
      (i % 2 == 0, nextRNG)
    }
    Generator(State(res))
  }
  def listOfN[A](n: Int, g: Generator[A]): Generator[List[A]] = {
    val fs: List[State[RNG, A]] = List.fill(n)(g.sample)
    val res: State[RNG, List[A]] = State.sequence(fs)
    Generator(res)
  }
}

object Exercise8_8 {
  def weighted[A](g1: Generator[A], g2: Generator[A], weight: Double): Generator[A] = {
    val res = (rng: RNG) => {
      val g = if (scala.math.random > weight) g1 else g2 
      g.sample.run(rng)
    }
    Generator(State(res))
  }
}

object Exercise8_7 {
  import Exercise8_8.weighted
  def union[A](g1: Generator[A], g2: Generator[A]): Generator[A] = weighted(g1, g2, 0.5)
}


object NewProp {
  type SuccessCount = Int
  type FailedCase = String
  type TestCases = Int

  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    val isFalsified = false
  }

  case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
    val isFalsified = true
  }
  case class Prop(run: TestCases => Result) {
    def &&(p: Prop): Prop = {
      val newRun = (i: TestCases) => {
        val res1 = this.run(i)
        res1 match {
          case a: Falsified => a
          case _ => {
            val res2 = p.run(i)
            res2 match {
              case Passed => Passed
              case b: Falsified => b
            }
          }
        }
      }
      Prop(newRun)
    }
    def ||(p: Prop): Prop = {
      val newRun = (i: TestCases) => {
        val res1 = this.run(i)
        res1 match {
          case Passed => Passed
          case a: Falsified => {
            val res2 = p.run(i)
            res2 match {
              case Passed => Passed
              case b: Falsified => b
            }
          }
        }
      }
      Prop(newRun)
    }
  }
  case class SGen[+A](forSize: Int => Generator[A])
}
