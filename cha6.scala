trait RNG {
  def nextInt: (Int, RNG)
}
object RNG {
  def simple(seed: Long): RNG = new RNG {
    def nextInt = {
      val seed2 = (seed * 0x5DEECE66DL + 0xBL) &
        ((1L << 48) - 1)
      ((seed2 >>> 16).asInstanceOf[Int],
        simple(seed2))
    }
  }
}

type State[S,+A] = S => (A,S)
//case class State[S, +A](run: S=>(A,S))
type Rand[A] = State[RNG, A]
//1
def positiveInt(rng: RNG): (Int, RNG)  = {
    var next = rng.nextInt
    if (next._1 == Int.MinValue ){
        next = next._2.nextInt
    }
    next = next.copy(_1 = next._1.abs)
    next
}
//2
def double(rng: RNG): (Double, RNG)={
    val temp = positiveInt(rng)
    (temp._1.toDouble/Int.MaxValue.toDouble, temp._2)
}
//4
def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    var tempRng = rng
    var rs = List[Int]()
    var tempCount = count
    while(tempCount > 0){
        val temp = tempRng.nextInt
        rs = rs++List(temp._1)
        tempRng = temp._2
        tempCount = tempCount -1
    }
    (rs, tempRng)
}
// 5
//type Rand[+A] = RNG => (A, RNG)
val int: Rand[Int] = _.nextInt
def unit[S,A](a: A): S => (A,S) =
    rng => (a, rng)

def map[S,A,B](s: S => (A,S))(f: A => B): S => (B,S)=
    rng => {
        val (a, rng2) =s(rng)
        (f(a), rng2)
    }

def positiveMax(n: Int): Rand[Int]=
    map((rng: RNG) =>{ double(rng) })(x => (x*n).floor.toInt)



val rng = RNG.simple(2898090)
val rsInt = positiveInt(rng)
val rsDouble = double(rng)

println(rsInt, rsDouble, ints(5)(rng))

val pMax = positiveMax(30)(rng)
val pMax2 = positiveMax(30)(pMax._2)
val pMaxw = map(positiveMax(30))(z => z+30)(rng)

println(pMax, pMax2)
println(pMaxw)

// 6
// def map[A, B](s: Rand[A])(f: A=>B): Rand[B]=
def double2(rng: RNG): (Double, RNG) =
    map(positiveInt)(a=>a.toDouble/Int.MaxValue.toDouble)(rng)

println(double2(rng))

// 7
def map2[S,A,B,C](ra: S=>(A,S), rb: S=>(B,S))(f: (A, B) => C): S=>(C,S) =
    rng =>{
        val (a, rng1) = ra(rng)
        val (b, rng2) = rb(rng1)
        (f(a,b), rng2)
    }

// 8
def sequence[A](fs: List[Rand[A]]): Rand[List[A]]=
    rng =>{
        fs match {
            case Nil => (Nil, rng)
            case _ => {
                val (a, newRng) = fs.head(rng)
                (List(a) ++ sequence(fs.tail)(newRng)._1, newRng)
            }
        }
    }
// 9
def flatMap[S,A,B](f: S=>(A,S))(g: A => (S=>(B,S))): S=>(B,S) =
    rng => {
        val (a, rng2) = f(rng)
        g(a)(rng2)
    }

// 10
def mapCopy[A, B](s: Rand[A])(f: A=>B): Rand[B]=
    flatMap(s)(a =>{
        unit(f(a))
    })

def map2Copy[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a=>{
        flatMap(rb)(b=>{
            unit(f(a,b))
        })
    })
val pMaxw2 = mapCopy(positiveMax(30))(z => z+30)(rng)
println(pMaxw2)

//11
// types changed

/// 12
def get[S]: State[S, S] = (s:S) => (s, s)
def set[S](newS: S): State[S, Unit] = (s)=>((),newS)
