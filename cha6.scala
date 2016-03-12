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
//type State[S,+A] = S => (A,S)
case class State[S,+A](run: S => (A,S))
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
val int: Rand[Int] = new Rand[Int](_.nextInt)
def unit[A](a: A): Rand[A] =
    new Rand[A](rng => (a, rng))

def map[A, B](s: Rand[A])(f: A=>B): Rand[B]=
    new Rand[B](rng => {
        val (a, rng2) =s.run(rng)
        (f(a), rng2)
    })

def positiveMax(n: Int): Rand[Int]=
    map(new Rand[Double](rng =>{ double(rng) }))(x => (x*n).floor.toInt)



val rng = RNG.simple(2898090)
val rsInt = positiveInt(rng)
val rsDouble = double(rng)

println(rsInt)
println(rsDouble)
println(new Rand[List[Int]](ints(5)).run(rng))

val pMax = positiveMax(30).run(rng)
val pMax2 = positiveMax(30).run(pMax._2)
val pMaxw = map(positiveMax(30))(z => z+30).run(rng)

println(pMax, pMax2)
println(pMaxw)

// 6
// def map[A, B](s: Rand[A])(f: A=>B): Rand[B]=
def double2(rng: RNG): (Double, RNG) =
    map(new Rand[Int](positiveInt))(a=>a.toDouble/Int.MaxValue.toDouble).run(rng)

println(double2(rng))

// 7
def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    new Rand[C](
        rng =>{
            val (a, rng1) = ra.run(rng)
            val (b, rng2) = rb.run(rng1)
            (f(a,b), rng2)
        }
    )

// 8
//type State[S,+A] = S => (A,S)
//case class State[S,+A](run: S => (A,S))
//type Rand[A] = State[RNG, A]
def sequence[A](fs: List[Rand[A]]): Rand[List[A]]=
    new Rand[List[A]](
        rng=>{
            fs match {
                case Nil => (Nil, rng)
                case _ => {
                    val (a, newRng) = fs.head.run(rng)
                    (List(a) ++ sequence(fs.tail).run(newRng)._1, newRng)
                }
            }
        }
    )
// 9
def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] ={
        val (a, rng2) = f.run(rng)
        g(a)
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
val pMaxw2 = mapCopy(positiveMax(30))(z => z+30).run(rng)
println(pMaxw2)

//11
