trait Stream[+A] {
    def uncons: Option[(A, Stream[A])]
    def isEmpty: Boolean = uncons.isEmpty
    def toList: List[A] =
        uncons match {
            case None => List()
            case Some((hd, tl)) => List(hd) ++ tl.toList
        }
    def take(n: Int): Stream[A] =
        if( n== 0){
            Stream.empty
        } else {
            uncons match {
                case None => Stream.empty
                case Some((hd, tl)) => Stream.cons(hd, tl.take(n-1))
            }
        }
    def takeWhile(f: A=>Boolean): Stream[A] =
        uncons match {
            case None => Stream.empty
            case Some((hd, tl)) => if (f(hd)) Stream.cons(hd, tl.takeWhile(f)) else Stream.empty
        }
    def forall(f: A=>Boolean): Boolean =
        uncons match {
            case None => true
            case Some((hd, tl)) => f(hd) && tl.forall(f)
        }
    def exists(f: A=>Boolean): Boolean =
        uncons match {
            case None => false
            case Some((hd, tl)) => f(hd) || tl.exists(f)
        }
    def foldRight[B](z: => B)(f: (A, => B) => B): B =
        uncons match {
            case Some((h, t)) => f(h, t.foldRight(z)(f))
            case None => z
        }
    def takeWhile2(f: A=>Boolean): Stream[A] =
        this.foldRight(Stream.empty: Stream[A])((x, y)=>{
            if(f(x)) Stream.cons(x, y) else y
        })
    def map[B](f: A=> B):Stream[B] =
        this.foldRight(Stream.empty: Stream[B])((x, y)=>{
            Stream.cons(f(x), y)
        })

    def filter(f: A=>Boolean): Stream[A]=
        this.foldRight(Stream.empty: Stream[A])((x, y)=>{
            if(f(x)) Stream.cons(x, y) else y
        })
    def append[T >:A](init: Stream[T]): Stream[T] = {
        this.foldRight(init)((x, y)=>{
            Stream.cons(x, y)
        })
    }
    def flatMap[B](f: A=>Stream[B]): Stream[B] =
        foldRight(Stream.empty: Stream[B])((x, y)=>{
            f(x).append(y)
        })

}
object Stream {
    def empty[A]: Stream[A] =
        new Stream[A] { def uncons = None }
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
        new Stream[A] {
            lazy val uncons = Some((hd, tl))
        }
    def apply[A](as: A*): Stream[A] =
        if (as.isEmpty) empty
        else cons(as.head, apply(as.tail: _*))



}
//1
println(Stream(1,23,4).toList)
//2
println(Stream(1,2,3,4,5).take(2).toList)
//3
println(Stream(1,2,3,4,5).takeWhile((_ < 4)).toList)
//4
println(Stream(1,2,3,4,5).forall((_ < 4)))
println(Stream(1,2,3,4,5).forall((_ < 6)))
//5
println(Stream(1,2,3,4,5).takeWhile((_ < 4)).toList)
// 6
println(Stream(1,2,3,4,5).map((_ + 4)).filter((_ > 7)).append(Stream(1.1)).toList)

println(Stream(1,1,1,1,1)
.flatMap((x)=>Stream(x + 1, x+2)).toList)
//7
def constant[A](a: A): Stream[A] =
    Stream.cons(a, constant(a))
// 8
def from(n: Int): Stream[Int] =
    Stream.cons(n, from(n+1))
// 9
def fibs(): Stream[Int] = {
    def iter(a: =>Int, b: =>Int): Stream[Int] = {
        lazy val total = a + b
        Stream.cons(total, iter(b, total))
    }
    Stream.cons(0, Stream.cons(1, iter(0,1)))
}
println(fibs().take(15).toList)
// 10
def unfold[A, S](z: S)(f: S=>Option[(A,S)]): Stream[A] = {
    f(z) match {
        case None => Stream.empty
        case Some((a,s)) => Stream.cons(a, unfold(s)(f))
    }
}

println(unfold(0)((x)=> if(x <10) Some(x, x+1) else None ).toList)

//11
def const2(a: Int): Stream[Int] =
    unfold(a)((x)=> Some((x,x)))

def from2(a: Int): Stream[Int] =
    unfold(a)((x)=> Some(x, x+1))

def fibs2(): Stream[Int] =
    unfold((0,0))((x)=>x match {
        case (0, 0) => Some(0, (0,1))
        case (a, b) => Some(b, (b,a+b))
    })


println(const2(2).take(5).toList)
println(from2(2).take(5).toList)
println(fibs2().take(15).toList)

//12
def map2[A, B](a: Stream[A],f: A=>B):Stream[B] = {
    unfold(a)(x=>{
        x.uncons match {
            case None => None
            case Some((m, n)) => Some(f(m), n)
        }

    })
}

println(map2(Stream(1,2,3,4,5),(x: Int)=>x + 4).toList)

def take2[A](a: Stream[A], n: Int): Stream[A] ={
    unfold((a, n))((x)=>{
        (x._1.uncons, x._2) match {
            case (None, _) => None
            case (Some((a, s)), 0) => None
            case (Some((a, s)), count) =>  Some(a, (s, count-1))
        }
    })
}

println(take2(Stream(1,2,3,4,5),3).toList)

def takeWhile3[A](a: Stream[A], f: A=>Boolean): Stream[A]=
    unfold(a)(x=>{
        x.uncons match {
            case None => None
            case Some((a, s)) => if(f(a))Some(a,s) else None
        }
    })
println(takeWhile3(Stream(1,2,3,4,5),(x: Int) => x < 3).toList)

def zip[A, B](a: Stream[A], b: Stream[B]): Stream[(A, B)] =
    unfold((a,b))(x =>{
        (x._1.uncons, x._2.uncons) match {
            case (None, _) => None
            case (_, None) => None
            case (Some((a, sa)), Some((b, sb))) => Some((a, b), (sa, sb))
        }

    })
println( zip(Stream(1,2,3,4,5), Stream(1,2,3,4,5)).toList)

def zipAll[A, B](a: Stream[A], b: Stream[B], aElem: A, bElem: B): Stream[(A, B)] =
    unfold((a,b))(x =>{
        (x._1.uncons, x._2.uncons) match {
            case (None, None) => None
            case (None, Some((b, sb))) => Some(((aElem, b), (Stream.empty, sb)))
            case (Some((a, sa)), None) => Some(((a, bElem), (sa, Stream.empty)))
            case (Some((a, sa)), Some((b, sb))) => Some((a, b), (sa, sb))
        }

    })
println( zipAll(Stream(1,2,3), Stream(1,2,3,4,5), 1,1).toList)

//13
def startWith[A](a: Stream[A], b: Stream[A]): Boolean =
    zip(a, b).forall((x)=>{
        x match {
            case (m, n) => if (m == n) true else false
        }
    })
println("13:"+startWith(Stream(2,2), Stream(1,2,3,4,5)))
//14
def tails[A](a: Stream[A]): Stream[Stream[A]] =
    unfold(a)(x =>{
        x.uncons match {
            case None => None
            case Some((a,s)) => Some((x,s))
        }
    })
println(tails(Stream(1,2,3,4,5)).map(_.toList).toList)

def hasSubsequence[A](s1: Stream[A], s2: Stream[A]): Boolean =
  tails(s1) exists (y=>startWith(y,s2))
println(hasSubsequence(Stream(1,2,3,4,5), Stream(1,3)))
// 15
def scanRight[A, B](a: Stream[A], init: B)(f: (A, =>B)=>B): Stream[B] =
    tails(a).map(x =>{
        x.foldRight(init)(f)
    })
println(scanRight(Stream(1,2,3,4,5),0)(_ + _).toList)
