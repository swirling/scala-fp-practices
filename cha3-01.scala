trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]


object  List{
    def apply[A](as: A*):List[A] = {
        if( as.isEmpty ) Nil else {
            Cons(as.head, apply(as.tail: _*))
        }
    }
}
def tail[A](as: List[A]) = {
    as match {
        case Nil => throw new Error("no tail of Nil!")
        case Cons(h,t) => t
    }
}
println(tail[Int](List(1,2,3)))
def drop[A](xs: List[A], n: Int): List[A] = {
    if(n==0)
        xs
    else
        drop(tail(xs), n-1)
}
println(drop(List(1,3,4,5), 3))

def dropWhile[A](xs: List[A])(f: A=>Boolean):List[A]= {
    xs match {
        case Nil => Nil
        case Cons(x, t)=> if(f(x))dropWhile(t)(f) else xs
    }

}
println(dropWhile(List(1,3,4,5))((x: Int)=> x<4))

def setHead[A]( l: List[A], head: A)= l match {
    case Nil => Cons(head, Nil)
    case Cons(h, t) => Cons(head, t)
}
println(setHead(List(1,3,4,5), 4))

def init[A](l: List[A]): List[A] = {
    l match {
        case Nil => throw new Error("no such init of Nil")
        case Cons(a, Nil) => Nil
        case Cons(b, Cons(a, Nil)) => Cons(b, Nil)
        case Cons(x, xs) => Cons(x, init(xs))
    }

}
println(init(List(1,3,4,5)))

def foldRight[A, B](l: List[A], initValue: B)(f: (A, B)=>B): B = {
    l match {
        case Nil => initValue
        case Cons(h, t) => f(h, foldRight(t, initValue)(f))
    }
}

println(foldRight(List(1,3,4,5), 0)(_ + _))
println(foldRight(List(0.0, 2.0, 3.0), 1.0)((x: Double, y: Double)=>{
    if(x==0) 0 else x*y
}))
println(foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)))

def length[A](l : List[A]) = {
    foldRight(l, 0)((x: A, y: Int)=>{
        1+y
    })
}
println(length(List(12,3)))


def foldLeft[A, B](l: List[A], init: B)(f: (B, A)=> B): B = {
    l match {
        case Nil => init
        case Cons(h, t) => foldLeft(t, f(init, h))(f)
    }
}
println(foldLeft(List(1,3,4,5), 0)(_ + _))

def sum(l: List[Int]): Int = {
    foldLeft[Int, Int](l, 0)(_ + _)
}

def product(l: List[Int]): Int= foldLeft[Int, Int](l, 1)(_ * _)

println(sum(List(1,2,3))+"==="+ product(List(1,2,3)))
def lengthl[A](l : List[A]) = {
    foldLeft(l, 0)((y: Int, x: A)=>{
        1+y
    })
}

println(lengthl(List(12,3)))

def reverse[A](l: List[A]): List[A] = {
    foldLeft[A, List[A]](l, Nil)(( y:List[A], x: A)=>{
        x match {
            case Nil => y
            case _ => Cons(x, y)
        }
    })
}
println(reverse(List(12,3)))

def foldLeft2[A, B](l : List[A], z: B)(f: (B, A)=> B): B = {
    foldRight[A, B=>B](l , (b: B) => b)((a, g)=> b=> g(f(b, a)))(z)
}
println(foldLeft2(List(1,3,4,5), 0)(_ + _))
//13
def append[A](l: List[A], one: A): List[A]={
    l match {
        case Nil => Cons(one, Nil)
        case Cons(h, t) => Cons(h, append(t, one))
    }
}
println(append(List(1,2), 5))
//14
def concate[A](list1: List[A], list2: List[A]) = {
    foldRight[A,List[A]](list1, list2)((y: A, x: List[A])=>{
        Cons(y,x)
    })
}
println("concate" + concate(List(1,2),List(2)))
//15
def flatten[A](l: List[List[A]]): List[A] = {
    reverse(foldLeft(l, Nil: List[A])((x: List[A], y: List[A])=>{
        concate(y, x)
    }))
}

println(flatten(List(List(1),List(2),List(3))))
//16
def add1(l: List[Int]) = {
    reverse(foldLeft(l, Nil: List[Int])((x: List[Int], y: Int)=>{
        Cons(y+1, x)
    }))
}
println(add1(List(1,2)))
//17
def convert(l: List[Double]) = {
    reverse(foldLeft(l, Nil: List[String])((x: List[String], y: Double)=>{
        Cons(y.toString(), x)
    }))
}
println(convert(List(1.1,2.3)).isInstanceOf[List[String]])
//18
def map[A, B](list: List[A])(f: A=>B) = {
    reverse(foldLeft(list, Nil: List[B])((x: List[B], y: A)=>{
        Cons(f(y), x)
    }))
}
println(map(List(1,2,3,4))(_ + 1))
//19
def filter[A](list: List[A])(f: A=>Boolean) = {
    reverse(foldLeft(list, Nil: List[A])((x: List[A], y: A)=>{
        if(f(y)) Cons(y, x) else x
    }))
}
println(filter(List(1,2,3,4,5))(_ > 3))
//20
def flatMap[A, B](list: List[A])(f: A=> List[B]): List[B] = {
    flatten(map(list)(f))
}
println(flatMap(List(1,3,4,5))(i => List(i, i)))
//21
def filter2[A](list: List[A])(f: A=> Boolean): List[A] = {
    flatMap(list)((x: A)=>{
        if (f(x)) List(x) else Nil
    })
}
println(filter2(List(1,2,3,4,5))(_ > 3))
//22
def addList(list1: List[Int], list2: List[Int]): List[Int] = {
    (list1,list2) match {
        case (Cons(a,b),Cons(c,d)) => Cons(a+c, addList(b, d))
        case (Nil, _) => list2
        case (_, Nil) => list1
    }
}
println(addList(List(1,2,3), List(1,2)))
//23
def zipWith[A, B, C](list1: List[A], list2: List[B])(f: (A,B)=>C): List[C] = {
    (list1, list2) match {
        case (Cons(a,b),Cons(c,d)) => Cons(f(a,c), zipWith(b, d)(f))
        case _ => Nil
    }
}
println(zipWith(List(1,2,3), List("fdsa","fsdafdsa"))((x,y)=>{
    x.toString()+y
}))
