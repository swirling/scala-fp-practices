//1
sealed trait Option[+A]{
    def map[B](f: A=>B):Option[B] = this match {
        case None => None
        case Some(a) => Some(f(a))
    }
    def flatMap[B](f: A => Option[B]): Option[B] = this match {
        case None => None
        case Some(a) => f(a)
    }

    def getOrElse[B >: A](default: =>B): B = this match {
        case None => default
        case Some(a) => a
    }
    def orElse[B >: A](ob: Option[B]): Option[B] = this match {
        case None => ob
        case _ => this
    }
    def filter(f: A=>Boolean): Option[A] = this match {
        case None => None
        case Some(a) => if (f(a)) this else None
    }

}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]


println(Some(1).map((_ + 1)))
println(None.asInstanceOf[Option[Int]].map((_ + 1)))

def notZero( dnum: Double): Option[Double] =
    if(dnum == 0.0) None else Some(dnum)
println(Some(0.0).flatMap(notZero))
println(Some(2.0).flatMap(notZero))
println("flag ",for{data<-notZero(0.0)} yield data)
println(Some(22.2).getOrElse(0.0))
println(None.getOrElse(0.1))
println(None.orElse(Some(0.0)))

println(Some(22.2).filter((_ > 10)))
println(Some(22.2).filter((_ < 10)))

// 2
def vari(xs: Seq[Double]): Option[Double]={
    def mean(xs: Seq[Double]): Option[Double] =
        if (xs.isEmpty) None
        else Some(xs.sum / xs.length)
    val vMean = mean(xs)
    vMean.flatMap(v=>mean(xs.map(item => math.pow(item - v,2))))
}

println(vari(List(1.0,1.0,1.0,1.0,1.0)))
println(vari(List()))

def lift[A, B](f: A=> B): Option[A]=>Option[B] = _ map f

import java.util.regex._
def pattern(s: String): Option[Pattern] =
    try {
        Some(Pattern.compile(s))
    } catch {
        case e: PatternSyntaxException => None
    }
def mkMatcher(pat: String): Option[String => Boolean] = {
    pattern(pat) map (p => (s: String) => p.matcher(s).matches)
}
def mkMatcher_1(pat: String): Option[String => Boolean] =
    for {
        p <- pattern(pat)
    } yield ((s:String)=> p.matcher(s).matches)

def doMatch(pat: String, s: String): Option[Boolean] =
    for {
        p <- mkMatcher_1(pat)
    } yield p(s)

def bothMatch(pat: String, pat2: String, s: String): Option[Boolean] =
    for {
        f <- mkMatcher(pat)
        g <- mkMatcher(pat2)
    } yield f(s) && g(s)

// 3
def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(aValue => b.map(bValue=> f(aValue,bValue)))

println(map2(Some(2),Some(3))(_ + _))
println(map2(Some(2),None :Option[Int])(_ + _))

// 4
def bothMatch_2(pat: String, pat2: String, s: String): Option[Boolean] =
    map2(mkMatcher(pat),mkMatcher(pat2))((x, y)=> x(s)&&y(s))

// 5
def sequence[A](list: List[Option[A]]): Option[List[A]] = {
    list.foldRight(Some(List()): Option[List[A]])((x, y)=>{
        map2(x, y)((a, b)=> a :: b)
    })
}
println(sequence(List(Some(1),Some(2), Some(2))))
println(sequence(List(Some(1),None, Some(2))))

// 6
def traverse[A, B](list: List[A])(f: A=>Option[B]): Option[List[B]] = {
    list.foldRight(Some(List()): Option[List[B]])((x, y)=>{
        map2(f(x), y)((a, b)=> a :: b)
    })
}

def sequence2[A](list: List[Option[A]]): Option[List[A]] = {
    traverse(list)(a=>a)
}
println(sequence2(List(Some(1),Some(2), Some(2))))
println(sequence2(List(Some(1),None, Some(2))))

// 7
sealed trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B] =this match {
        case Left(a) => Left(a)
        case Right(a) => Right(f(a))
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
        case Left(a) => Left(a)
        case Right(a) => f(a)
    }
    def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
        case Left(a) => Left(a)
        case Right(a) => Right(a)
    }
    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C):Either[EE, C] =
        this.flatMap(aValue => b.map(bValue=> f(aValue,bValue)))

}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

//8
def traverse2[A, EE, B](list: List[A])(f: A => Either[EE, B]): Either[EE, List[B]] = {
    list.foldRight(Right(List()): Either[EE, List[B]])((x, y)=>{
        f(x).map2(y)((a, b)=> a :: b)
    })
}

// 9
case class Person(name: Name, age: Age)
sealed class Name(val value: String)
sealed class Age(val value: Int)
def mkName(name: String): Either[String, Name] =
  if (name == "" || name == null) Left("Name is empty.")
  else Right(new Name(name))
def mkAge(age: Int): Either[String, Age] =
  if (age < 0) Left("Age is out of range.")
  else Right(new Age(age))
def mkPerson(name: String, age: Int): Either[String, Person] =
  mkName(name).map2(mkAge(age))(Person(_, _))

 println(mkPerson("fd", -1))
 println(mkPerson("", 1))
 println(mkPerson("", -1))
 println(mkPerson("fds", 1))
// use writer monad to manipulate the error?
// I dont wanna write any more.
