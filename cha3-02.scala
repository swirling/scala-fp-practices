//24 work with the scala.List

def hasSubSeq[A](l : List[A], sub: List[A]): Boolean = {
    if(!(l.length > sub.length)){
        return false;
    }
    if(l.head == sub.head ){
        l.zip(sub).forall(item =>{
            item match {
                case (i, j) => if(i==j)true else false
                case _ => false
            }
        })
    }else {
        hasSubSeq(l.tail, sub)
    }
}
println(hasSubSeq(List(1,2,3,4), List(1,2)))
println(hasSubSeq(List(1,2,3,4), List(3,4)))
println(hasSubSeq(List(1,2),List(1,2,3,4)))

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
//25
def size[A](tree: Tree[A]): Int = {
    tree match {
        case Leaf(_) => 1
        case Branch(left, right) => size(left) + size(right)
    }
}

println(size(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))))
//26
def max(tree: Tree[Int]): Int = tree match {
    case Leaf(a) => a
    case Branch(Leaf(a), Leaf(b)) => a max b
    case Branch(a, b) => max(a) max max(b)
}
println(max(Branch(Leaf(5),Branch(Leaf(1), Branch(Leaf(2), Leaf(3))))))

//27

def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(a, b) => (depth(a)+1) max (depth(b)+1)
}
println(depth(Branch(Leaf(5),Branch(Leaf(1), Branch(Leaf(2), Leaf(3))))))

//28
def map[A, B](tree: Tree[A])(f: A=>B): Tree[B] = tree match {
    case Leaf(a) => Leaf(f(a))
    case Branch(a, b) => Branch(map(a)(f), map(b)(f))
}
println(map(Branch(Leaf(5),Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))))(_ + 1))
//29
def fold[A](tree: Tree[A])(f: (A, A)=> A): A = tree match {
    case Leaf(a) => a
    case Branch(a, b) => f(fold(a)(f), fold(b)(f))
}
println(fold(Branch(Leaf(5),Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))))(_ + _))
