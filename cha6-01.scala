
case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = this.flatMap(a => {
    State.unit(f(a))
  })

  def flatMap[B](f: A => State[S, B]): State[S, B] = new State(s => {
    val (a, s2) = this.run(s)
    f(a).run(s2)
  })
  def get[S]: State[S, S] = new State((s: S) => (s, s))
  def set[S, A](s: S): State[S, Unit] = new State(_ => ((), s))
  def modify(f: S => S): State[S, A] = State(s=>{
      val (a, _) = run(s)
      (a, f(s))
  })

}

object State {
  def unit[S, A](a: A): State[S, A] = State(rng => (a, rng))
}

val m1 = State ((s: String) => (s.size, s) )
println(m1.run("sss"))
val len = m1.set("fds")
println(len.run("f"))
val temp = m1.modify(a=> a+"fsd")
println(temp.run("gg"))
def repeat(num: Int): State[String, Unit] = State( (s: String) => ((), s * num) )
m1.flatMap(repeat).flatMap({ _ => m1 }).run("hello")
println(m1.flatMap(repeat).flatMap({ _ => m1 }).run("333"))

sealed trait Input
case object Coin extends Input
case object Turn extends Input
case class Machine(locked: Boolean, candies: Int, coins: Int)

val start = State((s: Machine) =>(s.coins,s))
def add1(num: Int): State[Machine, Int] = {
    return State(
        a => (num+1, a)
    );
}
def action(formState: State[Machine,Int], input: Input): State[Machine, Int] = input match {
    //def acc: State[Machine,Int] = sys.error("f")
    case Coin => (for{
        count <- formState
        state2 <-State.unit(count).modify((state: Machine) =>{
            Machine(locked= state.locked, candies= state.candies, coins= state.coins+1)
        })
        state3 <- State.unit(state2).map(_+1)
    } yield state3)
}
println(start)
val temp2 = action(action(start, Coin), Coin)
println(temp2.run(Machine(locked=true,candies=2,coins=5)))
def simulateMachine(inputs: List[Input]): State[Machine, Int] =
    inputs match {
        case Nil => State((machine: Machine) =>(0,machine))
        case (head:: tail) => head match {
            case Coin => for{
                count <- simulateMachine(tail)
                st <- State.unit(count).get
                a<-State.unit(count).modify((state: Machine) => {
                    if (state.locked && state.candies > 0) Machine(locked= !state.locked, candies= state.candies, coins= state.coins+1) else state})
                b<-State.unit(a).map(a=>if( st.locked && st.candies > 0 )a+1 else a)
            }yield b;
            case Turn => for{
                count <-simulateMachine(tail)
                a <-State.unit(count).modify((state: Machine) => {if (!state.locked && state.candies > 0) Machine(locked= !state.locked, candies= state.candies-1, coins= state.coins) else state})
            }yield a;
        }
    }
val rsSt = simulateMachine(List(Coin,Turn ,Coin,Turn, Coin)).run(Machine(locked=true,candies=2,coins=5))
println(rsSt)
