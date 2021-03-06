open System

type Result<'a,'s> =
  Done of 'a*'s
  | Running of Coroutine<'a,'s>

and Coroutine<'a,'s> =
  's -> Result<'a,'s>

type State<'a,'s> = 's -> ('a*'s)

type StateBuilder() =
  member this.Bind (o:State<'a,'s>, f:'a -> State<'b,'s>) :State<'b,'s> =
   fun s ->
     let (a,s') = o s
     let (b,s'') = f a s'
     (b,s'')
  member this.Return x = fun (s:'s) -> x,s
  member this.ReturnFrom x = x

let st = StateBuilder()

let s0 = fun s -> (1),(0)

let f1 = fun f -> (fun s -> ((f*2),(s+1)))

let test = st{
  let t1 = s0 f1
  return! t1

}


(*
type ResultBuilder()=
  member this.Return (x:'a) :Result<'a,'s> =
    fun s = Result(x,s)
  member this.Bind (Result<'a,'s>) ('a -> Result<'a,'s>) :Result<'b,'s> =
*)

