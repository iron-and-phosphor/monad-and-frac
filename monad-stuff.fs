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

let st = StateBuilder()

let plus = (fun a -> (fun s:'s -> a+5,s))

let test st{
  let! s0 = 

}



(*
type ResultBuilder()=
  member this.Return (x:'a) :Result<'a,'s> =
    fun s = Result(x,s)
  member this.Bind (Result<'a,'s>) ('a -> Result<'a,'s>) :Result<'b,'s> =
*)

