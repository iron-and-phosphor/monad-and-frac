open System
open System.IO
open System.Drawing
open System.Windows.Forms

type complex(r:float, i:float, px:int, py:int) =
  member this.r = r
  member this.i = i
  member this.px = px
  member this.py = py
  static member(+) (x:complex, y:complex) =
    complex(x.r+y.r, x.i+y.i, x.px+y.px, x.py+y.py)
  static member(*) (x:complex, y:complex) =
    complex(x.r*y.r-x.i*y.i, x.r*y.i+x.i*y.r, x.px, x.py)
  static member r2 (x:complex) = x.r*x.r + x.i*x.i

let w = 100
let h = 100
let iter = 255
let bitmap = new Bitmap(w, h)

let mandelbrot c =
  let rec iterate z i =
    let zn = z * z + c
    if complex.r2(zn) > 2.0 then i
    else
      if i >= iter then 0
      else iterate zn (i+1)
  iterate (complex(0.0,0.0,0,0)) 0

let rec listBind (l:'a list) (f:'a -> 'b list) :'b list =
  match l with
  | [] -> []
  | x::xs -> (f x) @ listBind xs f 

type listBuilder() =
  member this.Bind(l,f) = 
    match l with
    | [] -> []
    | x::xs -> (f x) @ listBind xs f 
  member this.Return(l) x = [x]
  member this.ReturnFrom x = x
  member this.Zero() = []

let lb = listBuilder()

let growy (c:complex) :complex list =  
  let rec res (y:int) :complex list =
    if y < h then
      (c + complex(0.0,float((y-(h/2))*2)/(float h),0,y))
        :: res (y+1)
    else []
  res 0

let bla c =
  c::c::[]

let test = lb{
  let! l = [complex(0.0,0.0,0,0)]
  let x = growy l
  return! x
}

let rec lpak (uit:complex list) = 
  match uit with 
  | [] -> []
  | x::xs -> 
    printfn "%A" (x.r,x.i,x.px,x.py)
    x :: lpak xs 

[<EntryPoint>]
let main args =
  printfn "%A" (lpak(test))
  //do Application.Run(app)
  0