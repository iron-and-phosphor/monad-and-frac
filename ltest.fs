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

let w = 500
let h = 500
let iter = 360
let shift = 60
let bitmap = new Bitmap(w, h)

let mandelbrot (c) :int*int*int =
  let torgb (i:int) :int*int*int =
    let mo = ((i + shift) % 360)
    if     0<mo&&mo<61  then 255,255*(mo-0)/60,0
    elif  60<mo&&mo<121 then 255-(255*(mo-60)/60),255,0
    elif 120<mo&&mo<181 then 0,255,255*(mo-120)/60
    elif 180<mo&&mo<241 then 0,255-(255*(mo-180)/60),255
    elif 240<mo&&mo<301 then 255*(mo-240)/60,0,255
    elif 300<mo&&mo<361 then 255,0,255-(255*(mo-300)/60)
    else 0,0,0

  let rec iterate z i =
    let zn = z * z + c
    if complex.r2(zn) > 2.0 then (torgb i)
    else
      if i >= iter then 0,0,0
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

let growx (c:complex) :complex list = 
  let rec res (x:int) :complex list =
    if x < w then
      let calc = (c + complex(float((x-(w/2))*2)/(float w)-0.5,0.0,x,0))
      let r,g,b = mandelbrot calc
      bitmap.SetPixel((calc.px),(calc.py),Color.FromArgb(r,g,b))
      calc :: res (x+1)
    else []
  res 0

let test = lb{
  let! l = [complex(0.0,0.0,0,0)]
  let! y = growy l
  return! growx y
}

let rec lpak (uit:complex list) = 
  match uit with 
  | [] -> []
  | x::xs -> 
    printfn "%A" (x.r,x.i,x.px,x.py)
    x :: lpak xs 

let app =
  let temp = new Form() in
  temp.Paint.Add(fun e -> e.Graphics.DrawImage(bitmap, 0, 0))
  temp


[<EntryPoint>]
let main args =
  let stuff = test
  //printfn "%A" (lpak(test))
  do Application.Run(app)
  0