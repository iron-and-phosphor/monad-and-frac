open System
open System.IO
open System.Drawing
open System.Windows.Forms

let listReturn x = [x]

let rec listBind (l:'a list) (f:'a -> 'b list) :'b list =
  match l with
  | [] -> []
  | x::xs -> (f x) @ listBind xs f 

type listBuilder() =
  member this.Bind(l,f) = listBind l f
  member this.Return(l) = listReturn l
  member this.Zero() = []

let lb = listBuilder()

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

let blowy = (fun x c -> c + complex(0.0,float(x*2)/(float h),0,(x+int(w/2))))
let blowx = (fun x c -> 
  let calc = c + complex(float(x*2)/(float w)-0.5,0.0,(x+int(w/2)),0)
  bitmap.SetPixel((calc.px-1),(calc.py-1),Color.FromArgb(0,(mandelbrot calc),0))
  calc)

let rec blowup (flist)(amount) (ax:int -> complex -> complex) =
  let rec row (a:complex) (elnumb) =
    if elnumb > int(amount * -0.5) then
      (ax elnumb a) :: (row a (elnumb - 1))
    else []
  match flist with
  | [] -> []
  | x::xs -> (row x (int(amount * 0.5))) @ (blowup xs amount ax)

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

let growy c = 
  blowup (c::[]) (float h) blowy

let growx c = 
  blowup (c::[]) (float w) blowx

let test = lb{
  let! y = [complex(0.0,0.0,0,0)]
  let! x = growy y
  let! z = growx x
  return z
}

[<EntryPoint>]
let main args =
  printfn "%A" test
  do Application.Run(app)
  0