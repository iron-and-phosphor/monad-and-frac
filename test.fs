open System
open System.IO
open System.Drawing
open System.Windows.Forms

type complex(r:float, i:float) =
  member this.r = r
  member this.i = i

  static member(+) (x:complex, y:complex) =
    complex(x.r+y.r, x.i+y.i)

  static member(*) (x:complex, y:complex) =
    complex(x.r*y.r-x.i*y.i, x.r*y.i+x.i*y.r)

  static member r2 (x:complex) = x.r*x.r + x.i*x.i

let mandelbrot c niter =
  let rec iterate z i =
    let zn = z * z + c
    if complex.r2(zn) > 2.0 then 0
    else
      if i = 0 then 1
      else iterate zn (i-1)
  iterate (complex(0., 0.)) niter

let seq2bitmaprow (bitmap:Bitmap) row seq =
  let mutable i = 0
  for x in seq do
    if x = 1 then
      bitmap.SetPixel(i, row, Color.Black)
    else
      bitmap.SetPixel(i, row, Color.White)
    i <- i + 1

let app =
  let w = int(Environment.GetCommandLineArgs().[1])
  let h = w
  let bitmap = new Bitmap(w, h)
  let row = [|0..(w-1)|]
  for y in [0..(h-1)] do
    [|0..(w-1)|]
      |> Array.map
        (fun x -> async {
          return (mandelbrot (complex(float (x*2)/(float w)-1.5,
                                      float (y*2)/(float h)-1.0)) 50)})
      |> Async.Parallel
      |> Async.RunSynchronously
      |> seq2bitmaprow bitmap y 

  let temp = new Form() in
  temp.Paint.Add(fun e -> e.Graphics.DrawImage(bitmap, 0, 0))
  temp

do Application.Run(app)

//  bitmap.Save("mandel.png")