// #load "c:/work/adventofcode2018/day01/.paket/load/net472/main.group.fsx"

open System

type Claim = {
  Name: string
  Left: int
  Top: int
  Width: int
  Height: int }

let split (c:char) (x:String) = 
  let y = x.Trim().Split(c)
  y.[0],y.[1]

let parseLine s =
  let name, rest = s |> split '@'
  let corner, dims = rest |> split ':'
  let splitInt c s = s |> split c |> (fun a -> a |> fst |> int,a |> snd |> int)
  let le,t = corner |> splitInt ','
  let w,h = dims |> splitInt 'x'
  { Name = name; Left = le; Top = t; Width = w; Height = h }

let overlay (a:int [,]) (b:Claim) =
  let increment x y a = Array2D.set a x y (a.[x,y] + 1)
  seq { b.Top .. (b.Top+b.Height-1) }
  |> Seq.iter (fun y ->
    seq { b.Left .. (b.Left+b.Width-1) }
    |> Seq.iter (fun x ->
      increment x y a))
  a

let layClaims c w = c |> Seq.fold overlay w

let findOverlaps (p:int [,]) =
  let inches = seq {
    let e = p.GetEnumerator ()
    while e.MoveNext () do 
      yield e.Current :?> int }
  inches
  // |> Seq.map (fun x -> printf "%i, " x; x)
  |> Seq.filter (fun x -> x > 1)
  |> Seq.length


let testClaims =
  @"#1 @ 1,3: 4x4
#2 @ 3,1: 4x4
#3 @ 5,5: 2x2"
  |> (fun s -> s.Split('\n'))
  |> Array.toSeq
  |> Seq.map parseLine

let testWhole: int [,] = Array2D.zeroCreate 15 15 |> layClaims testClaims

findOverlaps testWhole
|> printfn "test one: %i"

seq { 0 .. testWhole.GetLength 0 - 1 }
|> Seq.iter (fun x -> 
  seq { 0 .. testWhole.GetLength 1 - 1 } 
  |> Seq.iter (fun y ->
    testWhole.[x,y] |> printf "%i" )
  printf "\n")


let input = System.IO.File.ReadLines "c:/work/adventofcode2018/day03/input.txt"
let claims = input |> Seq.map parseLine 
let whole: int [,] = Array2D.zeroCreate 1500 1500 |> layClaims claims

findOverlaps whole |> printfn "Part 1: %i"

let exclusiveClaim (p:int [,]) claims =
  let checkExclusive claim =
    seq { claim.Top .. (claim.Top+claim.Height-1) }
    |> Seq.forall (fun y ->
      seq { claim.Left .. (claim.Left+claim.Width-1) }
      |> Seq.forall (fun x -> p.[x,y] = 1 ))
  
  claims |> Seq.find checkExclusive 

let printClaimArea (w: int [,]) c =
  seq { c.Top .. c.Top+c.Height - 1 }
  |> Seq.iter (fun x -> 
    seq { c.Left .. c.Left+c.Width - 1 } 
    |> Seq.iter (fun y ->
      w.[x,y] |> printf "%i" )
    printf "\n")

let e = claims |> exclusiveClaim whole
e |> printClaimArea whole
e.Name |> printfn "Part 2: %s"

