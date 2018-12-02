#load "c:/work/adventofcode2018/day01/.paket/load/net472/main.group.fsx"

open System

"abcdef" |> Seq.sort |> Seq.countBy id

let hasGroup i (s:String) =
  s |> Seq.sort
  |> Seq.countBy id
  |> Seq.filter (fun x -> (snd x) = i)
  |> Seq.isEmpty
  |> not
  |> function | true -> 1 | false -> 0

let getPart1CheckSum (s:seq<string>) =
  s
  |> Seq.map (fun s -> (hasGroup 2 s),(hasGroup 3 s))
  |> Seq.fold (fun (x,y) (x',y') -> (x+x',y+y') ) (0,0)
  |> (fun (x,y) -> x * y)

let testInput calc name (s:String) = s.Split('\n') |> calc |> printfn "test %s: %i" name

@"abcdef
bababc
abbcde
abcccd
aabcdd
abcdee
ababab"
|> testInput getPart1CheckSum "one"


let input = System.IO.File.ReadLines "c:/work/adventofcode2018/day02/input.txt"

input |> getPart1CheckSum |> printfn "Part 1: %i"

let findPart2matches (s:seq<string>) = 
  let isMatch a b =
    Seq.zip a b
    |> Seq.filter (fun (x,y) -> x <> y)
    |> Seq.length
    |> (=) 1
  let commonLetters a b =
    match isMatch a b with
      | true -> 
        Seq.zip a b
        |> Seq.filter (fun (x,y) -> x = y)
        |> Seq.map (fst >> string)
        |> String.concat ""
        |> Some
      | false -> None
  s
  |> Seq.tryPick (fun t -> s |> Seq.tryPick (commonLetters t))
  |> function | Some s -> s | None -> "No match!!!"

@"abcde
fghij
klmno
pqrst
fguij
axcye
wvxyz"
|> fun s -> s.Split('\n') 
|> findPart2matches
|> printfn "test one: %s"

input |> findPart2matches |> printfn "Part 2: %s"
