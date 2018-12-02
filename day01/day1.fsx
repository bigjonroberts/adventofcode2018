#load "c:/work/adventofcode2018/day01/.paket/load/net472/main.group.fsx"

open System
open FastCollections.Unsafe

let parseLine (s:String) : int =
    s |> Seq.tail |> Array.ofSeq |> String.Concat |> int
    |> match (Seq.head s) with | '-' -> (fun x -> x * -1)  | _ -> id

let freqCalc (x:string seq) : int = 
    x |> Seq.sumBy parseLine

let testInput calc name (s:String) = s.Split('\n') |> calc |> printfn "test %s: %i" name

"+1\n+1\n+1" |> testInput freqCalc "one"
"+1\n+1\n-2" |> testInput freqCalc "two"
"-1\n-2\n-3" |> testInput freqCalc "three"

let input = System.IO.File.ReadLines "c:/work/adventofcode2018/day01/input.txt"

input |> freqCalc |> printfn "Part 1: %i"


let freqCalc2 (x:string seq) : int =
    let source = seq { while true do yield! x }
    let rec findDup modSource (currentF:int) (priorF:BTree<int,int>) =
        if (currentF = 57538) then
          printfn "Found it!\n\n\n\n\n"
        match priorF.ContainsKey currentF with
          | true ->
            printfn "%i" currentF 
            currentF
          | false ->
            let newF = modSource |> Seq.head |> parseLine |> (+) currentF 
            printf "%i, " newF
            priorF.Add (currentF,currentF)
            findDup (Seq.tail modSource) newF priorF
    use b = new BTree<int,int> (null)
    findDup source 0 b

"+1\n-1" |> testInput freqCalc2 "one"
"+3\n+3\n+4\n-2\n-4" |> testInput freqCalc2 "two"
"-6\n+3\n+8\n+5\n-6" |> testInput freqCalc2 "three"
"+7\n+7\n-2\n-7\n-4" |> testInput freqCalc2 "four"

let solvePart2 changes =
    let cumulativeSum =
        Seq.scan (+) 0 changes // get cumulative sums
        |> Seq.tail // ignore 0 at start
        |> Seq.toArray // convert to array for performance reasons
    let sumSet = Set.ofArray cumulativeSum
    let  = Array.last cumulativeSum // the final element will be the resulting sum
    let rec iterate sums =
        let newSums = (Array.map ((+) finalSum) sums)
        let firstMatch = Array.tryFind (fun i -> Set.contains i sumSet) newSums
        match firstMatch with
        | Some x -> x
        | None -> iterate newSums
    iterate cumulativeSum

let benInput = System.IO.File.ReadLines "c:/work/adventofcode2018/day01/ben_input.txt"

input |> freqCalc2 |> printfn "Part 2: %i"
//benInput |> freqCalc2 |> printfn "Part 2: %i"
//input |> Seq.map parseLine |> solvePart2 |> printfn "Part 2: %i"
