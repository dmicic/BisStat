namespace BisStat.Data.Parser

open System.Collections.Generic
open System.IO
open System.Linq

open FSharp.Data

type public Dimension(dimensionName: string, memberList: string[]) =  
    class
        member this.name = dimensionName
        member this.members = memberList
    end

type Dataset(dimensions: Dimension[]) =
    class 
        member this.dimensions = dimensions
    end
        
type dimPos = { dim : string; pos : int; members : string list }
        
[<AbstractClass>]
type public Parser(filePath: string) = 
    let data = CsvFile.Load(filePath, ",", '"',true,false, 8).Cache()
        
    abstract member headerRowCount : int

    member x.getDataset = 
            let lines = File.ReadLines(filePath)

            let headers = lines
                            |> Seq.skip x.headerRowCount
                            |> Seq.take 1 
                            |> Seq.collect (fun h -> h.Split ([|"\",\""|], System.StringSplitOptions.RemoveEmptyEntries))
                            |> Seq.takeWhile (fun d -> d <> "Time Period")
                            |> Array.ofSeq

            let observations = lines
                                |> Seq.skip (x.headerRowCount + 1)
                                |> Seq.map (fun o -> o.Split([|"\",\""|], headers.Length + 1, System.StringSplitOptions.RemoveEmptyEntries))
                                |> Array.ofSeq
                
            let dimensions = [1 .. headers.Length - 1]
                                |> Seq.mapi (fun i d -> observations
                                                            |> Seq.map (fun o -> Array.get o i)
                                                            |> Seq.distinct
                                                            |> Array.ofSeq)
                                |> Seq.mapi (fun i d -> new Dimension ((Array.get headers i), d))
                                |> Array.ofSeq
                
            new Dataset(dimensions)


    member x.filter (obsFilter:Dictionary<string, string list>) =
            let lines = File.ReadLines(filePath)

            let headers = lines
                            |> Seq.skip x.headerRowCount
                            |> Seq.take 1 
                            |> Seq.collect (fun h -> h.Split ([|"\",\""|], System.StringSplitOptions.RemoveEmptyEntries))
                            |> Seq.takeWhile (fun d -> d <> "Time Period")
                            |> Seq.toArray

            let dims =  headers
                        |> Seq.mapi (fun i d -> { dim = d; pos = i; members = if obsFilter.ContainsKey(d) then obsFilter.[d] else [] })
                        |> Seq.filter (fun f -> f.members.Length > 0)
                        |> Seq.toArray

            let headerCount = headers.Count()

            let filtered = lines
                                |> Seq.skip (x.headerRowCount + 1)
                                |> Seq.map (fun o -> o.Split([|"\",\""|], System.StringSplitOptions.RemoveEmptyEntries))
                                |> Seq.filter (fun o -> let obsArr = o.[headerCount].Split ([|':'|], System.StringSplitOptions.RemoveEmptyEntries)
                                                        dims
                                                            |> Seq.filter (fun flt -> flt.members.Contains(obsArr.[flt.pos]))
                                                            |> Seq.length = dims.Count()
                                                )
                                |> Seq.map (fun o -> o.Skip(headerCount).ToArray())
                                |> List.ofSeq

            filtered

type public CbsParser(filePath) =
    inherit Parser(filePath)
    override this.headerRowCount = 8

type public LbsParser(filePath) =
    inherit Parser(filePath)
    override this.headerRowCount = 7