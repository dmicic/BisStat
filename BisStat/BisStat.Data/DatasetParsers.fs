namespace BisStat.Data.Parser

open System.Collections.Generic
open System.IO
open System.Linq

open FSharp.Data

// Representation of a dataset dimension
type public Dimension(dimensionName: string, position: int, memberList: string[]) =  
    class
        member this.name = dimensionName
        member this.position = position
        member this.members = memberList
    end

// Representation of a dataset
type Dataset(dimensions: Dimension[]) =
    class 
        member this.dimensions = dimensions
    end
       
// representation of a filter 
type ObservationFilter(dimension : string, dimensionPosition : int, memberFilter : option<string list>) = 
    class
        member this.dimension = dimension
        member this.dimensionPosition = dimensionPosition
        member this.memberFilter = memberFilter
    end
        
// Base class for parsers
[<AbstractClass>]
type public Parser(filePath: string) = 
        
    abstract member headerRowCount : int

    member val dataset : option<Dataset> = None with get, set

    // Split string with comma separated dimensions to array
    member x.splitDimensions (dimensions : string, ?startPosition : int) = 
        let sep = [|"\",\""|]
        let opt = System.StringSplitOptions.RemoveEmptyEntries

        if startPosition.IsSome then
            dimensions.Split (sep, startPosition.Value, opt)
        else
            dimensions.Split (sep, opt)

    member x.splitObservation (obs : string) =
        obs.Split ([|':'|], System.StringSplitOptions.RemoveEmptyEntries)

    member x.isTimePeriodColumn (column : string) =
        column = "Time Period"

    // Get dataset including the dimesions and the related memebers
    member x.getDataset = 
            match x.dataset with
                | Some d -> d
                | None ->   let lines = File.ReadLines(filePath)
            
                            let dimNames = 
                                x.splitDimensions <| lines.Skip(x.headerRowCount).First()
                                    |> Seq.takeWhile (fun d -> not (x.isTimePeriodColumn d))
                                    |> Array.ofSeq

                            let observations = 
                                lines
                                    |> Seq.skip (x.headerRowCount + 1)
                                    |> Seq.map (fun o -> x.splitDimensions(o, dimNames.Length + 1))
                                    |> Array.ofSeq
                
                            let dimensions = 
                                [1 .. dimNames.Length]
                                    |> Seq.mapi 
                                        (fun i d -> 
                                            observations
                                                |> Seq.map (fun obs -> Array.get obs i)
                                                |> Seq.distinct
                                                |> Array.ofSeq)
                                    |> Seq.mapi (fun i dim -> new Dimension ((Array.get dimNames i), i, dim))
                                    |> Array.ofSeq
               
                            x.dataset <- Some(new Dataset(dimensions))
                            x.dataset.Value

    // Retrieve observations based on observation code part filter
    member x.filter (obsFilter : Dictionary<string, string list>) =
            
            let dataset = x.getDataset
            
            let filterDims = 
                dataset.dimensions
                    |> Seq.map (fun dim -> new ObservationFilter(dim.name, dim.position, if obsFilter.ContainsKey(dim.name) then Some(obsFilter.[dim.name]) else None))
                    |> Seq.filter (fun f -> f.memberFilter.IsSome)
                    |> Seq.toArray

            let headerCount = dataset.dimensions.Count()

            let filtered = 
                File.ReadAllLines(filePath)
                    |> Seq.skip (x.headerRowCount + 1)
                    |> Seq.map x.splitDimensions
                    |> Seq.filter (fun o -> let obs = x.splitObservation(o.[headerCount])
                                            filterDims
                                                |> Seq.filter (fun obsFilter -> obsFilter.memberFilter.Value.Contains(obs.[obsFilter.dimensionPosition]))
                                                |> Seq.length = filterDims.Count())
                    |> Seq.map (fun o -> o.Skip(headerCount).ToArray())
                    |> List.ofSeq

            filtered

// CBS specific parser
type public CbsParser(filePath) =
    inherit Parser(filePath)
    override this.headerRowCount = 8

// LBS specific parser
type public LbsParser(filePath) =
    inherit Parser(filePath)
    override this.headerRowCount = 7