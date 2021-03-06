﻿namespace BisStat.Data

open System.Collections.Generic
open System.IO
open System.Linq

open Microsoft.FSharp.Core.Operators

open FSharp.Data

module Parser =

    // Representation of a dataset dimension
    type public Dimension(dimensionName: string, position: int, memberList: string[]) =  
        class
            member this.name = dimensionName
            member this.position = position
            member this.members = memberList
        end

    // Representation of a dataset
    type Dataset(dimensions: Dimension[], periods: string[]) =
        class 
            member this.dimensions = dimensions
            member this.periods = periods
        end
       
    // representation of a filter 
    type ObservationFilter(dimension : string, dimensionPosition : int, memberFilter : option<string list>) = 
        class
            member this.dimension = dimension
            member this.dimensionPosition = dimensionPosition
            member this.memberFilter = memberFilter
        end

    // Representation of an observation an values per period
    type Observation(key : string, values : Map<string, option<float>>) =
        class
            member this.key = key
            member this.values = values
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

        member x.skipHeader (reader:StreamReader) =
            [1 .. x.headerRowCount] |> List.iter (fun i -> reader.ReadLine() |> ignore)

        member x.getHeader () =
            use reader = new StreamReader(filePath)
            x.skipHeader reader
            
            x.splitDimensions(reader.ReadLine()) 
                |> Seq.takeWhile (fun d -> not (x.isTimePeriodColumn d))
                |> Seq.map (fun d -> d.Replace("\"", ""))
                |> Seq.toArray

        member x.getPeriods () = 
            use reader = new StreamReader(filePath)
            x.skipHeader reader

            x.splitDimensions(reader.ReadLine())
                |> Seq.skipWhile (fun d -> not (x.isTimePeriodColumn d))
                |> Seq.skip 1
                |> Seq.map (fun d -> d.Replace("\"", ""))
                |> Array.ofSeq

        // Get dataset including the dimesions and the related memebers
        member x.getDataset () = 
                match x.dataset with
                    | Some d -> d
                    | None ->   let lines = File.ReadLines(filePath)
            
                                let dimNames = x.getHeader()

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
               
                                x.dataset <- Some(new Dataset(dimensions, x.getPeriods()))
                                x.dataset.Value

        // Retrieve observations based on observation code part filter
        member x.filter (obsFilter : Dictionary<string, string list>) =
            let dimensions = 
                x.getHeader()
                    |> Seq.mapi (fun i dim -> new Dimension(dim, i, Array.empty))
                
            
            let filterDims = 
                dimensions
                    |> Seq.map (fun dim -> new ObservationFilter(dim.name, dim.position, if obsFilter.ContainsKey(dim.name) then Some(obsFilter.[dim.name]) else None))
                    |> Seq.filter (fun f -> f.memberFilter.IsSome)
                    |> Seq.toArray

            let headerCount = dimensions.Count()

            let filtered = 
                File.ReadAllLines(filePath)
                    |> Seq.skip (x.headerRowCount + 1)
                    |> Seq.map x.splitDimensions
                    |> Seq.filter (fun o -> let obs = x.splitObservation(o.[headerCount])
                                            filterDims
                                                |> Seq.filter (fun obsFilter -> obsFilter.memberFilter.Value.Contains(obs.[obsFilter.dimensionPosition]))
                                                |> Seq.length = filterDims.Count())
                    |> Seq.map (fun o -> o.Skip(headerCount).ToArray())
                    |> Seq.map (fun o -> o.[o.Length - 1] <- o.[o.Length - 1].Replace("\"",System.String.Empty)
                                         new Observation(o.[0], 
                                            new Map<string, option<float>>(
                                                x.getPeriods() 
                                                    |> Seq.mapi (fun i period -> 
                                                                       period, (if i+1 < o.Length && not (System.String.IsNullOrWhiteSpace o.[i+1]) then 
                                                                                   Some(System.Convert.ToDouble(o.[i+1])) 
                                                                                else 
                                                                                   None)))))
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

    // Property prices long
    type public PpLongParser(filePath) =
        inherit Parser(filePath)
        override this.headerRowCount = 6

    // Property prices selected
    type public PpSelectedParser(filePath) =
        inherit Parser(filePath)
        override this.headerRowCount = 5

    // Debt. securities
    type public DebtSecurityParser(filePath) =
        inherit Parser(filePath)
        override this.headerRowCount = 10

    // Effective exchange rates
    type public EffectiveExchangeRatesParser(filePath) =
        inherit Parser(filePath)
        override this.headerRowCount = 4

    // Credit to non-financial sector
    type public CreditNonFinancialSectorParser(filePath) =
        inherit Parser(filePath)
        override this.headerRowCount = 5

    // Debt service ratios for the private non-financial sector
    type public DebtServiceRatioParser(filePath) =
        inherit Parser(filePath)
        override this.headerRowCount = 7

    // Parser factory
    let createPraser pathToDatasetFile =
        match Path.GetFileName(pathToDatasetFile).ToLower() with
            | ds when ds.Contains("_cbs_") -> new CbsParser(pathToDatasetFile) :> Parser
            | ds when ds.Contains("_lbs_") -> new LbsParser(pathToDatasetFile) :> Parser
            | ds when ds.Contains("_long_pp_") -> new PpLongParser(pathToDatasetFile) :> Parser
            | ds when ds.Contains("_selected_pp_") -> new PpSelectedParser(pathToDatasetFile) :> Parser
            | ds when ds.Contains("_debt_sec2_") -> new DebtSecurityParser(pathToDatasetFile) :> Parser
            | ds when ds.Contains("_eer_") -> new EffectiveExchangeRatesParser(pathToDatasetFile) :> Parser
            | ds when ds.Contains("_total_credit_") -> new CreditNonFinancialSectorParser(pathToDatasetFile) :> Parser
            | ds when ds.Contains("_dsr_") -> new DebtServiceRatioParser(pathToDatasetFile) :> Parser
            | _ -> failwith("Dataset not yet supported. File: " + pathToDatasetFile)