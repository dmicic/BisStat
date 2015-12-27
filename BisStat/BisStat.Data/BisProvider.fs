namespace Bis.Data.Provider

open System.IO
open System.Linq
open System.Reflection

open Microsoft.FSharp.Core.CompilerServices
open ProviderImplementation.ProvidedTypes

open Microsoft.FSharp.Quotations

open System.Collections.Generic

open BisStat.Data.Parser

[<TypeProvider>]
type public DatasetProvider(cfg:TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces()

    let ns = "Bis"
    let asm = Assembly.GetExecutingAssembly()
    
    let mutable pathToDatasetFile = @"C:/Users/Darko/Desktop/full_BIS_CBS_csv.csv"
    let parameters = [ProvidedStaticParameter("PathToDatasetFile", typeof<string>)]
    let mutable parserInstance : option<Parser> = None

    let datasetProvider = 
        let dataset = ProvidedTypeDefinition(asm, ns, "Dataset", Some typeof<obj>)

        dataset.AddMember <| ProvidedConstructor(parameters = [], InvokeCode = fun args -> <@@ new Dictionary<string, string list>() @@>)

        // Apply filter on statistics file
        dataset.AddMember <| ProvidedMethod("Filter",
                                       [ProvidedParameter("obsFilter", typeof<Dictionary<string,string list>>)], 
                                       typeof<Observation list>, 
                                       InvokeCode = (fun [me; obsFilter] -> 
                                                            <@@
                                                                let parser = new CbsParser(@"C:/Users/Darko/Desktop/full_BIS_CBS_csv.csv")
                                                                parser.filter ((%%obsFilter : Dictionary<string, string list>))
                                                            @@>))
        dataset
    
    let parser =
        match parserInstance with
            | (Some d) -> d
            | None -> parserInstance <-
                        match Path.GetFileName(pathToDatasetFile).ToLower() with
                            | ds when ds.Contains("_cbs_") -> Some(new CbsParser(pathToDatasetFile) :> Parser)
                            | ds when ds.Contains("_lbs_") -> Some(new LbsParser(pathToDatasetFile) :> Parser)
                      parserInstance.Value

    // Generate type per dimension
    let dimensionTypes =  
        let providers = 
            parser.getDataset.dimensions
                |> Seq.map (fun d -> 
                                let p = ProvidedTypeDefinition(asm, ns, d.name, Some typeof<obj>)
                                d.members
                                    |> Seq.map (fun m -> ProvidedLiteralField(m, typeof<string>, m.Substring(0, m.IndexOf(':'))))
                                    |> Seq.iter (fun m -> p.AddMember(m))
                                p)
                |> Seq.toList

        providers

    let filterProvider =
        let set = parser.getDataset

        // Observation filter type
        let fp = ProvidedTypeDefinition(asm, ns, "Filter", Some typeof<obj>, HideObjectMethods = true)
        fp.AddMember <| ProvidedConstructor(parameters = [], InvokeCode = fun args -> <@@ new Dictionary<string, string list>() @@>)

        // Generate property per dataset dimension
        set.dimensions.Select(fun x -> x.name)
            |> Seq.map (fun d -> 
                            ProvidedProperty (
                                d, 
                                typeof<string list>, 
                                IsStatic = false, 
                                GetterCode = (fun args -> 
                                                <@@ let dict = ((%%args.[0] : obj) :?> Dictionary<string,string list>)
                                                    if not (dict.ContainsKey d) then dict.Add (d, [])
                                                    dict.[d]
                                                 @@>),
                                SetterCode = (fun args -> 
                                                <@@ ((%%args.[0] : obj) :?>Dictionary<string,string list>).[d] <- (%%args.[1] : string list) @@>)))
            |> Seq.toList
            |> fp.AddMembers

        // Get whole filter definition
        let fg = ProvidedMethod("Get", [], typeof<Dictionary<string,string list>>)
        fg.InvokeCode <- (fun args -> 
                            <@@ 
                                let dict = ((%%args.[0] : obj) :?> System.Collections.Generic.Dictionary<string,string list>)
                                let ret = new Dictionary<string, string list>()
                                for f in dict.Where((fun d -> d.Value.Length > 0)) do
                                    ret.Add(f.Key, f.Value)
                                ret
                             @@>)
        fp.AddMember (fg)
        fp

    do this.AddNamespace(ns, dimensionTypes.Union([datasetProvider;filterProvider]) |> Seq.toList)    

[<assembly:TypeProviderAssembly>]
do ()