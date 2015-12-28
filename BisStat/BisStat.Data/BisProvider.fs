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
    
    let datasetProvider = ProvidedTypeDefinition(asm, ns, "Dataset", Some typeof<obj>)
           
    do datasetProvider.DefineStaticParameters([ProvidedStaticParameter("pathToBisFile", typeof<string>)], fun typeName args ->
        let pathToDatasetFile = args.[0] :?> string

        let parser = createPraser pathToDatasetFile

        let dimensionTypes =  
            let dimesionTys = 
                parser.getDataset().dimensions
                    |> Seq.map (fun d -> 
                                    let p = ProvidedTypeDefinition(d.name, Some typeof<obj>, HideObjectMethods = true)
                                    d.members
                                        |> Seq.map (fun m -> ProvidedLiteralField(m, typeof<string>, m.Substring(0, m.IndexOf(':'))))
                                        |> Seq.iter (fun m -> p.AddMember(m))
                                    p)
                    |> Seq.toList

            dimesionTys

        let filterProvider =
            let set = parser.getDataset()

            // Observation filter type
            let filterTy = ProvidedTypeDefinition("ObservationFilter", Some typeof<obj>, HideObjectMethods = true)
            filterTy.AddMember <| ProvidedConstructor(parameters = [], InvokeCode = fun args -> <@@ new Dictionary<string, string list>() @@>)

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
                |> filterTy.AddMembers

            // Get whole filter definition
            let getFilterMeth = ProvidedMethod("Get", [], typeof<Dictionary<string,string list>>)
            getFilterMeth.InvokeCode <- (fun args -> 
                                <@@ 
                                    let dict = ((%%args.[0] : obj) :?> System.Collections.Generic.Dictionary<string,string list>)
                                    let ret = new Dictionary<string, string list>()
                                    for f in dict.Where((fun d -> d.Value.Length > 0)) do
                                        ret.Add(f.Key, f.Value)
                                    ret
                                 @@>)
            filterTy.AddMember (getFilterMeth)
            filterTy

        
        let provider = ProvidedTypeDefinition(asm, ns, typeName, Some typeof<obj>)

        provider.AddMember <| ProvidedConstructor(parameters = [], InvokeCode = fun args -> <@@ new Dictionary<string, string list>() @@>)

        let filePath = args.[0] :?> string
        // Apply filter on statistics file
        provider.AddMember <| ProvidedMethod("Filter",
                                       [ProvidedParameter("obsFilter", typeof<Dictionary<string,string list>>)], 
                                       typeof<Observation list>, 
                                       InvokeCode = (fun [me; obsFilter] -> 
                                                            <@@
                                                                 let fileParser = createPraser pathToDatasetFile
                                                                fileParser.filter ((%%obsFilter : Dictionary<string, string list>))
                                                            @@>))
        
        provider.AddMembers(dimensionTypes.Union([filterProvider]) |> Seq.toList)

        provider)

    do this.AddNamespace(ns, [datasetProvider])    

[<assembly:TypeProviderAssembly>]
do ()