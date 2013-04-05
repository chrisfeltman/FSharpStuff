namespace ISO15434Parser

open System
module Parser = 

    type internal ISO15434Format = 
        | FormatAI 
        | FormatDI 
        | FormatTEI 

    type public IssuingAgency = 
        | CAGE
        | DUNS
        | GS1
        | DODAAC
        | OA

    type internal UiiRec =  
        { 
            Format : ISO15434Format;
            Uii : string * string;
            EnterpriseId : string * string; 
            OriginalPartNumber : string; 
            LotBatchNumber : string; 
            SerialNumber : string; 
            CurrentPartNumber : string; 
        }

    // ISO special characters
    let public RS =  "\x1e"
    let public GS =  "\x1d"
    let public EOT = "\x04"
    let public ISOPreamble = "[)>"

    // format tags for ISO 15434
    let internal UiiCodes =  [ "8002"; "8003";"8004";"25S"; "I"; "22S";"18S"; "UID "; "USN "; "UST "; ] 
    let internal EiCodes = [ "95"; "17V"; "12V"; "3V"; "7L"; "18V"; "CAG "; "MFR "; "SPL "; "DUN "; "EUC ";  ]
    let internal OriginalPartNumbers =  [ "01"; "1P"; "PNO ";]
    let internal LotOrBatchNumbers =  [ "10"; "1T"; "LOT "; "LTN "; "BII "; ]
    let internal SerialNumbers =  [ "21"; "SEQ "; "SER "; "UCN "; "S";]  // S must be last because of possible bogus match on SEQ and SER when using StartsWith()
    let internal CurrentPartNumbers = [ "240"; "30P"; "PNR ";]

    let internal ReturnFirstOrNone (theList :  'a list) = 
        match theList.IsEmpty with
            | true-> None
            | false -> Some(theList.Head)

    let internal ExtractTaggedValue (tag : string) (values : string list) =    
        [
            for a in values do
                if a.StartsWith(tag) then
                    yield a.Substring(tag.Length), tag
        ]  
        |> ReturnFirstOrNone
              
    let internal GetTaggedValue tags values = 
        [
            for a in tags do
                yield  ExtractTaggedValue a values
        ] 
        |> List.choose id  |> ReturnFirstOrNone // filter out fields that are None     

    let internal SplitOn (stringToSplit : string) (specialChar : string) = 
        stringToSplit.Split([|specialChar|], StringSplitOptions.RemoveEmptyEntries)    

    let internal Parse (barcode : string) = 
        let recordsArray = barcode |> SplitOn RS

        if recordsArray.[0] <> ISOPreamble then
            failwith "Bad ISO 15434 Header"
   
        let valuesArray = recordsArray.[1] |> SplitOn GS

        let format = 
            match valuesArray.[0] with
                | "05" -> FormatAI 
                | "06" -> FormatDI 
                | "DD" -> FormatTEI
                | "12" -> FormatTEI
                | _    -> failwith "Bad ISO 15434 Format"

        let fields = Array.toList valuesArray.[1..]

        let MatchSingle tags values = 
            match GetTaggedValue tags values with
                | Some(x) -> fst x
                | None -> String.Empty    

        let MatchTuple tags values = 
            match GetTaggedValue tags values with
                | Some(x) -> x
                | None -> String.Empty, String.Empty

        let uii = MatchTuple UiiCodes fields      
        let  ei = MatchTuple EiCodes fields
        let opn = MatchSingle OriginalPartNumbers fields
        let lbn = MatchSingle LotOrBatchNumbers fields
        let ser = MatchSingle SerialNumbers fields
        let cur = MatchSingle CurrentPartNumbers fields

        {Format = format; Uii = uii; EnterpriseId = ei; OriginalPartNumber = opn; LotBatchNumber = lbn; SerialNumber = ser; CurrentPartNumber = cur}

   
    let internal GetPrefix  (aRec : UiiRec) = 

        let DPrefixes = ["18S"; "17V"; "MFR "; "CAG "; "USN "; "UST "; "SPL ";]
        let UNPrefixes = ["12V"; "DUN ";]
        let uiiCode, eiCode = snd aRec.Uii, snd aRec.EnterpriseId

        if List.exists (fun x-> x = uiiCode || x = eiCode) DPrefixes then
            "D"
        elif eiCode = "7L" then
            "LD"
        elif List.exists (fun x -> x = eiCode ) UNPrefixes then 
            "UN"
        else
            ""

    let internal ConcatUii (aRec : UiiRec) = 
        match aRec.Format with
            | FormatAI -> 
                match fst aRec.EnterpriseId with
                    | "" when aRec.OriginalPartNumber <> "" && aRec.SerialNumber <> "" 
                            -> aRec.OriginalPartNumber + aRec.SerialNumber

                    | _  when fst aRec.EnterpriseId <> "" && aRec.LotBatchNumber <> "" && aRec.SerialNumber <> "" 
                            -> fst aRec.EnterpriseId + aRec.LotBatchNumber + aRec.SerialNumber 

                    | _     
                            -> failwith "Missing fields required to create the UII"
                 
            | _ -> 
                match aRec.OriginalPartNumber with
                    | "" when fst aRec.EnterpriseId <> "" && aRec.LotBatchNumber <> "" && aRec.SerialNumber <> "" 
                            -> GetPrefix aRec + fst aRec.EnterpriseId + aRec.LotBatchNumber + aRec.SerialNumber

                    | _  when fst aRec.EnterpriseId <> "" && aRec.SerialNumber <> "" 
                            -> GetPrefix aRec +  fst aRec.EnterpriseId + aRec.OriginalPartNumber + aRec.SerialNumber
                    | _ 
                            -> failwith "Missing fields required to create the UII" 
            
    let internal GetUii aRec  = 
        match  aRec.Uii with
        | "", "" -> ConcatUii aRec
        | _ , _  -> GetPrefix aRec + fst aRec.Uii


    type public ParserObject() = 
        let noRecordMsg = "You must call Parse before reading field values"
        let mutable (uiRec : UiiRec option) = None

        let GetFieldWithRecordCheck fieldGetFunc = 
            match uiRec with
                 | None -> failwith noRecordMsg
                 | Some(x) ->  fieldGetFunc
                
        member this.Parse(barcode : string) =   
            uiRec <-  Some(Parse barcode)
            ()

        member this.Uii                 with get() = GetFieldWithRecordCheck  GetUii uiRec.Value       
        member this.EnterpriseId        with get() = GetFieldWithRecordCheck  fst uiRec.Value.EnterpriseId              
        member this.OriginalPartNumber  with get() = GetFieldWithRecordCheck  uiRec.Value.OriginalPartNumber                 
        member this.LotBatchNumber      with get() = GetFieldWithRecordCheck  uiRec.Value.LotBatchNumber                    
        member this.SerialNumber        with get() = GetFieldWithRecordCheck  uiRec.Value.SerialNumber             
        member this.CurrentPartNumber   with get() = GetFieldWithRecordCheck  uiRec.Value.CurrentPartNumber
               