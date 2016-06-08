let inline isCharAllowed c =
    Char.IsLetterOrDigit c || Char.IsWhiteSpace c || c = '-'
    
let sanitizeText (text: string) =
    let cleanChars = text |> Seq.filter isCharAllowed |> Seq.toArray
    new String(cleanChars)
