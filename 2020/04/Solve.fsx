#r "nuget: FParsec"

open System.IO
open FParsec

[<AutoOpen>]
module Common =

    // TODO: expand if necessary
    type Color = string
    type Year = string
    type Length = string

    type FieldKind =
        | Year of Year
        | Color of Color
        | Length of Length
        | Other of string

    type NorthPoleCredential =
        {
            BirthYear: Year
            IssueYear: Year
            ExpirationYear: Year
            Height: Length
            HairColor: Color
            EyeColor: Color
            PassportID: string
        }

    type Passport =
        {
            BirthYear: Year
            IssueYear: Year
            ExpirationYear: Year
            Height: Length
            HairColor: Color
            EyeColor: Color
            PassportID: string
            CountryID : string
        }

    type TravelDoc =
        | Npc of NorthPoleCredential
        | Pass of Passport
        | Invalid

    let private getYear = function
        | Year year -> Some year
        | _ -> None

    let private getLength = function
        | Length length -> Some length
        | _ -> None

    let private getColor = function
        | Color color -> Some color
        | _ -> None

    let private getOther = function
        | Other text -> Some text
        | _ -> None

    let private fieldsToNpc fields =
        Map.tryFind "byr" fields |> Option.bind getYear   |> Option.bind (fun byr ->
        Map.tryFind "iyr" fields |> Option.bind getYear   |> Option.bind (fun iyr ->
        Map.tryFind "eyr" fields |> Option.bind getYear   |> Option.bind (fun eyr ->
        Map.tryFind "hgt" fields |> Option.bind getLength |> Option.bind (fun hgt ->
        Map.tryFind "hcl" fields |> Option.bind getColor  |> Option.bind (fun hcl ->
        Map.tryFind "ecl" fields |> Option.bind getColor  |> Option.bind (fun ecl ->
        Map.tryFind "pid" fields |> Option.bind getOther  |> Option.bind (fun pid ->
        Some <| Npc {
            BirthYear = byr
            IssueYear = iyr
            ExpirationYear = eyr
            Height = hgt
            HairColor = hcl
            EyeColor = ecl
            PassportID = pid
        })))))))

    let private fieldsToPassport fields =
        Map.tryFind "byr" fields |> Option.bind getYear   |> Option.bind (fun byr ->
        Map.tryFind "iyr" fields |> Option.bind getYear   |> Option.bind (fun iyr ->
        Map.tryFind "eyr" fields |> Option.bind getYear   |> Option.bind (fun eyr ->
        Map.tryFind "hgt" fields |> Option.bind getLength |> Option.bind (fun hgt ->
        Map.tryFind "hcl" fields |> Option.bind getColor  |> Option.bind (fun hcl ->
        Map.tryFind "ecl" fields |> Option.bind getColor  |> Option.bind (fun ecl ->
        Map.tryFind "pid" fields |> Option.bind getOther  |> Option.bind (fun pid ->
        Map.tryFind "cid" fields |> Option.bind getOther  |> Option.bind (fun cid ->
        Some <| Pass {
            BirthYear = byr
            IssueYear = iyr
            ExpirationYear = eyr
            Height = hgt
            HairColor = hcl
            EyeColor = ecl
            PassportID = pid
            CountryID = cid
        }))))))))

    let parseText: Parser<string, unit> =
        let isWhitespace c = c = ' ' || c = '\n' // different newlines are converted
        many1Satisfy (isWhitespace >> not)

    let parseYear: Parser<Year, unit> = parseText
    let parseColor: Parser<Color, unit> = parseText
    let parseLength: Parser<Length, unit> = parseText

    let parseField: Parser<(string * FieldKind), unit> =
        let parseTag =
            ["byr"; "iyr"; "eyr"; "hgt"
             "hcl"; "ecl"; "pid"; "cid"]
            |> List.map pstring
            |> choice
        let wrap tag value = (tag, value)
        parseTag .>> skipChar ':' >>= fun tag ->
        match tag with
        | "byr" | "iyr" | "eyr" -> parseYear |>> (FieldKind.Year >> wrap tag)
        | "hgt" -> parseLength |>> (FieldKind.Length >> wrap tag)
        | "hcl" | "ecl" -> parseColor |>> (FieldKind.Color >> wrap tag)
        | "pid" | "cid" -> parseText |>> (FieldKind.Other >> wrap tag)
        | _ -> failwith $"Unknown tag: {tag}" // FIXME: Sum type

    let parseTravelDoc: Parser<TravelDoc, unit> =
        let parseDocSeparator = skipChar ' ' <|> skipNewline
        let fieldsToDoc fields =
            let doc =
                if Map.containsKey "cid" fields
                then fieldsToPassport fields
                else fieldsToNpc fields
            match doc with
            | Some traveldoc -> traveldoc
            | None -> Invalid
        sepEndBy parseField parseDocSeparator |>> (Map<_, _> >> fieldsToDoc)

    let parseAllDocs: Parser<TravelDoc list, unit> =
        let pDocSeparator = skipNewline
        sepBy parseTravelDoc pDocSeparator

    let onDocuments f =
        let encoding = new System.Text.UTF8Encoding ()
        runParserOnFile (parseAllDocs |>> f) () "input.txt" encoding

    let parseHeight: Parser<(uint16 * string), unit> =
        let pNum = puint16
        let pMeasure = pstring "cm" <|> pstring "in"
        let merge a b = (a, b)
        let pHeight = pipe2 pNum pMeasure merge
        pHeight .>> eof

    let parseHairColor: Parser<string, unit> =
        let toArr = pchar '#' >>. parray 6 (anyOf "0123456789abcdef") .>> eof
        toArr |>> System.String.Concat

    let parseEyeColor: Parser<string, unit> =
        let pEcl =
            [ "amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth" ]
            |> List.map pstring |> choice
        pEcl .>> eof

    let parsePassportId: Parser<string, unit> =
        let pPid = parray 9 (satisfy isDigit) |>> System.String.Concat
        pPid .>> eof

    let validateNpc byr iyr eyr hgt hcl ecl pid =
        let numberBetween (min, max) num =
            try
                System.Int32.Parse num |> fun num -> num >= min && num <= max
            with
            | _ -> false
        let validHeight height =
            runParserOnString parseHeight () "height" height
            |> function
                | Success ((size, measure) , _, _) ->
                    match measure with
                    | "in" -> size >= 59us  && size <= 76us
                    | "cm" -> size >= 150us && size <= 193us
                    | _ -> false
                | Failure _ -> false
        let validHairColor color =
            runParserOnString parseHairColor () "hairColor" color
            |> function
                | Success _ -> true
                | Failure _ -> false
        let validEyeColor color =
            runParserOnString parseEyeColor () "eyeColor" color
            |> function
                | Success _ -> true
                | Failure _ -> false
        let validPassportId pid =
            runParserOnString parsePassportId () "passportID" pid
            |> function
                | Success _ -> true
                | Failure _ -> false
        numberBetween (1920, 2002) byr &&
        numberBetween (2010, 2020) iyr &&
        numberBetween (2020, 2030) eyr &&
        validHeight hgt &&
        validHairColor hcl &&
        validEyeColor ecl &&
        validPassportId pid


    let validateDocument = function
        | Pass { BirthYear = byr; IssueYear = iyr; ExpirationYear = eyr
                 Height = hgt; HairColor = hcl; EyeColor = ecl; PassportID = pid } ->
                 validateNpc byr iyr eyr hgt hcl ecl pid
        | Npc  { BirthYear = byr; IssueYear = iyr; ExpirationYear = eyr
                 Height = hgt; HairColor = hcl; EyeColor = ecl; PassportID = pid } ->
                 validateNpc byr iyr eyr hgt hcl ecl pid
        | other -> false

    let solve1 () =
        onDocuments <| List.countBy (fun doc -> doc = Invalid)

    let solve2 () =
        let count doc = validateDocument doc
        onDocuments <| List.countBy count
