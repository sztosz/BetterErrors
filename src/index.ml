open BetterErrorsTypes
open Helpers
let normalizeCompilerLineColsToRange ~fileLines  ~lineRaw  ~col1Raw  ~col2Raw
   =
  let line = int_of_string lineRaw in
  let fileLength = List.length fileLines in
  let isOCamlBeingBadAndPointingToALineBeyondFileLength = line > fileLength in
  let (col1,col2) =
    if isOCamlBeingBadAndPointingToALineBeyondFileLength
    then
      let lastDamnReachableSpotInTheFile =
        String.length @@ (List.nth fileLines (fileLength - 1)) in
      ((lastDamnReachableSpotInTheFile - 1), lastDamnReachableSpotInTheFile)
    else
      (match (col1Raw, col2Raw) with
       | (((Some (a))[@explicit_arity ]),((Some (b))[@explicit_arity ])) ->
           ((int_of_string a), (int_of_string b))
       | _ -> (0, 0)) in
  let startRow =
    if isOCamlBeingBadAndPointingToALineBeyondFileLength
    then fileLength - 1
    else line - 1 in
  let currentLine = List.nth fileLines startRow in
  let numberOfCharsBetweenStartAndEndColumn = col2 - col1 in
  let numberOfCharsLeftToCoverOnStartingRow =
    ((String.length currentLine) - col1) + 1 in
  if
    numberOfCharsBetweenStartAndEndColumn <=
      numberOfCharsLeftToCoverOnStartingRow
  then ((startRow, col1), (startRow, col2))
  else
    (let howManyCharsLeftToCoverOnSubsequentLines =
       ref
         (numberOfCharsBetweenStartAndEndColumn -
            numberOfCharsLeftToCoverOnStartingRow) in
     let suddenlyFunctionalProgrammingOutOfNowhere =
       ((fileLines |> (Helpers.listDrop (startRow + 1))) |>
          (List.map String.length))
         |>
         (Helpers.listTakeWhile
            (fun numberOfCharsOnThisLine  ->
               if
                 (!howManyCharsLeftToCoverOnSubsequentLines) >
                   numberOfCharsOnThisLine
               then
                 (howManyCharsLeftToCoverOnSubsequentLines :=
                    (((!howManyCharsLeftToCoverOnSubsequentLines) -
                        numberOfCharsOnThisLine)
                       - 1);
                  true)
               else false)) in
     let howManyMoreRowsCoveredSinceStartRow =
       1 + (List.length suddenlyFunctionalProgrammingOutOfNowhere) in
     ((startRow, col1),
       ((startRow + howManyMoreRowsCoveredSinceStartRow),
         (!howManyCharsLeftToCoverOnSubsequentLines))))
let extractFromFileMatch fileMatch =
  let open Re_pcre in
    match fileMatch with
    | (Delim _)::((Group (_,filePath))[@implicit_arity ])::((Group
        (_,lineNum))[@implicit_arity ])::col1::col2::((Text
        (body))[@explicit_arity ])::[] ->
        let cachedContent = Helpers.fileLinesOfExn filePath in
        let (col1Raw,col2Raw) =
          match (col1, col2) with
          | (((Group (_,c1))[@implicit_arity ]),((Group
             (_,c2))[@implicit_arity ])) ->
              if ((String.trim c1) = "") || ((String.trim c2) = "")
              then (None, None)
              else ((Some c1), (Some c2))
          | _ -> (None, None) in
        (filePath, cachedContent,
          (normalizeCompilerLineColsToRange ~fileLines:cachedContent
             ~lineRaw:lineNum ~col1Raw ~col2Raw), (String.trim body))
    | _ -> raise (invalid_arg "Couldn't extract error")
let printFullSplitResult =
  List.iteri
    (fun i  ->
       fun x  ->
         print_int i;
         print_endline "";
         let open Re_pcre in
           (match x with
            | ((Delim (a))[@explicit_arity ]) ->
                print_endline @@ ("Delim " ^ a)
            | ((Group (_,a))[@implicit_arity ]) ->
                print_endline @@ ("Group " ^ a)
            | ((Text (a))[@explicit_arity ]) ->
                print_endline @@ ("Text " ^ a)
            | NoGroup  -> print_endline @@ "NoGroup"))
let fileR =
  Re_pcre.regexp ~flags:[let open Re_pcre in `MULTILINE]
    {|^File "([\s\S]+?)", line (\d+)(?:, characters (\d+)-(\d+))?:$|}
let hasErrorOrWarningR =
  Re_pcre.regexp ~flags:[let open Re_pcre in `MULTILINE]
    {|^(Error|Warning \d+): |}
let hasIndentationR =
  Re_pcre.regexp ~flags:[let open Re_pcre in `MULTILINE] {|^       +|}
let hasHintRStr = {|^Hint: Did you mean |}
let hasHintR =
  Re_pcre.regexp ~flags:[let open Re_pcre in `MULTILINE] hasHintRStr
let parse ~customErrorParsers  err =
  let err = String.trim err in
  try
    let open Re_pcre in
      match full_split ~rex:fileR err with
      | (Delim _)::((Group (_,filePath))[@implicit_arity ])::((Group
          (_,lineNum))[@implicit_arity ])::col1::col2::((Text
          (body))[@explicit_arity ])::[] ->
          let body = String.trim body in
          let errorCapture = get_match_maybe {|^Error: ([\s\S]+)|} body in
          (match ParseError.specialParserThatChecksWhetherFileEvenExists
                   filePath errorCapture
           with
           | ((Some (err))[@explicit_arity ]) -> err
           | None  ->
               let cachedContent = Helpers.fileLinesOfExn filePath in
               let (col1Raw,col2Raw) =
                 match (col1, col2) with
                 | (((Group (_,c1))[@implicit_arity ]),((Group
                    (_,c2))[@implicit_arity ])) ->
                     if ((String.trim c1) = "") || ((String.trim c2) = "")
                     then
                       raise
                         ((Invalid_argument ("HUHUHUH"))[@explicit_arity ])
                     else ((Some c1), (Some c2))
                 | _ -> (None, None) in
               let range =
                 normalizeCompilerLineColsToRange ~fileLines:cachedContent
                   ~lineRaw:lineNum ~col1Raw ~col2Raw in
               let warningCapture =
                 match execMaybe {|^Warning (\d+): ([\s\S]+)|} body with
                 | None  -> (None, None)
                 | ((Some (capture))[@explicit_arity ]) ->
                     ((getSubstringMaybe capture 1),
                       (getSubstringMaybe capture 2)) in
               (match (errorCapture, warningCapture) with
                | (((Some (errorBody))[@explicit_arity ]),(None ,None )) ->
                    ((ErrorContent
                        ({
                           filePath;
                           cachedContent;
                           range;
                           parsedContent =
                             (ParseError.parse ~customErrorParsers ~errorBody
                                ~cachedContent ~range)
                         }))[@explicit_arity ])
                | (None
                   ,(((Some (code))[@explicit_arity ]),((Some
                     (warningBody))[@explicit_arity ])))
                    ->
                    let code = int_of_string code in
                    ((Warning
                        ({
                           filePath;
                           cachedContent;
                           range;
                           parsedContent =
                             {
                               code;
                               warningType =
                                 (ParseWarning.parse code warningBody
                                    filePath cachedContent range)
                             }
                         }))[@explicit_arity ])
                | _ -> raise ((Invalid_argument (err))[@explicit_arity ])))
      | _ -> ((Unparsable (err))[@explicit_arity ])
  with | _ -> ((Unparsable (err))[@explicit_arity ])
let line_stream_of_channel channel =
  Stream.from
    (fun _  -> try Some (input_line channel) with | End_of_file  -> None)
let parseFromStdin ~refmttypePath  ~customErrorParsers  =
  let errBuffer = ref "" in
  let prettyPrintParsedResult =
    TerminalReporter.prettyPrintParsedResult ~refmttypePath in
  try
    (line_stream_of_channel stdin) |>
      (Stream.iter
         (fun line  ->
            match ((errBuffer.contents), (Re_pcre.pmatch ~rex:fileR line),
                    (Re_pcre.pmatch ~rex:hasErrorOrWarningR line),
                    (Re_pcre.pmatch ~rex:hasIndentationR line))
            with
            | ("",false ,false ,false ) -> print_endline line
            | ("",true ,_,_)|("",_,true ,_)|("",_,_,true ) ->
                errBuffer := (line ^ "\n")
            | (_,true ,_,_) ->
                (((parse ~customErrorParsers errBuffer.contents) |>
                    prettyPrintParsedResult)
                   |> print_endline;
                 errBuffer := (line ^ "\n"))
            | (_,_,_,true )|(_,_,true ,_) ->
                errBuffer := (errBuffer.contents ^ (line ^ "\n"))
            | (_,false ,false ,false ) ->
                if Re_pcre.pmatch ~rex:hasHintR line
                then
                  (errBuffer := (errBuffer.contents ^ (line ^ "\n"));
                   ((parse ~customErrorParsers errBuffer.contents) |>
                      prettyPrintParsedResult)
                     |> print_endline;
                   errBuffer := "")
                else
                  (((parse ~customErrorParsers errBuffer.contents) |>
                      prettyPrintParsedResult)
                     |> print_endline;
                   errBuffer := (line ^ "\n"))));
    if (String.trim errBuffer.contents) <> ""
    then
      ((parse ~customErrorParsers errBuffer.contents) |>
         prettyPrintParsedResult)
        |> print_endline;
    close_in stdin
  with | e -> (close_in stdin; raise e)