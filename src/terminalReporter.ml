open BetterErrorsTypes
open Helpers
let numberOfDigits n =
  let digits = ref 1 in
  let nn = ref n in
  while ((!nn) / 10) > 0 do (nn := ((!nn) / 10); digits := ((!digits) + 1))
  done;
  !digits
let pad ?(ch= ' ')  content n =
  (String.make (n - (String.length content)) ch) ^ content
let startingSpacesCount str =
  let rec startingSpacesCount' str idx =
    if idx = (String.length str)
    then idx
    else
    if (str.[idx]) <> ' ' then idx else startingSpacesCount' str (idx + 1) in
  startingSpacesCount' str 0



let _printFile ~highlightColor:color 
    ~highlight:((startRow,startColumn),(endRow,endColumn))  content =
  let displayedStartRow = max 0 (startRow - 3) in
  let displayedEndRow = min ((List.length content) - 1) (startRow + 3) in
  let lineNumWidth = numberOfDigits (List.length content) in
  let rowsForCountingStartingSpaces =
    ((listDrop displayedStartRow content) |>
     (listTake ((displayedEndRow - displayedStartRow) + 1)))
    |> (List.filter (fun row  -> row <> "")) in
  let minIndent =
    match rowsForCountingStartingSpaces with
    | [] -> 0
    | _ ->
      let startingSpaces =
        List.map startingSpacesCount rowsForCountingStartingSpaces in
      List.fold_left (fun acc  -> fun num  -> if num < acc then num else acc)
        (List.hd startingSpaces) startingSpaces in

  let sep = if minIndent = 0 then " │ " else " ┆ " in

  for i = displayedStartRow to displayedEndRow do
    (let currLine = (List.nth content i) |> (stringSlice ~first:minIndent) in
     if (i >= startRow) && (i <= endRow)
     then
       (if startRow = endRow
        then
          result :=
            (((pad (string_of_int (i + 1)) lineNumWidth) ^
              (sep ^
               (highlight ~color ~first:startColumn ~last:endColumn
                  currLine)))
             :: (!result))
        else
        if i = startRow
        then
          result :=
            (((pad (string_of_int (i + 1)) lineNumWidth) ^
              (sep ^ (highlight ~color ~first:startColumn currLine)))
             :: (!result))
        else
        if i = endRow
        then
          result :=
            (((pad (string_of_int (i + 1)) lineNumWidth) ^
              (sep ^ (highlight ~color ~last:endColumn currLine)))
             :: (!result))
        else
          result :=
            (((pad (string_of_int (i + 1)) lineNumWidth) ^
              (sep ^ (highlight ~color currLine)))
             :: (!result)))
     else
       result :=
         (((pad (string_of_int (i + 1)) lineNumWidth) ^ (sep ^ currLine)) ::
          (!result)))
  done;
  ((!result) |> List.rev) |> (String.concat "\n")

let printFile ?(isWarning= false)  { cachedContent; filePath; range } =
  let ((startRow,startColumn),(endRow,endColumn)) = range in
  let filePathDisplay =
    if startRow = endRow
    then
      cyan @@
      (sp "%s:%d %d-%d\n" filePath (startRow + 1) startColumn endColumn)
    else
      cyan @@
      (sp "%s:%d:%d-%d:%d\n" filePath (startRow + 1) startColumn
         (endRow + 1) endColumn) in
  filePathDisplay ^
  (_printFile ~highlightColor:(if isWarning then yellow else red)
     ~highlight:range cachedContent)

let prettyPrintParsedResult ~refmttypePath  (result : result) =
  match result with
  | ((Unparsable (str))[@explicit_arity ]) -> str
  | ((ErrorFile (NonexistentFile ))[@explicit_arity ]) -> ""
  | ((ErrorFile (((Stdin (original))[@explicit_arity ])))[@explicit_arity ])
    -> sp "%s: (from stdin)\n%s" (red "Error") original
  | ((ErrorFile
        (((CommandLine (moduleName))[@explicit_arity ])))[@explicit_arity ]) ->
    sp "%s: module `%s` not found." (red "Error") moduleName
  | ((ErrorFile
        (((NoneFile (filename))[@explicit_arity ])))[@explicit_arity ]) ->
    if Filename.check_suffix filename ".cmo"
    then
      sp
        "%s: Cannot find file %s. Cmo files are artifacts the compiler looks for when compiling/linking dependent files."
        (red "Error") (cyan filename)
    else sp "%s: Cannot find file %s." (red "Error") (cyan filename)
  | ((ErrorContent (withFileInfo))[@explicit_arity ]) ->
    sp "%s\n\n%s: %s" (printFile withFileInfo) (red "Error")
      (ReportError.report ~refmttypePath withFileInfo.parsedContent)
  | ((Warning (withFileInfo))[@explicit_arity ]) ->
    sp "%s\n\n%s %d: %s" (printFile ~isWarning:true withFileInfo)
      (yellow "Warning") (withFileInfo.parsedContent).code
      (ReportWarning.report ~refmttypePath
         (withFileInfo.parsedContent).code withFileInfo.filePath
         (withFileInfo.parsedContent).warningType)