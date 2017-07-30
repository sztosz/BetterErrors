open BetterErrorsTypes
open Helpers
let warning_UnusedVariable code err _ _ _ = raise Not_found
let warning_PatternNotExhaustive code err _ _ _ =
  let unmatchedR =
    {|this pattern-matching is not exhaustive.\sHere is an example of a value that is not matched:\s([\s\S]+)|} in
  let unmatchedRaw = get_match unmatchedR err in
  let unmatched =
    if (unmatchedRaw.[0]) = '('
    then
      (unmatchedRaw |>
         (Helpers.stringSlice ~first:1
            ~last:((String.length unmatchedRaw) - 1)))
        |> (split {|\|[\s]*|})
    else [unmatchedRaw] in
  ((Warning_PatternNotExhaustive ({ unmatched }))[@explicit_arity ])
let warning_PatternUnused code err _ _ _ = raise Not_found
let warning_OptionalArgumentNotErased code err _ cachedContent range =
  let ((startRow,startColumn),(endRow,endColumn)) = range in
  let allR = {|this optional argument cannot be erased\.|} in
  let fileLine = List.nth cachedContent startRow in
  let _ = get_match_n 0 allR err in
  let argumentNameRaw =
    Helpers.stringSlice ~first:startColumn
      ~last:(if startRow = endRow then endColumn else 99999) fileLine in
  let argumentNameR = {|(:?\?\s*\()?([^=]+)|} in
  let argumentName = get_match_n 2 argumentNameR argumentNameRaw in
  ((Warning_OptionalArgumentNotErased
      ({ argumentName = (String.trim argumentName) }))[@explicit_arity ])
let warning_BadFileName code err filePath _ _ =
  if code = 24
  then
    let fileName = Filename.basename filePath in
    let offendingChar =
      match ((get_match_maybe {|^([^a-zA-Z])|} fileName),
              (get_match_maybe {|.+?([^a-zA-Z\.])|} fileName))
      with
      | (((Some (m))[@explicit_arity ]),_) ->
          ((Leading (m))[@explicit_arity ])
      | (None ,((Some (m))[@explicit_arity ])) ->
          ((Contains (m))[@explicit_arity ])
      | _ -> UnknownIllegalChar in
    ((Warning_BadFileName (offendingChar))[@explicit_arity ])
  else raise Not_found
let parsers =
  [warning_UnusedVariable;
  warning_PatternNotExhaustive;
  warning_PatternUnused;
  warning_OptionalArgumentNotErased;
  warning_BadFileName]
let parse code warningBody filePath cachedContent range =
  try
    Helpers.listFindMap
      (fun parse'  ->
         try Some (parse' code warningBody filePath cachedContent range)
         with | _ -> None) parsers
  with | Not_found  -> ((Warning_CatchAll (warningBody))[@explicit_arity ])