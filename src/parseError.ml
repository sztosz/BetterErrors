open BetterErrorsTypes
open Helpers
let stripCommonPrefix (l1,l2) =
  let i = ref 0 in
  while
    ((!i) < (List.length l1)) &&
      (((!i) < (List.length l2)) && ((List.nth l1 (!i)) = (List.nth l2 (!i))))
    do i := ((!i) + 1) done;
  ((Helpers.listDrop (!i) l1), (Helpers.listDrop (!i) l2))
let applyToBoth f (a,b) = ((f a), (f b))
let typeDiff a b =
  ((((((Helpers.stringNsplit a ~by:"."), (Helpers.stringNsplit b ~by:".")) |>
        stripCommonPrefix)
       |> (applyToBoth List.rev))
      |> stripCommonPrefix)
     |> (applyToBoth List.rev))
    |> (applyToBoth (String.concat "."))
let splitEquivalentTypes raw =
  try Some (Helpers.stringSplit raw ~by:"=") with | Not_found  -> None
let functionArgsCount str =
  let nestedFunctionTypeR = Re_pcre.regexp {|\([\s\S]+\)|} in
  let cleaned =
    Re_pcre.substitute ~rex:nestedFunctionTypeR ~subst:(fun _  -> "|||||")
      str in
  (List.length (split {|->|} cleaned)) - 1
let type_IncompatibleType err _ range =
  let allR =
    {|This expression has type([\s\S]*?)but an expression was expected of type([\s\S]*?)(Type\b([\s\S]*?)|$)?((The type constructor[\s\S]*?)|$)?((The type variable[\s\S]* occurs inside ([\s\S])*)|$)|} in
  let extraRaw = get_match_n_maybe 3 allR err in
  let extra =
    match extraRaw with
    | ((Some (a))[@explicit_arity ]) ->
        if (String.trim a) = "" then None else Some (String.trim a)
    | None  -> None in
  let actualRaw = get_match_n 1 allR err in
  let expectedRaw = get_match_n 2 allR err in
  let (actual,actualEquivalentType) =
    match splitEquivalentTypes actualRaw with
    | Some (a,b) ->
        ((String.trim a), (Some (String.trim b)))
    | None  -> ((String.trim actualRaw), None) in
  let (expected,expectedEquivalentType) =
    match splitEquivalentTypes expectedRaw with
    | Some (a,b) ->
        ((String.trim a), (Some (String.trim b)))
    | None  -> ((String.trim expectedRaw), None) in
  ((Type_IncompatibleType
      ({
         actual;
         expected;
         differingPortion = (typeDiff actual expected);
         actualEquivalentType;
         expectedEquivalentType;
         extra
       }))[@explicit_arity ])
let type_MismatchTypeArguments err _ _ =
  let allR =
    {|The constructor ([\w\.]*) *expects[\s]*(\d+) *argument\(s\),\s*but is applied here to (\d+) argument\(s\)|} in
  let typeConstructor = get_match_n 1 allR err in
  let expectedCount = int_of_string @@ (get_match_n 2 allR err) in
  let actualCount = int_of_string @@ (get_match_n 3 allR err) in
  ((Type_MismatchTypeArguments
      ({ typeConstructor; expectedCount; actualCount }))[@explicit_arity ])
let type_UnboundValue err _ _ =
  let unboundValueR = {|Unbound value ([\w\.]*)|} in
  let unboundValue = get_match unboundValueR err in
  let suggestionR =
    {|Unbound value [\w\.]*[\s\S]Hint: Did you mean ([\s\S]+)\?|} in
  let suggestions =
    (get_match_maybe suggestionR err) |>
      (Helpers.optionMap (Re_pcre.split ~rex:(Re_pcre.regexp {|, | or |}))) in
  ((Type_UnboundValue ({ unboundValue; suggestions }))[@explicit_arity ])
let type_SignatureMismatch err cachedContent = raise Not_found
let type_SignatureItemMissing err cachedContent = raise Not_found
let type_UnboundModule err _ _ =
  let unboundModuleR = {|Unbound module ([\w\.]*)|} in
  let unboundModule = get_match unboundModuleR err in
  let suggestionR =
    {|Unbound module [\w\.]*[\s\S]Hint: Did you mean (\S+)\?|} in
  let suggestion = get_match_maybe suggestionR err in
  ((Type_UnboundModule ({ unboundModule; suggestion }))[@explicit_arity ])
let type_UnboundRecordField err _ _ =
  let recordFieldR = {|Unbound record field (\w+)|} in
  let recordField = get_match recordFieldR err in
  let suggestionR = {|Hint: Did you mean (\w+)\?|} in
  let suggestion = get_match_maybe suggestionR err in
  ((Type_UnboundRecordField ({ recordField; suggestion }))[@explicit_arity ])
let type_UnboundConstructor err cachedContent = raise Not_found
let type_UnboundTypeConstructor err _ _ =
  let constructorR = {|Unbound type constructor ([\w\.]+)|} in
  let constructor = get_match constructorR err in
  let suggestionR = {|Hint: Did you mean ([\w\.]+)\?|} in
  let suggestion = get_match_maybe suggestionR err in
  ((Type_UnboundTypeConstructor
      ({ namespacedConstructor = constructor; suggestion }))[@explicit_arity
                                                              ])
let type_AppliedTooMany err _ _ =
  let functionTypeR =
    {|This function has type([\s\S]+)It is applied to too many arguments; maybe you forgot a `;'.|} in
  let functionType = String.trim (get_match functionTypeR err) in
  ((Type_AppliedTooMany
      ({ functionType; expectedArgCount = (functionArgsCount functionType) }))
    [@explicit_arity ])
let type_RecordFieldNotInExpression err cachedContent range = raise Not_found
let type_RecordFieldError err cachedContent range = raise Not_found
let type_FieldNotBelong err cachedContent range = raise Not_found
let type_NotAFunction err _ range =
  let actualR =
    {|This expression has type([\s\S]+)This is not a function; it cannot be applied.|} in
  let actual = String.trim (get_match actualR err) in
  ((Type_NotAFunction ({ actual }))[@explicit_arity ])
let file_SyntaxError err cachedContent range =
  let allR = Re_pcre.regexp {|Syntax error|} in
  if not (Re_pcre.pmatch ~rex:allR err)
  then raise Not_found
  else
    (let hintR = {|Syntax error:([\s\S]+)|} in
     let hint = get_match_maybe hintR err in
     let ((startRow,startColumn),(_,endColumn)) = range in
     ((File_SyntaxError
         ({
            hint = (Helpers.optionMap String.trim hint);
            offendingString =
              (Helpers.stringSlice ~first:startColumn ~last:endColumn
                 (List.nth cachedContent startRow))
          }))[@explicit_arity ]))
let build_InconsistentAssumptions err cachedContent range = raise Not_found
let file_IllegalCharacter err _ _ =
  let characterR = {|Illegal character \(([\s\S]+)\)|} in
  let character = get_match characterR err in
  ((File_IllegalCharacter ({ character }))[@explicit_arity ])
let parsers =
  [type_MismatchTypeArguments;
  type_UnboundValue;
  type_SignatureMismatch;
  type_SignatureItemMissing;
  type_UnboundModule;
  type_UnboundRecordField;
  type_UnboundConstructor;
  type_UnboundTypeConstructor;
  type_AppliedTooMany;
  type_RecordFieldNotInExpression;
  type_RecordFieldError;
  type_FieldNotBelong;
  type_IncompatibleType;
  type_NotAFunction;
  file_SyntaxError;
  build_InconsistentAssumptions;
  file_IllegalCharacter]
let goodFileNameR = Re_pcre.regexp {|^[a-zA-Z][a-zA-Z_\d]+\.\S+$|}
let cannotFindFileRStr = {|Cannot find file ([\s\S]+)|}
let unboundModuleRStr = {|Unbound module ([\s\S]+)|}
let specialParserThatChecksWhetherFileEvenExists filePath errorBody =
  match filePath with
  | "_none_" ->
      (match errorBody with
       | None  -> None
       | ((Some (err))[@explicit_arity ]) ->
           (match get_match_maybe cannotFindFileRStr err with
            | None  -> None
            | ((Some (fileName))[@explicit_arity ]) ->
                Some
                  ((ErrorFile (((NoneFile (fileName))[@explicit_arity ])))
                  [@explicit_arity ])))
  | "command line" ->
      (match errorBody with
       | None  -> None
       | ((Some (err))[@explicit_arity ]) ->
           (match get_match_maybe unboundModuleRStr err with
            | None  -> None
            | ((Some (moduleName))[@explicit_arity ]) ->
                Some
                  ((ErrorFile
                      (((CommandLine (moduleName))[@explicit_arity ])))
                  [@explicit_arity ])))
  | "(stdin)" ->
      (match errorBody with
       | None  -> None
       | ((Some (err))[@explicit_arity ]) ->
           Some
             ((ErrorFile (((Stdin (err))[@explicit_arity ])))[@explicit_arity
                                                               ]))
  | _ -> None
let parse ~customErrorParsers  ~errorBody  ~cachedContent  ~range  =
  try
    (customErrorParsers @ parsers) |>
      (Helpers.listFindMap
         (fun parse  ->
            try Some (parse errorBody cachedContent range) with | _ -> None))
  with | Not_found  -> ((Error_CatchAll (errorBody))[@explicit_arity ])