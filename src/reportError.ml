open BetterErrorsTypes
open Helpers
let listify suggestions =
  (suggestions |> (List.map (fun sug  -> "- `" ^ (sug ^ "`")))) |>
    (String.concat "\n")
let highlightPart ~color  ~part  str =
  let indexOfPartInStr = Helpers.stringFind str part in
  highlight ~color ~first:indexOfPartInStr
    ~last:(indexOfPartInStr + (String.length part)) str
let report ~refmttypePath  parsedContent =
  let formatOutputSyntax types =
    match refmttypePath with
    | None  -> types
    | ((Some (path))[@explicit_arity ]) ->
        let types = String.concat {|\"|} types in
        let cmd = path ^ ((sp {| "%s"|}) types) in
        let input = Unix.open_process_in cmd in
        let result = ref [] in
        ((try while true do result := ((input_line input) :: (!result)) done
          with | End_of_file  -> ignore (Unix.close_process_in input));
         List.rev (!result)) in
  match parsedContent with
  | ((Error_CatchAll (error))[@explicit_arity ]) -> error
  | ((Type_MismatchTypeArguments
      ({ typeConstructor; expectedCount; actualCount }))[@explicit_arity ])
      ->
      sp "This needs to be applied to %d argument%s, we found %d."
        expectedCount (if expectedCount = 1 then "" else "s") actualCount
  | ((Type_IncompatibleType
      ({ actual; expected; differingPortion; actualEquivalentType;
         expectedEquivalentType; extra }))[@explicit_arity ])
      ->
      let (actual,expected) =
        match formatOutputSyntax [actual; expected] with
        | a::b::[] -> (a, b)
        | _ -> (actual, expected) in
      (sp "The types don't match.\n%s %s\n%s  %s" (red "This is:")
         (highlight actual) (green "Wanted:")
         (highlight ~color:green expected))
        ^
        ((match extra with
          | ((Some (e))[@explicit_arity ]) -> "\nExtra info: " ^ e
          | None  -> ""))
  | ((Type_NotAFunction ({ actual }))[@explicit_arity ]) ->
      let actual =
        match formatOutputSyntax [actual] with | a::[] -> a | _ -> actual in
      "This is " ^
        (actual ^
           (". You seem to have called it as a function.\n" ^
              "Careful with spaces, semicolons, parentheses, and whatever in-between!"))
  | ((Type_AppliedTooMany
      ({ functionType; expectedArgCount }))[@explicit_arity ]) ->
      let functionType =
        match formatOutputSyntax [functionType] with
        | a::[] -> a
        | _ -> functionType in
      sp
        "This function has type %s\nIt accepts only %d arguments. You gave more. Maybe you forgot a `;` somewhere?"
        functionType expectedArgCount
  | ((File_SyntaxError ({ offendingString; hint }))[@explicit_arity ]) ->
      (match hint with
       | ((Some (a))[@explicit_arity ]) -> "The syntax is wrong: " ^ a
       | None  -> "The syntax is wrong.") ^
        ("\n" ^
           ((match offendingString with
             | ";" ->
                 "Semicolon is an infix symbol used *between* expressions that return `unit` (aka \"nothing\").\n"
             | "else" ->
                 "Did you happen to have put a semicolon on the line before else?"
                   ^
                   " Also, `then` accepts a single expression. If you've put many, wrap them in parentheses.\n"
             | _ -> "") ^
              "Note: the location indicated might not be accurate."))
  | ((File_IllegalCharacter ({ character }))[@explicit_arity ]) ->
      sp
        "The character `%s` is illegal. EVERY CHARACTER THAT'S NOT AMERICAN IS ILLEGAL!"
        character
  | ((Type_UnboundTypeConstructor
      ({ namespacedConstructor; suggestion }))[@explicit_arity ]) ->
      let namespacedConstructor =
        match formatOutputSyntax [namespacedConstructor] with
        | a::[] -> a
        | _ -> namespacedConstructor in
      (sp "The type constructor %s can't be found." namespacedConstructor) ^
        ((match suggestion with
          | None  -> ""
          | ((Some (h))[@explicit_arity ]) ->
              sp "\nHint: did you mean `%s`?" h))
  | ((Type_UnboundValue ({ unboundValue; suggestions }))[@explicit_arity ])
      ->
      (match suggestions with
       | None  -> sp "`%s` can't be found. Could it be a typo?" unboundValue
       | ((Some (hint::[]))[@explicit_arity ]) ->
           sp "`%s` can't be found. Did you mean `%s`?" unboundValue hint
       | ((Some (hint1::hint2::[]))[@explicit_arity ]) ->
           sp "`%s` can't be found. Did you mean `%s` or `%s`?" unboundValue
             hint1 hint2
       | ((Some (hints))[@explicit_arity ]) ->
           sp "`%s` can't be found. Did you mean one of these?\n%s"
             unboundValue (listify hints))
  | ((Type_UnboundRecordField
      ({ recordField; suggestion }))[@explicit_arity ]) ->
      let recordField =
        match formatOutputSyntax [recordField] with
        | a::[] -> a
        | _ -> recordField in
      (match suggestion with
       | None  ->
           sp "Field `%s` can't be found in any record type." recordField
       | ((Some (hint))[@explicit_arity ]) ->
           sp
             "Field `%s` can't be found in any record type. Did you mean `%s`?"
             recordField hint)
  | ((Type_UnboundModule ({ unboundModule; suggestion }))[@explicit_arity ])
      ->
      let unboundModule =
        match formatOutputSyntax [unboundModule] with
        | a::[] -> a
        | _ -> unboundModule in
      (sp "Module `%s` not found in included libraries.\n" unboundModule) ^
        ((match suggestion with
          | ((Some (s))[@explicit_arity ]) -> sp "Hint: did you mean `%s`?" s
          | None  ->
              let pckName = String.lowercase unboundModule in
              "Hint: your build rules might be missing a link. If you're using: \n"
                ^
                (" - Oasis: make sure you have `" ^
                   (pckName ^
                      ("` under `BuildDepends` in your _oasis file.\n" ^
                         (" - ocamlbuild: make sure you have `-pkgs " ^
                            (pckName ^
                               ("` in your build command.\n" ^
                                  (" - ocamlc | ocamlopt: make sure you have `-I +"
                                     ^
                                     (pckName ^
                                        ("` in your build command before the source files.\n"
                                           ^
                                           (" - ocamlfind: make sure you have `-package "
                                              ^
                                              (pckName ^
                                                 " -linkpkg` in your build command.")))))))))))))
  | _ -> "huh"