module type CHAR_CONTAINER =
  sig
    type container
    val contains : container -> char -> bool
    val add : container -> char -> container
    val empty : unit -> container
  end

module HashTblContainer = 
  struct
    type container = (char, char) Hashtbl.t
  
    let contains con c = Hashtbl.find_opt con c |> Option.is_some
    let add con c = Hashtbl.replace con c c; con
    let empty () = Hashtbl.create 10
  end

module SetContainer = 
  struct
    module CharSet = Set.Make(Char)
    type container = CharSet.t

    let contains con c = CharSet.find_opt c con |> Option.is_some
    let add con c = CharSet.add c con
    let empty () = CharSet.empty
  end

let solve word (module M : CHAR_CONTAINER) =
  let rec solve' con word' = match word'() with
      Seq.Cons (c, _) when M.contains con c -> Some c
    | Seq.Cons (c, xs) -> solve' (M.add con c) xs
    | Nil -> None in
  let empty = M.empty () and word_seq = String.to_seq word in
  solve' empty word_seq
  
(* Finds the first repeated character in a word usingn a set *)
let solve_set word =
  Dream.info (fun log -> log "Solving [%s] using set" word);
  let mySet = (module SetContainer : CHAR_CONTAINER) in
  solve word mySet

(* Finds the first repeated character in a word using a hash table *)
let solve_ht word =
  Dream.info (fun log -> log "Solving [%s] using hash table" word);
  let myHtable = (module HashTblContainer : CHAR_CONTAINER) in
  solve word myHtable

(* Finds the repetition using Set or Hashtable *)
let res w s = 
  let res' = match s with
      Some "set" -> solve_set w
    | _ -> solve_ht w in
  match res' with
    Some c -> Printf.sprintf "Found %c" c
  | None -> "No repetition found"

let () = 
  Dream.run ~debug:true
  @@ Dream.logger
  @@ Dream.router [
    Dream.get "/find_repeated" (fun req -> match Dream.query "word" req with 
                  | None -> Dream.empty `Bad_Request
                  | Some word -> Dream.html (res word (Dream.query "s" req)))
  ]
  @@ Dream.not_found