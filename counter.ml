module CharSet = Set.Make(Char)

module type CHAR_CONTAINER =
  sig
    type container
    val contains : container -> char -> bool
    val add : container -> char -> container
    val empty : unit -> container
  end

(* module HashTblContainer = 
  struct
    type container = (char, char) Hashtbl.t
  
    let contains con c = Hashtbl.find_opt con c |> Option.is_some
    let add con c = Hashtbl.replace con c c
    let empty () = Hashtbl.create 10
  end *)

module SetContainer = 
  struct
    module CharSet = Set.Make(Char)
    type container = CharSet.t

    let contains con c = CharSet.find_opt c con |> Option.is_some
    let add con c = CharSet.add c con
    let empty () = CharSet.empty
  end

let solve3 word (module M : CHAR_CONTAINER) =
  let rec solve2 con word' = match word'() with
      Seq.Cons (c, _) when M.contains con c -> Some c
    | Seq.Cons (c, xs) -> solve2 (M.add con c) xs
    | Nil -> None in
  let empty = M.empty () and word_seq = String.to_seq word in
  solve2 empty word_seq
  

let setSolve word =
  let mySet = (module SetContainer : CHAR_CONTAINER) in
  solve3 word mySet

(* let hashSolve word =
  let myHtable = (module HashTblContainer : CHAR_CONTAINER) in
  solve3 word myHtable *)

(* Adds a char to the Set and returns the new instance and a 
bool indicating if the insertion suceeded *)
let add achar target =
    let added = CharSet.add achar target in
    target <> added, added

let add_ht c ht = 
  let lbefore = Hashtbl.length ht in
  Hashtbl.replace ht c c;
  lbefore < Hashtbl.length ht, ht

let rec solve' word' container add_fn = match word'() with
    Seq.Cons (c, xs) -> 
        (match add_fn c container with
          (true, s)  -> solve' xs s add_fn
          | _ -> Some c)
  | _ -> None


(* Finds the first repeated character in a word usingn a set *)
let solve word =
  Dream.info (fun log -> log "Solving [%s] using set" word);
  setSolve word

(* Finds the first repeated character in a word using a hash table *)
let solve_ht word =
  Dream.info (fun log -> log "Solving [%s] using hash table" word);
  let size = String.length word and seq = String.to_seq word in
  solve' seq (Hashtbl.create size) add_ht

(* Finds the repetition using Set or Hashtable *)
let res w s = 
  let res' = match s with
      Some "set" -> solve w
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