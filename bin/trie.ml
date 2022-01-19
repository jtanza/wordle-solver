open Batteries
module CharMap = Map.Make (Char)
module CharSet = Set.Make (Char)

type trie = Node of string * trie CharMap.t

let add word trie =
  let rec add_word w (Node (_, children)) =
    match w with
    | [] -> Node (word, children)
    | x :: xs ->
        let updated =
          match CharMap.find_opt x children with
          | None -> add_word xs @@ Node ("", CharMap.empty)
          | Some t -> add_word xs t
        in
        Node ("", CharMap.add x updated children)
  in
  add_word (String.explode word) trie

let add_all words =
  List.fold (fun t w -> add w t) (Node ("", CharMap.empty)) words

let wildcard_search query exclude trie =
  let acc = Dllist.create "" in
  let rec search w t =
    match (w, t) with
    | x :: xs, Node ("", children) -> (
      match CharMap.find_opt x children with
      | Some t -> search xs t
      | None -> (
        match x with
        | '.' ->
            CharMap.filter (fun k _ -> not @@ CharSet.mem k exclude) children
            |> CharMap.values
            |> Enum.iter (fun t -> search xs t)
        | _ -> () ) )
    | _, Node (word, _) -> Dllist.add acc word
  in
  search (String.explode query) trie ;
  Dllist.to_list acc
