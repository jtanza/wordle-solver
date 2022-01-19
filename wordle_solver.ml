open Batteries

module StringMap = Map.Make(String)

let build_word_list file =
  File.lines_of file
    |> List.of_enum
    |> List.map @@ String.split_on_char ','
    |> List.filter_map (fun l -> match l with
      | ((a::b::_)) when String.length a = 5 && int_of_string b > 200_000 -> Some((a, int_of_string b))
      | _ -> None)
    |> List.sort (fun a b -> compare (snd a) (snd b))
    |> List.rev

let build_word_map words = List.fold (fun m t -> StringMap.add (fst t) (snd t) m) StringMap.empty words

let first_guess words = words |> List.take 500 |> List.shuffle |> List.first |> fst

type color = Green | Yellow | Gray

module CharMap = Map.Make(Char)
module CharSet = Set.Make(Char)
module IntSet = Set.Make(Int)

let build_candidates m =
  let keepers = CharMap.filterv (fun v -> let color = fst v in (color = Green || color = Yellow)) m in
    let permutations = CharMap.keys keepers
      |> Enum.merge (>) @@ Enum.repeat ~times:5 '.'
      |> Enum.take 5
      |> List.of_enum
      |> LazyList.permutations
      |> LazyList.unique in

  LazyList.filter (fun l -> CharMap.for_all (fun k v -> match List.index_of k l with
      | None -> true
      | Some(i) -> match v with
        | (color, indexes) -> match color with
          | Green -> IntSet.mem i indexes
          | Yellow -> not @@ IntSet.mem i indexes
          | _ -> true) keepers) permutations

let rec prompt_first_guess words =
  let g = (first_guess words) in
    IO.write_line IO.stdout @@ Printf.sprintf "Heres a first guess: [%s]\nContinue with this guess [y] or generate another? [n]" g;
    IO.flush_all();
    match IO.read_line IO.stdin with
      | "y" -> g
      | _ -> prompt_first_guess words

type trie = Node of string * trie CharMap.t

let add word trie =
  let rec add_word w (Node(_, children)) = match w with
    | [] -> Node (word, children)
    | x::xs -> let updated = match CharMap.find_opt x children with
      | None -> add_word xs @@ Node("", CharMap.empty)
      | Some(t) -> add_word xs t in
   Node ("", CharMap.add x updated children) in
  add_word (String.explode word) trie

let add_all words =
  List.fold (fun t w -> add w t) (Node("", CharMap.empty)) words

let wildcard_search query exclude trie =
  let acc = Dllist.create "" in
    let rec search w t = match w, t with
      | x::xs, Node("", children) -> (match CharMap.find_opt x children with
        | Some(t) -> search xs t
        | None -> match x with
          | '.' -> CharMap.filter (fun k _ -> not @@ CharSet.mem k exclude) children |> CharMap.values |> Enum.iter (fun t -> search xs t)
          | _ -> ())
      | _, Node(word, _) -> Dllist.add acc word in
    search (String.explode query) trie;
    Dllist.to_list acc

let main () =
  let words = build_word_list "unigram_freq.csv" in
  prompt_first_guess words |> IO.write_line IO.stdout

let () = main ()

