open Batteries
module StringMap = Map.Make (String)
module CharMap = Map.Make (Char)
module CharSet = Set.Make (Char)
module IntSet = Set.Make (Int)

let first_guess words = words |> List.take 1_000 |> List.shuffle |> List.last |> fst

let build_word_map words =
  List.fold (fun m t -> StringMap.add (fst t) (snd t) m) StringMap.empty words

let build_word_list file =
  File.lines_of file |> List.of_enum
  |> List.map @@ String.split_on_char ','
  |> List.filter_map (fun l ->
         match l with
         | a :: b :: _ when String.length a = 5 && int_of_string b > 200_000 ->
             Some (a, int_of_string b)
         | _ -> None )
  |> List.sort (fun a b -> compare (snd a) (snd b))
  |> List.rev

type color = Green | Yellow | Gray

let build_candidates m =
  let keepers =
    CharMap.filterv
      (fun v ->
        let color = fst v in
        color = Green || color = Yellow )
      m
  in
  let permutations =
    CharMap.keys keepers
    |> Enum.merge ( > ) @@ Enum.repeat ~times:5 '.'
    |> Enum.take 5 |> List.of_enum |> LazyList.permutations |> LazyList.unique
  in
  LazyList.filter
    (fun l ->
      CharMap.for_all
        (fun k v ->
          match List.index_of k l with
          | None -> true
          | Some i -> (
            match v with
            | color, indexes -> (
              match color with
              | Green -> IntSet.mem i indexes
              | Yellow -> not @@ IntSet.mem i indexes
              | _ -> true ) ) )
        keepers )
    permutations
  |> LazyList.to_list |> List.map String.of_list

let top_matches word_map matches =
  List.map (fun m -> (m, StringMap.find_default Int.min_num m word_map)) matches
  |> List.sort (fun a b -> compare (snd a) (snd b))
  |> List.rev |> List.take 3 |> List.map fst

let merge a b =
  CharMap.merge
    (fun _ xo yo ->
      match (xo, yo) with
      | Some x, Some y -> Some (fst x, IntSet.union (snd x) (snd y))
      | None, yo -> yo
      | xo, None -> xo )
    a b

let rec prompt_first_guess words =
  let g = first_guess words in
  String.println IO.stdout
  @@ Printf.sprintf
       "\nHeres a first guess: [%s]\nContinue with this guess [y] or generate another? [n]" g ;
  IO.flush IO.stdout ;
  match IO.read_line IO.stdin with "y" -> () | _ -> prompt_first_guess words

let prompt_for color =
  String.print IO.stdout @@ Printf.sprintf "%s: " color ;
  IO.flush IO.stdout ;
  match IO.read_line IO.stdin with "" -> None | r -> Some r

let parse_response color = function
  | None -> None
  | Some s -> (
      let l = String.split_on_char ',' s in
      match color with
      | Gray -> Some (List.map (fun s -> (s.[0], (color, IntSet.empty))) l)
      | _ ->
          Some
            (List.map
               (fun s ->
                 (s.[0], (color, s.[1] |> String.of_char |> String.to_int |> IntSet.singleton)) )
               l ) )

let rec prompt prev word_map trie =
  String.println IO.stdout "How'd we do?" ;
  IO.flush IO.stdout ;
  let resps =
    [ prompt_for "Gray" |> parse_response Gray
    ; prompt_for "Green" |> parse_response Green
    ; prompt_for "Yellow" |> parse_response Yellow ]
  in
  let response_map =
    List.filter_map identity resps |> List.flatten |> Seq.of_list |> CharMap.of_seq |> merge prev
  in
  let excludes =
    CharMap.filterv (fun v -> fst v = Gray) response_map
    |> CharMap.keys
    |> Enum.fold (fun s e -> CharSet.add e s) CharSet.empty
  in
  let matches =
    build_candidates response_map
    |> List.map (fun cand -> Trie.wildcard_search cand excludes trie)
    |> List.flatten |> top_matches word_map |> String.concat ", "
    |> Printf.sprintf "Here are the top guesses ranked by frequency: %s"
  in
  String.println IO.stdout matches ;
  String.println IO.stdout "Did we win [y] or continue guessing [n]" ;
  IO.flush IO.stdout ;
  match IO.read_line IO.stdin with
  | "n" -> prompt response_map word_map trie
  | _ -> String.println IO.stdout "Hooray!"

let main () =
  match Sys.argv with
  | [|_; "--help"|] ->
      String.println IO.stdout
        "Provides guesses for Wordle puzzles by prompting users for previously placed letters and \
         their green/gray/yellow outcome.\n\n\
         For green and yellow prompts, enter a comma separated list of letters and the 0 based \
         index of their last placement, e.g. d2,s4\n\
         For gray prompts, enter a comma separated list of letters only, e.g. x,y,z\n\
         Simply omit any input if no letters of a particualr color were matched."
  | _ ->
      let words = build_word_list "unigram_freq.csv" in
      let word_map = build_word_map words in
      let trie = List.map fst words |> Trie.add_all in
      prompt_first_guess words ;
      prompt CharMap.empty word_map trie

let () = main ()
