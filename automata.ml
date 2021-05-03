open Containers

(* =============== utility functions =============== *)

(* string -> char list *)
(** [explode str] returns a [char list] corresponding to the given [str]. *)
let explode str =
    let rec helper acc i =
        if i = ~-1
        then acc
        else helper (str.[i]::acc) (i-1) in
    helper [] (String.length str - 1)

(* 'a list -> (unit -> 'a opt) *)
(** [get_iter l] returns a function [next] whose evalutation returns, one after the other, an option over every element of the given list [l]. Once the whole list has been iterated over, [next] returns [None]. *)
let get_iter l =
    let inner = ref l in
    let rec get_next () =
        match !inner with
        | [] -> None
        | h::t -> 
            inner := t;
            Some h in
    get_next


(* =============== automata type =============== *)

(** Type automata, represents a cellular automata of width * height. Stores cells
    as a 1D Array where elements whose index is in interval \[i*width, (i+1)*width\[ 
    are cells of the ith row. *)
type 'a automata = int * int * 'a array

let width (w, _, _) = w
let height (_, h, _) = h
let cells (_, _, cs) = cs

(* int -> int -> 'a automata -> 'a *)
(** [cell x y automata] returns cell at coordinates [(x, y)]. [Raise]: [Invalid_argument] if such a cell doesn't exist in [automata]. *)
let cell x y (width, height, cells) =
    if x < 0 || x >= width || y < 0 || y >= height
    then raise (Invalid_argument "cell: coordinates out of reach.")
    else cells.(y*width+x)

(** [xy_of_i i automata] returns the [(x, y)] coordinates of cell at index [i] in the given [automata]. [Raise]: [Invalid_argument] if the resulting pair of coordinates would point outside the given [automata]. *)
let xy_of_i i (width, height, _) =
    let x = i mod width and
    y = i / width in
    if 0 <= x && x < width && 0 <= y && y < height
    then (x, y)
    else raise (Invalid_argument "xy_of_i: coordinates out of reach.")

(** [i_of_xy (x, y) automata] returns the index i of cell at coordinate [(x, y)] in given [automata]. [Raise]: [Invalid_argument] if the pair of coordinates [(x, y)] points outside the given [automata]. *)
let i_of_xy (x, y) (width, height, _) =
    if 0 <= x && x < width && 0 <= y && y < height
    then y * width + x
    else raise (Invalid_argument "i_of_xy: coordinates out of reach.")

(* int -> int -> 'a automata -> ((int * int) * 'a) list *)
(** [neighbors_coords (x, y) automata] returns a list of all the cells adjacent to the cell at coordinates [(x, y)], as well as their coodinates in the form of tuples [(x_neighbor, y_neighbor), cell_value]. [Raise]: [Invalid_argument] if the pair of coordinates [(x, y)] points outside the given [automata]*)
let neighbors_coords (x, y) (width, height, _ as automata) =
    if x < 0 || width <= x || y < 0 || height <= y
    then raise (Invalid_argument "neighbors_coords: coordinates out of reach.")
    else
        let distances =
            List.flat_map 
                (fun x -> 
                    List.filter_map 
                        (fun y ->
                            if x = 0 && y = 0 
                            then None
                            else Some(x, y))
                        [-1; 0; 1])
                [-1; 0; 1] in
        List.filter_map
            (fun (dx, dy) ->
                if x+dx < 0 || x+dx >= width || y+dy < 0 || y+dy >= height
                then None
                else Some (x+dx, y+dy))
            distances
        |> List.map (fun (x, y) -> ((x, y), cell x y automata))

(* int -> int -> 'a automat -> 'a list *)
(** [neighbors] is the same as [neighbors_coords] expects it returns the adjacent cells without their coordinates. *)
let neighbors (x, y) automata =
    neighbors_coords x y automata |> List.map (fun ((x, y), cell) -> cell)

(** Return an automata whose cells are a subarray of automat. *)
let sub x y w h (width, height, cells as automat) =
    let arr =
        Array.init
            (w*h)
            (fun i ->
                let xc = x + i mod w and
                yc = y + i / w in
                cell xc yc automat) in
    (w, h, arr)

(* (int -> int -> 'a automata) -> 'a automata -> 'b automata *)
(** [update f automata] returns an automata where all the cells have been updated, using the function [f]. *)
let update f (width, height, _ as automata) =
    let new_cells =
        Array.init 
            (width*height)
            (fun i ->
                let (x, y) = xy_of_i i automata in
                f (x, y) automata) in
    (width, height, new_cells)

(** [iteri f automata] applies the function [f] in turn to every cell of the given [automata], [f] takes an index and its corresponding cell as arguments. *)
let iteri f (_, _, cells) =
    Array.iteri f cells

(** [iterxy] is the same as [iteri] except the given function [f] takes a pair of coordinates and its corresponding cells as arguments. *)
let iterxy f (_, _, cells) =
    Array.iteri 
        (fun i cell -> f (xy_of_i i automata) cell)
        cells

(* ('a -> 'b) -> 'a list list -> 'b automata *)
(** [of_llist f l] returns an automata built from the given list of lists [l]. Every sublist [sub] of [l] corresponds to a row and every element [el] of [sub] corresponds to a cell, the cells are generated by applying [f] to every element [el]. [Raise]: [Invalid_argument] if lists inside [l] don't have all the same length. *)
let of_llist f l =
    let rec length_lists li acc =
        match li, acc with
        | [], None -> 0
        | [], Some len -> len
        | h::t, None -> length_lists t (Some (List.length h))
        | h::t, Some len ->
            if List.length h = len
            then length_lists t acc
            else raise (Invalid_argument "of_llist: lists of different lengths.") in
    let width = length_lists l None and
    height = List.length l and
    get_next = get_iter (List.concat l) in
    let cells = Array.init 
        (width*height) 
        (fun _ -> let Some el = get_next () in f el) in
    (width, height, cells)

(* (char -> 'a) -> string -> 'a automata *)
(** [of_strings] is the same as [of_llist] except a string [str] is used to build the automata. [str] should consist of several substrings separated by the newline character ['\n']. *)
let of_string f str =
    String.trim str 
    |> String.split_on_char '\n'
    |> List.map explode 
    |> of_llist f

(* ('a -> 'b) -> 'a automata -> 'b list list *)
let to_llist f (width, height, cells) =
    let size = width * height in
    let rec helper i curr_list acc =
        if i = 0
        then acc
        else
            let curr_value = f cells.(i) in
            let updated = curr_value::curr_list in
            if i mod width = 0
            then helper (i-1) [] (updated::acc)
            else helper (i-1) updated acc in
    helper size [] []

(* ('a -> char) -> 'a automata -> string*)
let to_string f (width, height, cells) =
    let buff = Buffer.create ((width+1)*height) and
    size = width * height in
    let rec helper i =
        if i = size
        then Buffer.contents buff
        else 
            Printf.sprintf 
                (if i mod width = width - 1 then "%c\n" else "%c")
                (f cells.(i))
            |> Buffer.add_string buff
            |> (fun _ -> helper (i+1)) in
    helper 0

(* =============== example use: game of life =============== *)
type life_cell =
    | Alive
    | Dead

(* char -> life_cell *)
let life_of_char = function
    | '0' -> Dead
    | '1' -> Alive
    | _ -> raise (Invalid_argument "life_of_char")

(* life_cell -> char *)
let char_of_life = function
    | Dead -> '0'
    | Alive -> '1'

let str_of_life (width, height, cells) =
    let buff = Buffer.create ((width+1)*height) in
    Array.map char_of_life cells
    |> Array.iteri 
        (fun i e -> 
            Buffer.add_char buff e; 
            if i mod width = width - 1 
            then Buffer.add_char buff '\n');
    Buffer.contents buff

(* int -> int -> life_cell automata -> life_cell *)
(** Return the state of the cell at x y for the next generation. *)
let game_of_life x y automat =
    let alive_count =
        neighbors x y automat
        |> List.fold_left (fun acc -> function Dead -> acc | Alive -> acc+1) 0 in
    match cell x y automat with
    | Dead -> if alive_count = 3 then Alive else Dead
    | Alive -> if alive_count < 2 || alive_count > 3 then Dead else Alive
