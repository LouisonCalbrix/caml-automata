(** Graphical program to draw Conway's game of life. 
 * date: March 2021
 * author: Louisono *)

open Automata
open Graphics

(* ============ Utility functions ============ *)

(** [read_lines filename] returns a string with the content of the whole file. *)
let read_lines filename =
    Containers.IO.with_in filename Containers.IO.read_all

(** [automata_of_string str] returns the [life_cell automaton] corresponding to [str]. *)
let automata_of_string =
    of_string life_of_char

(** [automata_of_file filename] returns the [life_cell automata] written (as plain text) in [filename]. *)
let automata_of_file filename =
    let lines = read_lines filename in
    automata_of_string lines

(* ============ Drawing and graphics related functions ============ *)

(** [crop] is a type to describe a subsection of an automata. A crop value has the form [(x, y, width, height)] where [x] and [y] are the coordinates of the upper-right corner of the subsection whereas [width] and [height] are its dimensions. *)
type crop = int * int * int * int

(** [resize crop side] sets the size of the window so that it can display the given [crop] drawn using [side] as a unit. *)
let resize (_, _, width, height) side =
    resize_window (width*side) (height*side)

(** [tile_side automata] returns the maximum side for a square tile to represent cells of [automata]*)
let tile_side (_, _, width, height) =
    let max_width = 1000 and
    max_height = 650 in
    min (max_width/width) (max_height/height)

(* (int * int * int * int) -> int -> 'a automata -> unit *)
(** [draw_sub crop side automata] draws the subsection of the [automata] according to the given [crop]. *)
let draw_sub (x, y, width, height) side automata =
    let draw_at cell x y =
        set_color black;
        if cell = Alive
        then
            fill_rect (x*side) (y*side) side side in
    iteri
        (fun i cell ->
            let xc, yc = xy_of_i i automata in
            if xc >= x && xc < x+width && yc >= y && yc < y+height
            then draw_at cell (xc-x) (yc-y))
        automata

(** [draw_grid crop side] draws the grid according to the given [crop] *)
let draw_grid (_, _, width, height) side =
    set_color black;
    let tot_width = width * side and
    tot_height = height * side in
    let rec horizontal_lines i = 
        if i > 0
        then
            let y = i*side in
            let _ = moveto 0 y and
            _ = lineto tot_width y in
            horizontal_lines (i-1) in
    let rec vertical_lines i =
        if i > 0
        then
            let x = i*side in
            let _ = moveto x 0 and
            _ = lineto x tot_height in
            vertical_lines (i-1) in
    horizontal_lines (height-1);
    vertical_lines (width-1)

(** [visual_loop fps crop side automata] uses the draw_sub function to draw every cell of the [automata] according to the given [crop], at a pace of [fps]. *)
let visual_loop fps crop side automata =
    let interval = 1. /. fps in
    Printf.printf "interval: %f\n" interval;
    let rec loop time1 automat =
        let time2 = Unix.gettimeofday () in
        if time2 -. time1 < interval
        then loop time1 automat
        else
            let _ = clear_graph () and
            _ = draw_grid crop side and
            _ = draw_sub crop side automat and
            _ = synchronize () and
            automat2 = update game_of_life automat in
            loop (time1+.interval) automat2 in
    loop (Unix.gettimeofday ()) automata;
    ()

(** [Bad_cl_args] is an exception to be raised when the command line arguments are ill-formed. See [cl_args] for more details about said arguments. *)
exception Bad_cl_args

(** [cl_args] returns parsed command line arguments. Command line arguments are expected to be of the form: [<file> <fps> \[<horizontal offset> <vertical offset> <width> <height>\]]. [file] is the name of the file which describe the initial state of the automata. [fps] is the pace at which the rendering will be done. [horizontal offset], [vertical offset], [width] and [height] are optionnal but they must be supplied all together if they are supplied at all. They describe a value of type [crop]. *)
(* unit -> string * float * crop option *)
let cl_args () =
    let len = Array.length Sys.argv in
    if len <> 3 && len <> 7
    then
        raise Bad_cl_args
    else
        let fname = Sys.argv.(1) and
        fps = Sys.argv.(2) |> float_of_string in
        if len = 7
        then
            let [x; y; width; height] = 
                [Sys.argv.(3); Sys.argv.(4); Sys.argv.(5); Sys.argv.(6)]
                |> List.map int_of_string in
            fname, fps, Some (x, y, width, height)
        else
            fname, fps, None

(* ============ Main ============ *)

let () =
    let exit_code = ref 0 in
    try
        let fname, fps, crop_opt = cl_args () in
        Printf.printf "fps: %f\n" fps;
        let automata = automata_of_file fname in
        let crop =
            match crop_opt with
            | Some (x, y, width, height) -> (x, y, width, height)
            | None ->
                let (width, height) = ((width automata), (height automata)) in
                (0, 0, width, height) in
        let side = tile_side crop in
        open_graph "";
        set_window_title "life";
        resize crop side;
        auto_synchronize false;
        visual_loop fps crop side automata
    with
    | Graphic_failure _ -> 
        print_endline "goodbye";
    | Bad_cl_args -> 
        Printf.printf "Usage: %s <file> <fps> [<horizontal offset> <vertical offset> <width> <height>]\n" Sys.argv.(0);
        exit_code := 1;
    | e ->
        Printf.printf "%s\n" (Printexc.to_string e);
        exit_code := 2;
    exit !exit_code
