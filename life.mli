(** Simple interface specification for the life program. *)

val read_lines : string -> string
val automata_of_string : string -> Automata.life_cell Automata.automata
val automata_of_file : string -> Automata.life_cell Automata.automata

type crop
val resize : crop -> int -> unit
val tile_side : crop -> int
val draw_sub : crop -> int -> Automata.life_cell Automata.automata -> unit
val draw_grid : crop -> int -> unit
val visual_loop : float -> crop -> int -> Automata.life_cell Automata.automata -> unit

exception Bad_cl_args
val cl_args : unit -> string * float * crop option
