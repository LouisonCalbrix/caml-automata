type 'a automata

val width : 'a automata -> int
val height : 'a automata -> int
val cells : 'a automata -> 'a array

val cell : int -> int -> 'a automata -> 'a
val i_of_xy : int * int -> 'a automata -> int
val xy_of_i : int -> 'a automata -> int * int
val neighbors_coords : int -> int -> 'a automata -> ((int * int) * 'a) list
val neighbors : int -> int -> 'a automata -> 'a list
val update : (int -> int -> 'a automata -> 'b) -> 'a automata -> 'b automata
val sub : int -> int -> int -> int -> 'a automata -> 'a automata
val iteri : (int -> 'a -> unit) -> 'a automata -> unit

val of_llist : ('a -> 'b) -> 'a list list -> 'b automata
val of_string : (char -> 'a) -> string -> 'a automata
val to_llist : ('a -> 'b) -> 'a automata -> 'b list list
val to_string : ('a -> char) -> 'a automata -> string


type life_cell = Alive | Dead

val life_of_char : char -> life_cell
val char_of_life : life_cell -> char
val str_of_life : life_cell automata -> string
val game_of_life : int -> int -> life_cell automata -> life_cell
