open Graph

type cost = int

(** Tas binaire min, trié sur cost *)
type 'cost heap = {
  mutable size : int;
  mutable data : (id * 'cost) array;
  pos : (id, int) Hashtbl.t;   (* id -> index *)
}


val create_heap : int -> cost heap
(** [create capacity dummy]
    Crée un tas vide avec une capacité maximale.
    [dummy] est une valeur bidon interne. *)

val add_heap : cost heap -> (id * cost) -> unit
(** Ajoute un élément (arc, cost) au tas *)

val take_min_heap : cost heap -> (id * cost) option
(** Retire et retourne l’élément de coût minimal,
    ou [None] si le tas est vide *)

val is_empty_heap : cost heap -> bool
(** Teste si le tas est vide *)