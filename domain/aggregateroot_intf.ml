open Events

module type S = sig
  type t

  val get_uncommited_changes : t -> event list
  val mark_changes_as_committed : t -> t
  val apply_change : t -> event -> t
  val load : event list -> t
  val init : unit -> t
end
