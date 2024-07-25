module type S = sig
  include Aggregateroot_intf.S

  val init : unit -> t
  val create : string -> string -> t
  val change_name : t -> string -> t
  val remove : t -> int -> t
  val check_in : t -> int -> t
  val deactivate : t -> t
end
