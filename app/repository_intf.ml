open Core
open Async_kernel

module type S = sig
  type t

  val publish : t -> int -> unit Or_error.t Deferred.t
  val get_by_id : string -> t option Deferred.t
end
