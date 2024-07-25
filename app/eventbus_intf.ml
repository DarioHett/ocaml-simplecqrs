open Async_kernel
open Domain

module type S = sig
  type t = { subscribers : (Events.event -> unit Deferred.t) list }

  val init : unit -> t
  val subscribe : t -> f:(Events.event -> unit Deferred.t) -> t
  val publish : Events.event -> unit Deferred.t
  val run : t -> unit -> unit
end
