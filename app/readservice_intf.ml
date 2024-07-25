module type S = sig
  type t

  val handle : Queries.query -> t Async.Deferred.t
  val recv : Domain.Events.event -> unit Async.Deferred.t
  val to_string : t -> string
end
