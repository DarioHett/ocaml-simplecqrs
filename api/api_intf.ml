module type S = sig
  val handle_command : string -> string -> unit Async.Deferred.t Core.Or_error.t
  val handle_query : string -> string Async.Deferred.t Core.Or_error.t
end
