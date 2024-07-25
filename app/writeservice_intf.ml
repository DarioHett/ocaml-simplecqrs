module type S = sig
  val handle : Commands.command -> unit Async_kernel.Deferred.t
end
