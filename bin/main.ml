open Infra
open Api
open Core

let () =
  let module API =
    Api
      (Repositories.InMemoryInventoryItemRepoWithEventbus
         (Eventbus.InMemoryEventbus))
         (Eventbus.InMemoryEventbus)
  in
  let serve = Httpserver.run (module API) in
  Async.Command.async
    ~summary:"Entrypoint."
    (let%map_open.Command _ = anon (maybe_with_default "-" ("-" %: string)) in
     fun _ -> serve ())
  |> Command_unix.run
;;
