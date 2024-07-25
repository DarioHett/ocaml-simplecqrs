open Core
open Async
open App
module Api_intf = Api_intf

let make_handle_command service =
  let module Service = (val service : App.Writeservice_intf.S) in
  let handler body = body |> App.Commands.of_string |> Option.map ~f:Service.handle in
  let handle_command _ (body : string) =
    let result = body |> handler in
    result |> Or_error.of_option ~error:(Error.of_string "Not a command.")
  in
  handle_command
;;

let make_handle_query readservice =
  (* let module Repository =
     (val repo : App.Repository_intf.S with type t = Domain.Inventoryitem.t) in *)
  let module ReadService = (val readservice : App.Readservice_intf.S) in
  let handle_query resource =
    let resource = resource |> String.substr_replace_first ~pattern:"/" ~with_:"" in
    (* let q_opt = Queries.of_string resource in *)
    let q_opt = Some (Queries.LatestItemEvent resource) in
    let result_opt =
      match q_opt with
      | Some q -> Some (ReadService.handle q >>| ReadService.to_string)
      | None -> None
    in
    Or_error.of_option result_opt ~error:(Error.of_string "Not a query")
    (* let%map agg_opt = Repository.get_by_id resource in
       let agg_opt =
       Option.map
       ~f:(fun agg -> agg |> Domain.Inventoryitem.yojson_of_t |> Yojson.Safe.to_string)
       agg_opt
       in
       agg_opt |> Or_error.of_option ~error:(Error.of_string "Aggregate not found.") *)
  in
  handle_query
;;

module Api
    (InventoryItemRepo : Repository_intf.S with type t = Domain.Inventoryitem.t)
    (EventBus : Eventbus_intf.S) : Api_intf.S = struct
  module InventoryItemService = Services.InventoryItemService (InventoryItemRepo)
  module LatestEventService = Services.LatestEventService

  (* Routine running on the side. *)
  let () =
    let subcribers =
      EventBus.init ()
      |> EventBus.subscribe ~f:LatestEventService.recv
      |> EventBus.subscribe ~f:(fun e ->
        print_endline (Domain.Events.to_string e);
        return ())
    in
    EventBus.run subcribers ()
  ;;

  let handle_command = make_handle_command (module InventoryItemService)
  let handle_query = make_handle_query (module LatestEventService)
end
