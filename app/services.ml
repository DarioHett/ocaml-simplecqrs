module InventoryItemService
    (Repository : Repository_intf.S with type t = Domain.Inventoryitem.t) :
  Writeservice_intf.S = struct
  let handle = Commandhandler.handle (module Repository)
end

module LatestEventService :
  Readservice_intf.S with type t = (Core.Time_float.t * Domain.Events.event) option =
struct
  type t = (Core.Time_float.t * Domain.Events.event) option

  let _hashtable = Core.Hashtbl.create (module Core.String)

  let handle = Queryhandler.get _hashtable
  let recv = Queryhandler.set _hashtable

  let to_string t =
    match t with
    | Some (time, event) ->
      Core.Time_float.to_string_utc time ^ ": " ^ Domain.Events.to_string event
    | None -> ""
  ;;
end

module Commands = Commands
module Repository_intf = Repository_intf
