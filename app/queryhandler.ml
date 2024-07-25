open Core
open Domain
open Queries

let get hashtbl = function
  | LatestItemEvent agg_id -> Hashtbl.find hashtbl agg_id |> Async.return
;;

let set hashtbl event =
  Hashtbl.set hashtbl ~key:(Events.id event) ~data:(Time_float.now (), event)
  |> Async.return
;;
