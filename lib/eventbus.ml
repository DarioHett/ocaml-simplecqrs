open Events
open Core

type t = { subscribers : (event -> unit) list }

let subscribe t subscriber = { subscribers = List.append t.subscribers [ subscriber ] }

let publish t events =
  List.iter events ~f:(fun evt ->
    List.iter t.subscribers ~f:(fun subscriber -> subscriber evt))
;;

let init () = { subscribers = [] }
