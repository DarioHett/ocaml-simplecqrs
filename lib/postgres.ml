open! Core
open Async_kernel


let query_exn postgres string =
  let%bind result = Postgres_async.query_expect_no_data postgres string in
  match result with
  | Error _ -> return ()
  | Ok _ -> return ()
;;

let receive_notifications ?bvar  postgres ~channel =
  let%bind result =
    Postgres_async.listen_to_notifications postgres ~channel ~f:(fun ~pid:_ ~payload ->
      Option.iter bvar ~f:(fun bv -> Bvar.broadcast bv payload))
  in
  Or_error.ok_exn result;
  return ()
;;
