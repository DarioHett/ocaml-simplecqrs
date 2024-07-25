open Core
open Async

let headers =
  Cohttp.Header.add_list
    (Cohttp.Header.init ())
    [ "Content-Type", "text/plain"; "Access-Control-Allow-Origin", "*" ]
;;

let run api () =
  let module Body = Cohttp_async.Body in
  let module Server = Cohttp_async.Server in
  let module API = (val api : Api.Api_intf.S) in
  let host_and_port = Cohttp_async.Server.create
       ~on_handler_error:`Raise
       (Async.Tcp.Where_to_listen.of_port 8000)
       (fun ~body _ req ->
          match req |> Cohttp.Request.meth with
          | `POST ->
            let resource = Cohttp.Request.resource req in
            let%bind body = Body.to_string body in
            let result = API.handle_command resource body in
            (match result with
             | Ok _ -> Server.respond `OK ~body:(`String "Command dispatched.") ~headers
             | Error msg ->
               Server.respond
                 `Not_acceptable
                 ~body:(`String (Error.to_string_hum msg))
                 ~headers)
          | `GET ->
            let resource = Cohttp.Request.resource req in
            (match API.handle_query resource with
             | Error err ->
               Server.respond `OK ~body:(`String (Error.to_string_hum err)) ~headers
             | Ok agg ->
               let%bind act_agg = agg in
               Server.respond `OK ~body:(`String act_agg) ~headers)
          | _ -> Server.respond `Method_not_allowed) in 
    ignore(host_and_port);
    Deferred.never ()
;;
