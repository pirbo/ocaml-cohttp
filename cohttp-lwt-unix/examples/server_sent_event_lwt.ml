module ConnMap = Map.Make (Cohttp.Connection) [@@warning "-3"]

let connections = ref ConnMap.empty

let conn_closed (_, conn) =
  connections :=
    ConnMap.update conn
      (function
        | None -> None
        | Some shutdown ->
            shutdown := true;
            None)
      !connections

let clock shutdown =
  let stream, feeder = Lwt_stream.create () in
  let rec aux () =
    if !shutdown then
      let () = feeder None in
      Lwt.return_unit
    else
      let () = feeder (Some "Tic\n") in
      Lwt.bind (Lwt_unix.sleep 1.) aux
  in
  let () = Lwt.ignore_result (aux ()) in
  stream

let callback (_, conn) req _body =
  let uri = Cohttp.Request.uri req in
  match Uri.path uri with
  | "/tic" ->
      let shutdown = ref false in
      let () = connections := ConnMap.add conn shutdown !connections in
      let body = Cohttp_lwt.Body.of_stream (clock shutdown) in
      Cohttp_lwt_unix.Server.respond ~status:`OK ~body ()
  | _ -> Cohttp_lwt_unix.Server.respond_not_found ~uri ()

let () =
  let stop, stopper = Lwt.task () in
  let () =
    Sys.set_signal Sys.sigterm
      (Sys.Signal_handle (fun _ -> Lwt.wakeup_later stopper ()))
  in
  let () = Cohttp_lwt_unix.Debug.activate_debug () in
  Lwt_main.run
    (Cohttp_lwt_unix.Server.create ~stop
       ~mode:(`TCP (`Port 8800))
       (Cohttp_lwt_unix.Server.make ~conn_closed ~callback ()))
