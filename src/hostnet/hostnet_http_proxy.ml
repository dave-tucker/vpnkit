open Lwt
open Cohttp
open Cohttp_lwt_unix

let src =
  let src = Logs.Src.create "proxy" ~doc:"Proxy HTTP Connections" in
  Logs.Src.set_level src (Some Logs.Info);
  src

module Log = (val Logs.src_log src : Logs.LOG)

module Make (S: Cohttp_lwt.Server) = struct

  type s = Conduit_mirage.server -> S.t -> unit Lwt.t

  let handler (ch,conn) req body =
    let uri = Cohttp.Request.uri req in
    (* Log the request to the console *)
    Log.info (fun f -> f "--> %s %s %s\n%!"
                 (Cohttp.(Code.string_of_method (Request.meth req)))
                 (Uri.to_string uri)
                 (Sexplib.Sexp.to_string_hum (Request.sexp_of_t req)));
    (* Strip out hop-by-hop connection headers *)
    let headers =
      Request.headers req |> fun h ->
      Header.remove h "accept-encoding" |> fun h ->
      Header.remove h "content-length" |> fun h ->
      Header.remove h "transfer-encoding" |> fun h ->
      Header.remove h "connection" |> fun h ->
      Header.add h "accept-encoding" "identity"
    in
    (* Fetch the remote URI *)
    let meth = Request.meth req in
    Client.call ~headers ~body meth uri >>= fun (resp, body) ->
    Log.info (fun f -> f "<-- %s %s\n%!"
                 (Uri.to_string (Request.uri req))
                 (Sexplib.Sexp.to_string_hum (Response.sexp_of_t resp)));
    let status = Response.status resp in
    let headers =
      Response.headers resp |> fun h ->
      Header.remove h "transfer-encoding" |> fun h ->
      Header.remove h "content-length" |> fun h ->
      Header.remove h "connection"
    in
    S.respond ~headers ~status ~body ()

  let start http =
    let callback = handler in
    let conn_closed (_,cid) =
      let cid = Cohttp.Connection.to_string cid in
      Log.info (fun f -> f "[%s] closing" cid);
    in
    http (S.make ~conn_closed ~callback)

end
