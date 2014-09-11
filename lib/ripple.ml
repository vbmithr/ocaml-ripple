(*
module type RIPPLE_CONFIG = sig
  val server : Uri.t
  val handler : Ripple_api_t.response -> unit Lwt.t
end

module Make = functor (RC : RIPPLE_CONFIG) ->
struct

  (** {2 A Websocket connection} *)

  let rec response_handler ~stream ~push () =
    let open Websocket in
    Lwt_stream.next stream >>= fun frame ->
        Websocket.Frame.content frame
        |> Ripple_api_j.response_of_string
        |> RC.handler
    >>= response_handler ~stream ~push ~handler

  let websocket_connection =
    Websocket.open_connection RC.server >>= (stream, push) ->
    Lwt.async (response_handler ~stream ~push);
    stream, push

  (** {2 Low-level functions} *)

  let send_command cmd =
    websocket_connection >>= fun (_stream, push) ->
    let cmd_str = Ripple_api_t.string_of_command cmd in
    let req = Ripple_api_j.string_of_request { request_command = cmd_str } in
    Lwt.wrap (fun () -> push (Some (Websocket.Frame.of_string req)))

  (** {2 High-level commands} *)

  let server_info () =
    send_command `Server_info

  let server_state () =
    send_command `Server_info

end

module Default_config = struct

  let server =
    Uri.of_string "wss://s-east.ripple.com"

  let handler response =
    let response_str = Ripple_api_j.string_of_response response in
    Lwt_io.printl response_str

end
*)

module Util = struct
  module Option = struct

    let is_none = function
      | None -> true
      | Some _ -> false

    let is_some = function
      | None -> false
      | Some _ -> true

  end

  (* Check if [c] is between A (65) and Z (90) *)
  let ascii_uppercase c =
    let n = Char.code c in
    n >= 65 && n <= 90

  (* Check that [s] is not empty,
     and [f c] is true for every character [c] of [s].
  *)
  let validate_string ~f s =
    s <> "" &&
    try
      String.iter (fun c -> if not (f c) then raise Exit) s;
      true
    with Exit ->
      false

  let validate_currency = function
    | `Xrp -> true
    (* Currency code must be of the form 'XXX' *)
    | `Code x ->
        if String.length x = 3 then
          validate_string ~f:ascii_uppercase x
        else
          false
end

module Transaction = struct
  open WSAPI
  (* High level transaction types *)

  type node_op =
    | Created
    | Modified
    | Deleted

  type node =
    { op: node_op;
      node_content: Node.t
    }

  let nodes_of_tx t =
    let open Node in
    let affected_nodes = t.Transaction.affected_nodes in
    List.map
      (function
        | { created=(Some n); modified=None; deleted=None } ->
            { op=Created; node_content=n }
        | { created=None; modified=(Some n); deleted=None } ->
            { op=Modified; node_content=n }
        | { created=None; modified=None; deleted=(Some n) } ->
            { op=Deleted; node_content=n }
        | _ -> raise (Invalid_argument "is_trade")
      ) affected_nodes

  let offer_nodes_of_tx t =
    let open Node in
    let nodes = nodes_of_tx t in
    List.filter
      (fun {op; node_content} ->
         node_content.ledger_entry_type = "Offer")
      nodes

  let is_trade t =
    let open Node in
    let nodes = nodes_of_tx t in
    let nodes = List.map (fun {op; node_content} -> node_content) nodes in
    List.exists (fun n -> n.ledger_entry_type = "Offer") nodes
end

module Ripple = struct
  open WSAPI

  exception Yojson_error of string
  exception Ripple_error of string

  open Lwt

  let response_handler frame =
  (* TODO: use [response_handler] to transform string to responses *)
    match Websocket.Frame.content frame with
    | None -> raise (Ripple_error "frame with no content")
    | Some c -> c

  let open_connection uri =
    let open Websocket in
    open_connection uri >>= fun (ws_stream, ws_pushfun) ->
    let stream = Lwt_stream.map Frame.content ws_stream in
    Lwt.return (stream, ws_pushfun)

  let with_transactions uri =
    open_connection uri >>= fun (stream, pushfun) ->
    let content = Subscribe.(make ~streams:["transactions"] () |>
                             to_yojson |> Yojson.Safe.to_string) in
    Printf.printf "%s\n%!" content;
    pushfun (Some (Websocket.Frame.of_string ~content ()));
    Lwt_stream.next stream >>= function
    | None ->
        fail Not_found (* Frame has no content, should not happen *)
    | Some json_str ->
        Printf.printf "%s\n%!" json_str;
        let json = Yojson.Safe.from_string json_str in
        match Response.(of_yojson json) with
        | `Error yojson_error -> fail (Yojson_error yojson_error)
        | `Ok resp ->
            match Response.wrap resp with
            | `Error ripple_error -> fail (Ripple_error ripple_error)
            | `Ok -> return stream

  let with_connection server ripple_handler =
    let handler (ws_stream, ws_pushfun) =
      let stream = Lwt_stream.map response_handler ws_stream
      in
      let push command =
        let req_yojson =
          Request.(to_yojson { id = Random.bits (); command; }) in
        let req = Yojson.Safe.to_string req_yojson in
        ws_pushfun (Some (Websocket.Frame.of_string ~content:req ()))
      in
      ripple_handler (stream, push)
    in
    Websocket.with_connection server handler
end
