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
    begin
      match Frame.opcode frame with
      | `Ping ->
        push (Some (Frame.of_string ~opcode:`Pong ""))
      | _ ->
        Websocket.Frame.content frame
        |> Ripple_api_j.response_of_string
        |> RC.handler
    end
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
  (* High level transaction types *)

  module R = Ripple_api_t

  type node_op =
    | Created
    | Modified
    | Deleted

  type node =
    { op: node_op;
      node_content: R.node
    }

  let nodes_of_tx t =
    let open R in
    let affected_nodes = t.tx_msg_meta.tx_meta_affected_nodes in
    List.map
      (function
        | { created_node=(Some n); modified_node=None; deleted_node=None } -> { op=Created; node_content=n }
        | { created_node=None; modified_node=(Some n); deleted_node=None } -> { op=Modified; node_content=n }
        | { created_node=None; modified_node=None; deleted_node=(Some n) } -> { op=Deleted; node_content=n }
        | _ -> raise (Invalid_argument "is_trade")
      ) affected_nodes

  let offer_nodes_of_tx t =
    let open R in
    let nodes = nodes_of_tx t in
    List.filter (fun {op; node_content} -> node_content.node_ledger_entry_type = "Offer") nodes

  let is_trade t =
    let open R in
    let nodes = nodes_of_tx t in
    let nodes = List.map (fun {op; node_content} -> node_content) nodes in
    List.exists (fun n -> n.node_ledger_entry_type = "Offer") nodes

end

module Ripple = struct
  open Lwt
  open Ripple_api_t

  let ping_handler ~push frame =
    match Websocket.Frame.opcode frame with
    | `Ping ->
        push (Some (Websocket.Frame.of_string ~opcode:`Pong ""));
        None
    | _ ->
        Some frame

  let response_handler frame =
    Websocket.Frame.content frame
  (* TODO: use [response_handler] to transform string to responses
   * defined in ATD file. *)
  (* |> Ripple_api_j.response_of_string *)

  let open_connection uri =
    Websocket.open_connection uri >>= fun (ws_stream, ws_pushfun) ->
    let stream =
      Lwt_stream.filter_map (ping_handler ~push:ws_pushfun) ws_stream
      |> Lwt_stream.map Websocket.Frame.content in
    Lwt.return (stream, ws_pushfun)

  let with_transactions uri =
    open_connection uri >>= fun (stream, pushfun) ->
    pushfun (Some (Websocket.Frame.of_string "{ \"id\": 345, \"command\": \"subscribe\", \"streams\": [\"transactions\"]}"));
    Lwt_stream.next stream >>= fun json_str ->
    match (Ripple_api_j.response_of_string json_str).response_status with
    | `Success -> Lwt.return stream
    | `Error -> Lwt.fail (Failure json_str)

  let with_connection server ripple_handler =
    let handler (stream_ws, push_ws) =
      let stream =
        Lwt_stream.filter_map (ping_handler ~push:push_ws) stream_ws
        |> Lwt_stream.map response_handler
      in
      let push cmd =
        let req = Ripple_api_j.string_of_request { request_id = Random.bits (); request_command = cmd } in
        push_ws (Some (Websocket.Frame.of_string req))
      in
      ripple_handler (stream, push)
    in
    Websocket.with_connection server handler
end
