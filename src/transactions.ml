open Lwt
open Ripple

let main () =
  Tls_lwt.rng_init () >>= fun () ->
  Ripple.with_transactions (Uri.of_string "wss://s-east.ripple.com") >>= fun trs ->
  Lwt_stream.iter (function
      | None -> () (* Frame has no message, dropping *)
      | Some transaction_msg_str ->
          try
            transaction_msg_str |> Yojson.Safe.from_string |>
            WSAPI.Transaction.msg_of_yojson |> function
            | `Error yojson_error -> failwith yojson_error
            | `Ok tr_msg ->
                let open WSAPI.Transaction in
                if tr_msg.validated &&
                   tr_msg.engine_result_code = 0 &&
                   List.mem tr_msg.transaction.ty ["Payment"; "OfferCreate"; "OfferCancel"] &&
                   Transaction.is_trade tr_msg.meta
                then
                  Printf.printf "%s\n\n%!"
                    (Yojson.Safe.prettify ~std:true
                       (msg_to_yojson tr_msg |> Yojson.Safe.to_string))
          with exn ->
            let t = Yojson.Safe.prettify ~std:true transaction_msg_str in
            Printf.printf "%s\n\n%!" t;
            raise exn
    ) trs

let () = Lwt_main.run (main ())
