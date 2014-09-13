module Fields = struct
  type t = {
    account [@key "Account"] : string option;
    book_directory [@key "BookDirectory"] : string option;
    book_node [@key "BookNode"] : string option;
    flags [@key "Flags"] : int option;
    owner_node [@key "OwnerNode"] : string option;
    previous_txn_id [@key "PreviousTxnID"] : string option;
    previous_txn_lgr_seq [@key "PreviousTxnLgrSeq"] : int option;
    sequence [@key "Sequence"] : int option;
    taker_gets [@key "TakerGets"] : Yojson.Safe.json option;
    taker_pays [@key "TakerPays"] : Yojson.Safe.json option;
    ledger_entry_type [@key "LedgerEntryType"] : string option;
    ledger_index [@key "LedgerIndex"] : string option;
  } [@@deriving Yojson]
end

module Node = struct
  type t = {
    ledger_entry_type [@key "LedgerEntryType"] : string;
    ledger_index [@key "LedgerIndex"] : string;
    final_fields [@key "FinalFields"] : Fields.t option;
    previous_fields [@key "PreviousFields"] : Fields.t option;
    previous_txn_id [@key "PreviousTxnID"] : string option;
    previous_txn_lgr_seq [@key "PreviousTxnLgrSeq"] : int option;
  } [@@deriving Yojson]

  type affected = {
    created [@key "CreatedNode"] : t option;
    modified [@key "ModifiedNode"] : t option;
    deleted [@key "DeletedNode"] : t option;
  } [@@deriving Yojson]
end

module Request = struct
  type t = {
    id: int;
    command: string;
  } [@@deriving Yojson]
end

module Response = struct
  type t = {
    id : int;
    result: Yojson.Safe.json;
    ty [@key "type"]: string;
    status : string;                        (* "success" or "error" *)
    error : string option [@default None];  (* If an error happens *)
    error_message : string option [@default None];
    request : Request.t option [@default None];
  } [@@deriving Yojson]

  let wrap r = match r.status, r.error with
    | "success", _ -> `Ok
    | "error", Some msg -> `Error msg
    | _ -> failwith "wrap"
end

module Transaction = struct
  type meta = {
    affected_nodes [@key "AffectedNodes"] : Node.affected list;
    transaction_index [@key "TransactionIndex"] : int;
    transactionResult [@key "TransactionResult"] : string;
  } [@@deriving Yojson]

  (* https://ripple.com/wiki/Transactions#Basic_Transaction_Format *)
  type t = {
    hash : string;
    ty [@key "TransactionType"] : string; (* UInt16 *)
    flags [@key "Flags"] : int option; (* UInt32 *)
    account [@key "Account"] : string;
    amount [@key "Amount"] : Yojson.Safe.json option;
    destination [@key "Destination"] : string option;
    fee [@key "Fee"] : string;
    invoice_id [@key "InvoiceID"] : string option;
    sequence [@key "Sequence"] : int;
    signing_pub_key [@key "SigningPubKey"] : string;
  } [@@deriving Yojson]

  type msg = {
    engine_result_code: int;
    ledger_hash: string;
    ledger_index: int;
    meta: meta;
    transaction: t;
    ty [@key "type"] : string;
    validated: bool;
  } [@@deriving Yojson]
end

module Subscribe = struct
  type t = {
    id: int;
    command: string;
    streams: string list option [@default None];
    accounts: string list option [@default None];
    accounts_proposed: string list option [@default None];
  } [@@deriving Yojson]

  let make ?(id=Random.bits ()) ?streams () =
    {
      id;
      command="subscribe";
      streams;
      accounts=None;
      accounts_proposed=None;
    }
end
