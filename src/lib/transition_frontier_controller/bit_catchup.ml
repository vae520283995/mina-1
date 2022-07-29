open Mina_block.Validation
open Mina_base
open Core_kernel
open Async_kernel

let ancestry_download_timeout = Time_ns.Span.of_sec 30.

let bitwap_download_timeout = Time_ns.Span.of_min 2.

let peer_download_timeout = Time_ns.Span.of_min 2.

type transition_gossip_t =
  | Not_a_gossip
  | Gossiped_header of Mina_net2.Validation_callback.t
  | Gossiped_block of Mina_net2.Validation_callback.t
  | Gossiped_both of
      { block_vc : Mina_net2.Validation_callback.t
      ; header_vc : Mina_net2.Validation_callback.t
      }

type verifying_chain_status =
  | Verifying_and_waiting_for_base of State_hash.t
  | Verifying
  | Waiting_for_base of State_hash.t

type body_download_mode =
  | Subordinate of
      { (* Descendants to notify when download is finished *)
        descendants_awaiting : State_hash.t list
      }
  | Major of
      { ancestors_downloading : State_hash.Set.t
      ; descendants_awaiting : State_hash.t list
      }

type received_header =
  | Pre_initial_valid of pre_initial_valid_with_header
  | Initial_valid of initial_valid_with_header

let header_with_hash_of_received_header h =
  match h with
  | Pre_initial_valid h ->
      Mina_block.Validation.header_with_hash h
  | Initial_valid h ->
      Mina_block.Validation.header_with_hash h

type ancestry_status =
  | Processing
  | Processed (* of (timeout, interruptible_action) *)
  | Waiting_for_ancestry

type ancestry_substate =
  { children_awaiting : State_hash.t list
  ; status : ancestry_status
  ; received_via_gossip : bool
  }

type download_phase = Bitswap | Peers

module Transition_state = struct
  type t =
    (* Important: because a received header is considered to be retrieved via
       download, it's assumed to always have parent in frontier/transition states *)
    | Received of
        { header : received_header
        ; ancestry_substate : ancestry_substate
        ; gossip_data : transition_gossip_t
        ; body_opt : Staged_ledger_diff.Body.t option
        }
    | Verifying_blockchain_proof of
        { header : received_header
        ; gossip_data : transition_gossip_t
        ; body_opt : Staged_ledger_diff.Body.t option
        ; ancestry_substate : ancestry_substate
        }
    | Downloading_body of
        { header : initial_valid_with_header
        ; block_vc : Mina_net2.Validation_callback.t option
        ; download_mode : download_phase
        ; ancestry_substate : ancestry_substate
        }
    | Verifying_complete_works of
        { block : initial_valid_with_block
        ; block_vc : Mina_net2.Validation_callback.t option
        ; ancestry_substate : ancestry_substate
        }
    | Waiting_for_parent_added_to_frontier of
        { block : initial_valid_with_block
        ; block_vc : Mina_net2.Validation_callback.t option
        }
    | Building_breadcrumb of
        { block : initial_valid_with_block
        ; block_vc : Mina_net2.Validation_callback.t option
        }
    | Waiting_to_be_added_to_frontier of
        Frontier_base.Breadcrumb.t * [ `Local | `Remote ]

  module Enum = struct
    type t =
      | Received
      | Verifying_blockchain_proof
      | Downloading_body
      | Verifying_complete_works
      | Waiting_for_parent_added_to_frontier
      | Building_breadcrumb
      | Waiting_to_be_added_to_frontier
    [@@deriving to_yojson, compare, equal]
  end

  let to_enum st =
    match st with
    | Received _ ->
        Enum.Received
    | Verifying_blockchain_proof _ ->
        Enum.Verifying_blockchain_proof
    | Downloading_body _ ->
        Enum.Downloading_body
    | Verifying_complete_works _ ->
        Enum.Verifying_complete_works
    | Waiting_for_parent_added_to_frontier _ ->
        Enum.Waiting_for_parent_added_to_frontier
    | Building_breadcrumb _ ->
        Enum.Building_breadcrumb
    | Waiting_to_be_added_to_frontier _ ->
        Enum.Waiting_to_be_added_to_frontier

  let parent_hash st =
    let header_to_parent_hash =
      Fn.compose Mina_state.Protocol_state.previous_state_hash
        Mina_block.Header.protocol_state
    in
    match st with
    | Received { header; _ } | Verifying_blockchain_proof { header; _ } ->
        header_with_hash_of_received_header header
        |> With_hash.data |> header_to_parent_hash
    | Downloading_body { header; _ } ->
        Mina_block.Validation.header_with_hash header
        |> With_hash.data |> header_to_parent_hash
    | Verifying_complete_works { block; _ }
    | Waiting_for_parent_added_to_frontier { block; _ }
    | Building_breadcrumb { block; _ } ->
        Mina_block.Validation.block_with_hash block
        |> With_hash.data |> Mina_block.header |> header_to_parent_hash
    | Waiting_to_be_added_to_frontier (breadcrumb, _) ->
        Frontier_base.Breadcrumb.parent_hash breadcrumb
end

module Transition_state_extended = struct
  type t =
    | Processing of Transition_state.t
    | Invalid of { header : Mina_block.Header.with_hash; error : Error.t }
    | Failure of { previous_state : Transition_state.t; error : Error.t }

  module Enum = struct
    type t =
      | Processing of Transition_state.Enum.t
      | Invalid
      | Failure of { previous_state : Transition_state.Enum.t }
    [@@deriving to_yojson]
  end

  let to_enum st_ext =
    match st_ext with
    | Processing st ->
        Enum.Processing (Transition_state.to_enum st)
    | Failure { previous_state = st; _ } ->
        Enum.Failure { previous_state = Transition_state.to_enum st }
    | Invalid _ ->
        Invalid
end

module Event = struct
  type t =
    | Success
    | Failure
    | Invalid
    | Timeout
    | Received_header_through_gossip of initial_valid_with_header
    | Received_block_through_gossip of initial_valid_with_block
    | Downloaded_ancestry_chain of Mina_block.Header.t list
    | Downloaded_body of Staged_ledger_diff.Body.t
    | Breadcrumb_built of Frontier_base.Breadcrumb.t

  module Enum = struct
    type t =
      | Success
      | Failure
      | Invalid
      | Timeout
      | Received_header_through_gossip
      | Received_block_through_gossip
      | Downloaded_ancestry_chain
      | Downloaded_body
      | Breadcrumb_built
    [@@deriving to_yojson]
  end

  let to_enum ev =
    match ev with
    | Success ->
        Enum.Success
    | Timeout ->
        Enum.Timeout
    | Failure ->
        Enum.Failure
    | Invalid ->
        Enum.Invalid
    | Received_header_through_gossip _ ->
        Enum.Received_header_through_gossip
    | Received_block_through_gossip _ ->
        Enum.Received_block_through_gossip
    | Downloaded_ancestry_chain _ ->
        Enum.Downloaded_ancestry_chain
    | Downloaded_body _ ->
        Enum.Downloaded_body
    | Breadcrumb_built _ ->
        Enum.Breadcrumb_built
end

type node_state_t = { transition_state : Transition_state.t }

type catchup_state =
  { transition_states : Transition_state_extended.t State_hash.Table.t
        (* Map from parent to list of children for children whose parent
           is not in the transition states *)
  ; orphans : State_hash.t list State_hash.Table.t
  ; (* Consider using priority queue with enum *)
    processing_queue : (State_hash.t * Event.t) Queue.t
  ; (* Here we need priority queue *)
    timeout_queue : (Time_ns.t * State_hash.t) Queue.t
  }

let verify_header_is_relevant ~logger ~consensus_constants ~frontier
    ~trust_system ~sender ~time_controller ~state header_with_hash =
  let open Transition_handler.Validator in
  let hash = State_hash.With_state_hashes.state_hash header_with_hash in
  let relevance_result =
    let open Result.Let_syntax in
    let%bind () =
      Option.value_map (Hashtbl.find state.transition_states hash)
        ~default:(Ok ()) ~f:(fun st -> Error (`In_process st))
    in
    verify_header_is_relevant ~consensus_constants ~logger ~frontier
      header_with_hash
  in
  let record_irrelevant error =
    don't_wait_for
    @@ record_transition_is_irrelevant ~logger ~trust_system ~sender ~error
         header_with_hash
  in
  (* This action is deferred because it may potentially trigger change of ban status
     of a peer which requires writing to a synchonous pipe. *)
  (* Although it's not evident from types, banning may be triiggered only for irrelevant
     case, hence it's safe to do don't_wait_for *)
  match relevance_result with
  | Ok () ->
      don't_wait_for
        (record_transition_is_relevant ~logger ~trust_system ~sender
           ~time_controller header_with_hash ) ;
      `Relevant
  | Error (`In_process (Transition_state_extended.Processing _) as error) ->
      record_irrelevant error ; `Preserve_gossip_data
  | Error error ->
      record_irrelevant error ; `Irrelevant

let update_gossip_data ~logger ~hash ~vc ~gossip_type old =
  let log_duplicate () =
    [%log warn] "Duplicate %s gossip for $state_hash"
      (match gossip_type with `Block -> "block" | `Header -> "header")
      ~metadata:[ ("state_hash", State_hash.to_yojson hash) ]
  in
  match (gossip_type, old) with
  | `Block, Gossiped_header header_vc ->
      Gossiped_both { block_vc = vc; header_vc }
  | `Header, Gossiped_block block_vc ->
      Gossiped_both { block_vc; header_vc = vc }
  | `Block, Not_a_gossip ->
      Gossiped_block vc
  | `Header, Not_a_gossip ->
      Gossiped_header vc
  | `Header, Gossiped_header _ ->
      log_duplicate () ; old
  | `Header, Gossiped_both _ ->
      log_duplicate () ; old
  | `Block, Gossiped_block _ ->
      log_duplicate () ; old
  | `Block, Gossiped_both _ ->
      log_duplicate () ; old

let preserve_relevant_gossip ~logger ~hash ~body_opt ~gossip_type ~vc_opt st_ext
    =
  let update_gossip_data =
    Option.value_map ~default:ident
      ~f:(fun vc -> update_gossip_data ~logger ~hash ~vc ~gossip_type)
      vc_opt
  in
  let update_body_opt = Option.first_some body_opt in
  let update_block_vc =
    match (gossip_type, vc_opt) with
    | `Block, Some vc ->
        Option.(Fn.compose some @@ value ~default:vc)
    | _ ->
        ident
  in
  let fire_callback =
    Option.value_map ~f:Mina_net2.Validation_callback.fire_if_not_already_fired
      ~default:ignore vc_opt
  in
  let fire_callback_for_header =
    match gossip_type with `Block -> ignore | `Header -> fire_callback
  in
  let open Transition_state in
  let open Transition_state_extended in
  let handle_ext handle =
    match st_ext with
    | Failure _ ->
        fire_callback `Ignore ;
        st_ext
    | Invalid _ ->
        fire_callback `Reject ;
        st_ext
    | Processing st ->
        Processing (handle st)
  in
  handle_ext
  @@ fun st ->
  match st with
  | Received ({ gossip_data; body_opt; _ } as r) ->
      Received
        { r with
          gossip_data = update_gossip_data gossip_data
        ; body_opt = update_body_opt body_opt
        }
  | Verifying_blockchain_proof ({ gossip_data; body_opt; _ } as r) ->
      Verifying_blockchain_proof
        { r with
          gossip_data = update_gossip_data gossip_data
        ; body_opt = update_body_opt body_opt
        }
  | Downloading_body ({ block_vc; _ } as r) ->
      fire_callback_for_header `Accept ;
      Downloading_body { r with block_vc = update_block_vc block_vc }
  | Verifying_complete_works ({ block_vc; _ } as r) ->
      fire_callback_for_header `Accept ;
      Verifying_complete_works { r with block_vc = update_block_vc block_vc }
  | Waiting_for_parent_added_to_frontier ({ block_vc; _ } as r) ->
      fire_callback_for_header `Accept ;
      Waiting_for_parent_added_to_frontier
        { r with block_vc = update_block_vc block_vc }
  | Building_breadcrumb ({ block_vc; _ } as r) ->
      fire_callback_for_header `Accept ;
      Building_breadcrumb { r with block_vc = update_block_vc block_vc }
  | Waiting_to_be_added_to_frontier _ ->
      fire_callback `Accept ;
      st

let to_gossip_data ?gossip_type vc_opt =
  Option.value ~default:Not_a_gossip
  @@ let%bind.Option gt = gossip_type in
     let%map.Option vc = vc_opt in
     match gt with `Header -> Gossiped_header vc | `Block -> Gossiped_block vc

(* let make_downloading_ancestry_bodies ~frontier ~gossip_type ~vc_opt ~body
       ~parent header_with_validation =
     (* TODO descend until encounter with one of:
           1. >= Verifying_complete_works
           2. Downloading_ancestry_bodies (then reuse ancestors_downloading)
           3. In frontier
           4. Downloading body, Major
     *)
     (Transition_state_extended.Processing (failwith ""), None)

   let make_download_body ~logger ~frontier ~gossip_type ~vc_opt ~body_opt ~parent
       header_with_validation =
       let hash = Mina_block.Validation.header_with_hash header_with_validation |> State_hash.With_state_hashes.state_hash in
       let update_gossip_data =
         preserve_relevant_gossip ~logger ~hash ~body_opt:None ~gossip_type ~vc_opt in
     match body_opt with
     | Some body ->
         make_downloading_ancestry_bodies ~frontier ~gossip_type ~vc_opt ~body
           ~parent header_with_validation
     | None ->
       let open Transition_state in
       let (>=) a b = Transition_state.Enum.compare a b >= 0 in
         let ancestors_downloading, launch_download =
           match parent with
           | `In_frontier _ ->
             State_hash.Set.empty, []
           | `In_process (Transition_state_extended.Processing st)
           | `In_process (Transition_state_extended.Failure {previous_state=st})
           (* Parent's chain has all bodies *)
             when (Transition_state.to_enum st >= Verifying_complete_works) ->
             State_hash.Set.empty, []
           (* Pre-condition: Verifying_ancestry_chain < st < Verifying_complete_works  *)
         (* | Downloading_body_from_bitswap
         | Downloading_body_from_peers
         | Downloading_ancestry_bodies
         | Waiting_for_complete_work_verification *)
             in
         (* TODO launch downloading of block *)
         (* TODO launch download of descendants *)
         ( update_gossip_data @@
            Processing
             (Downloading_body_from_bitswap
                { header = header_with_validation
                ; block_vc = None
                ; timeout_at =
                    Time_ns.add (Time_ns.now ()) ancestry_download_timeout
                ; download_mode = Major {
                 descendants_awaiting = []
                 ; ancestors_downloading
                }
                } )
         , None )

   let add_download_ancestry ~logger ~frontier ~transition_states ~gossip_type ~vc_opt
       ~body_opt header_with_validation =
     let header_with_hash =
       Mina_block.Validation.header_with_hash header_with_validation
     in
     let header = With_hash.data header_with_hash in
     let open Transition_state_extended in
     let hash = State_hash.With_state_hashes.state_hash header_with_hash in
     let parent_hash =
       Mina_block.Header.protocol_state header
       |> Mina_state.Protocol_state.previous_state_hash
     in
     let mk_download_ancestry () =
       Processing
         (Downloading_ancestry
            { header = header_with_validation
            ; gossip_data = to_gossip_data ~gossip_type vc_opt
            ; body_opt
            ; timeout_at = Time_ns.add (Time_ns.now ()) ancestry_download_timeout
            ; children_awaiting = []
            } )
     in
     let mk_verifying_ancestry status =
       Processing
         (Verifying_ancestry_chain
            { header = header_with_validation
            ; gossip_data = to_gossip_data ~gossip_type vc_opt
            ; body_opt
            ; status
            ; descendants_awaiting = []
            } )
     in
     let launch_downloading_ancestry parent_st_ext =
       match parent_st_ext with
       | None ->
           (* TODO Launch downloading of ancestry *)
           (mk_download_ancestry (), None)
       | Some (Processing (Downloading_ancestry st)) ->
           (* Not launching download because we can just rely
              on the outcome of the parent for both download and verification *)
           ( mk_download_ancestry ()
           , Some
               (Processing
                  (Downloading_ancestry
                     { st with children_awaiting = hash :: st.children_awaiting }
                  ) ) )
       | Some (Processing (Verifying_ancestry_chain st)) ->
           ( mk_verifying_ancestry (Waiting_for_base parent_hash)
           , Some
               (Processing
                  (Verifying_ancestry_chain
                     { st with
                       descendants_awaiting = hash :: st.descendants_awaiting
                     } ) ) )
       | Some (Processing (Received st)) ->
           (* TODO Launch verifying of ancestry for headers from parent until the first header that is > Received *)
           (* TODO status might be different *)
           ( mk_verifying_ancestry Verifying
           , Some
               (Processing
                  (Received
                     { st with
                       children_awaiting_verifying_ancestry =
                         hash :: st.children_awaiting_verifying_ancestry
                     } ) ) )
       | Some (Processing _) ->
           failwith "launch_downloading_ancestry: unexpected processing case"
       | Some (Failure { previous_state = Downloading_ancestry st; _ }) ->
           (* TODO Launch downloading of ancestry for parent *)
           ( mk_download_ancestry ()
           , Some
               (Processing
                  (Downloading_ancestry
                     { st with children_awaiting = hash :: st.children_awaiting }
                  ) ) )
       | Some (Failure { previous_state = Verifying_ancestry_chain st; _ }) ->
           (* TODO Launch verifying of ancestry for parent *)
           (* TODO status might be different *)
           ( mk_verifying_ancestry (Waiting_for_base parent_hash)
           , Some
               (Processing
                  (Verifying_ancestry_chain
                     { st with
                       descendants_awaiting = hash :: st.descendants_awaiting
                     } ) ) )
       | Some (Failure _) ->
           failwith "launch_downloading_ancestry: unexpected failure case"
       | Some (Invalid _) ->
           failwith "launch_downloading_ancestry: unexpected invalid case"
     in
     let state, parent_state_opt =
       match
         ( Hashtbl.find transition_states parent_hash
         , Transition_frontier.find frontier parent_hash )
       with
       | _, Some b ->
           make_download_body ~logger ~frontier ~gossip_type ~vc_opt ~body_opt
             ~parent:(`In_frontier b) header_with_validation
       | None, None ->
           launch_downloading_ancestry None
       | Some (Invalid { error; _ }), None ->
           (Invalid { error; header = header_with_hash }, None)
       | Some (Processing st as st_ext), None
       | Some (Failure { previous_state = st; _ } as st_ext), None ->
           let st_enum = Transition_state.to_enum st in
           let ( > ) a b = Transition_state.Enum.compare a b > 0 in
           let open Transition_state.Enum in
           if st_enum > Verifying_ancestry_chain then
             make_download_body ~logger ~frontier ~gossip_type ~vc_opt ~body_opt
               ~parent:(`In_process st_ext) header_with_validation
           else launch_downloading_ancestry (Some st_ext)
     in
     Hashtbl.set transition_states ~key:hash ~data:state ;
     Option.iter parent_state_opt ~f:(fun data ->
         Hashtbl.set ~key:hash ~data transition_states ) *)

let split_eithers ls =
  let f, s =
    List.unzip
    @@ List.map ls ~f:(fun e ->
           match e with First a -> (Some a, None) | Second a -> (None, Some a) )
  in
  (List.filter_opt f, List.filter_opt s)

let get_substate st =
  match st with
  | Transition_state.Received { ancestry_substate; _ } ->
      Some ancestry_substate
  | Verifying_blockchain_proof { ancestry_substate; _ } ->
      Some ancestry_substate
  | Downloading_body { ancestry_substate; _ } ->
      Some ancestry_substate
  | Verifying_complete_works { ancestry_substate; _ } ->
      Some ancestry_substate
  | _ ->
      None

let update_substate st ancestry_substate =
  match st with
  | Transition_state.Received r ->
      Transition_state.Received { r with ancestry_substate }
  | Verifying_blockchain_proof r ->
      Transition_state.Verifying_blockchain_proof { r with ancestry_substate }
  | Downloading_body r ->
      Transition_state.Downloading_body { r with ancestry_substate }
  | Verifying_complete_works r ->
      Transition_state.Verifying_complete_works { r with ancestry_substate }
  | _ ->
      st

let decide_mark_processed ~state ~frontier st_enum parent_hash =
  match Hashtbl.find state.transition_states parent_hash with
  | None ->
      if Option.is_some (Hashtbl.find frontier parent_hash) then `Next_state
      else `New_status
  | Some (Transition_state_extended.Invalid r) ->
      `Parent_invalid r.error
  | Some (Transition_state_extended.Failure { previous_state = parent_st; _ })
  | Some (Transition_state_extended.Processing parent_st) ->
      let ( > ) a b = Transition_state.Enum.compare a b > 0 in
      if Transition_state.to_enum parent_st > st_enum then `Next_state
      else `New_status

let mark_processed ~state ~frontier hash =
  let handle_st ~update_state st =
    match get_substate st with
    | Some ({ status = Processing; _ } as subst) -> (
        match
          decide_mark_processed ~state ~frontier
            (Transition_state.to_enum st)
            (Transition_state.parent_hash st)
        with
        | `Next_state ->
            Queue.enqueue state.processing_queue (hash, Success)
        | `Parent_invalid _ ->
            Queue.enqueue state.processing_queue (hash, Invalid)
        | `New_status ->
            let status =
              if subst.received_via_gossip then Waiting_for_ancestry
              else Processed
            in
            update_state (update_substate st { subst with status }) )
    | _ ->
        ( (* unexpected state, TODO log ? *) )
  in
  let st_ext = Hashtbl.find_exn state.transition_states hash in
  match st_ext with
  | Transition_state_extended.Processing st ->
      handle_st st ~update_state:(fun st' ->
          Hashtbl.set state.transition_states ~key:hash
            ~data:(Transition_state_extended.Processing st') )
  | Failure ({ previous_state = st; _ } as r) ->
      handle_st st ~update_state:(fun st' ->
          Hashtbl.set state.transition_states ~key:hash
            ~data:
              (Transition_state_extended.Failure { r with previous_state = st' }) )
  | Invalid _ ->
      ()

let add_received ~logger ~frontier ~state ?gossip_type ?vc ?body received_header
    =
  let header_with_hash = header_with_hash_of_received_header received_header in
  let header = With_hash.data header_with_hash in
  let open Transition_state_extended in
  let hash = State_hash.With_state_hashes.state_hash header_with_hash in
  let parent_hash =
    Mina_block.Header.protocol_state header
    |> Mina_state.Protocol_state.previous_state_hash
  in
  let children = Option.value ~default:[] @@ Hashtbl.find state.orphans hash in
  Hashtbl.remove state.orphans hash ;
  let children_processed, children_waiting_for_ancestry =
    split_eithers
    @@ List.filter_map children ~f:(fun child ->
           let%bind.Option st_ext =
             Hashtbl.find state.transition_states child
           in
           match st_ext with
           | Transition_state_extended.Processing st
           | Failure { previous_state = st; _ } -> (
               let%bind.Option subst = get_substate st in
               match subst.status with
               | Processing ->
                   None
               | Processed ->
                   Some (First child)
               | Waiting_for_ancestry ->
                   Some (Second child) )
           | Invalid _ ->
               None )
  in
  Hashtbl.add_exn state.transition_states ~key:hash
    ~data:
      (Processing
         (Received
            { body_opt = body
            ; header = received_header
            ; gossip_data = to_gossip_data ?gossip_type vc
            ; ancestry_substate =
                { children_processed
                ; children_waiting_for_ancestry
                ; received_via_gossip = Option.is_some gossip_type
                ; status = Processing
                }
            } ) )
(* TODO mark processed *)

let handle_gossip ~logger ~consensus_constants ~frontier ~trust_system
    ~time_controller ~state ~sender ?body ~gossip_type ?vc
    header_with_validation =
  (* TODO try to retrieve body from DB *)
  let header_with_hash =
    Mina_block.Validation.header_with_hash header_with_validation
  in
  let hash = State_hash.With_state_hashes.state_hash header_with_hash in
  let relevance_status =
    verify_header_is_relevant ~logger ~consensus_constants ~frontier
      ~trust_system ~sender ~time_controller ~state header_with_hash
  in
  let open Transition_state in
  let open Transition_state_extended in
  match relevance_status with
  | `Relevant ->
      add_received ~logger ~frontier ~transition_states:state.transition_states
        ~gossip_type ?vc ?body header_with_validation
  | `Preserve_gossip_data ->
      Hashtbl.update state.transition_states hash ~f:(fun st_opt ->
          let st = Option.value_exn st_opt in
          ( match st with
          | Processing (Transition_state.Received _) ->
              Queue.enqueue state.processing_queue
                ( hash
                , Option.value_map body_opt
                    ~default:
                      (Event.Received_header_through_gossip
                         header_with_validation ) ~f:(fun body ->
                      Received_block_through_gossip
                        (Mina_block.Validation.with_body header_with_validation
                           body ) ) )
          | Processing (Downloading_body_from_bitswap _)
          | Processing (Downloading_body_from_peers _) ->
              Option.iter body_opt ~f:(fun body ->
                  Queue.enqueue state.processing_queue
                    ( hash
                    , Received_block_through_gossip
                        (Mina_block.Validation.with_body header_with_validation
                           body ) ) )
          | _ ->
              () ) ;
          preserve_relevant_gossip ~logger ~hash ~body_opt ~vc_opt ~gossip_type
            st )
  | `Irrelevant ->
      ()

let handle_collected_transition ~logger ~consensus_constants ~frontier
    ~trust_system ~time_controller ~state (b_or_h_env, vc_opt) =
  let sender = Network_peer.Envelope.Incoming.sender b_or_h_env in
  let header_with_validation, body_opt, gossip_type =
    match Network_peer.Envelope.Incoming.data b_or_h_env with
    | Bootstrap_controller.Transition_cache.Block block ->
        ( Mina_block.Validation.to_header block
        , Some (Mina_block.body @@ Mina_block.Validation.block block)
        , `Block )
    | Bootstrap_controller.Transition_cache.Header header ->
        (header, None, `Header)
  in
  handle_gossip ~logger ~consensus_constants ~frontier ~trust_system
    ~time_controller ~state ~sender ~body_opt ~gossip_type
    header_with_validation vc_opt

let handle_network_transition ~logger ~consensus_constants ~frontier
    ~trust_system ~time_controller ~state (b_or_h, `Valid_cb vc_opt) =
  let sender, header_with_validation, body_opt, gossip_type =
    match b_or_h with
    | `Block b_env ->
        let block = Network_peer.Envelope.Incoming.data b_env in
        ( Network_peer.Envelope.Incoming.sender b_env
        , Mina_block.Validation.to_header block
        , Some (Mina_block.body @@ Mina_block.Validation.block block)
        , `Block )
    | `Header h_env ->
        let header = Network_peer.Envelope.Incoming.data h_env in
        (Network_peer.Envelope.Incoming.sender h_env, header, None, `Header)
  in
  handle_gossip ~logger ~consensus_constants ~frontier ~trust_system
    ~time_controller ~state ~sender ~body_opt ~gossip_type
    header_with_validation vc_opt

let handle_produced_transition ~logger ~state breadcrumb =
  let hash = Frontier_base.Breadcrumb.state_hash breadcrumb in
  let data =
    Transition_state_extended.Processing
      (Transition_state.Waiting_to_be_added_to_frontier (breadcrumb, `Local))
  in
  match Hashtbl.add state.transition_states ~key:hash ~data with
  | `Ok ->
      ()
  | `Duplicate ->
      [%log warn]
        "Produced breadcrumb $state_hash is already in bit-catchup state"
        ~metadata:[ ("state_hash", State_hash.to_yojson hash) ]

module Stream_reader_wrapper = struct
  type 'a t =
    | Deferred of
        { stream : 'a Pipe_lib.Strict_pipe.Reader.t
        ; next_value : [ `Eof | `Ok of 'a ] Deferred.t
        }
    | Eof

  let create stream =
    let next_value = Pipe_lib.Strict_pipe.Reader.read stream in
    Deferred { stream; next_value }

  let recharge wrapper =
    match wrapper with
    | Eof ->
        Eof
    | Deferred wrapper ->
        let next_value = Pipe_lib.Strict_pipe.Reader.read wrapper.stream in
        Deferred { wrapper with next_value }

  let to_choice ~f wrapper =
    match wrapper with
    | Eof ->
        None
    | Deferred { next_value; _ } ->
        Some (Deferred.choice next_value f)
end

type streams =
  { produced_transitions : Frontier_base.Breadcrumb.t Stream_reader_wrapper.t
  ; network_transitions : Types.produced_transition Stream_reader_wrapper.t
  }

let wait_for_next_event ?until streams =
  let timeout_choice =
    Option.(
      until >>| Async_kernel.at >>| Fn.flip Deferred.choice (const `Timeout))
  in
  let produced_tag x = `Produced x in
  let network_tag x = `Network x in
  let open Stream_reader_wrapper in
  let choices =
    List.filter_opt
      [ timeout_choice
      ; to_choice ~f:produced_tag streams.produced_transitions
      ; to_choice ~f:network_tag streams.network_transitions
      ]
  in
  let%map event =
    if List.is_empty choices then Deferred.return `Eof
    else Deferred.choose choices
  in
  match event with
  | `Produced `Eof ->
      (`No_event, { streams with produced_transitions = Eof })
  | `Network `Eof ->
      (`No_event, { streams with network_transitions = Eof })
  | `Produced (`Ok v) ->
      ( `Produced v
      , { streams with
          produced_transitions = recharge streams.produced_transitions
        } )
  | `Network (`Ok v) ->
      ( `Network v
      , { streams with
          network_transitions = recharge streams.network_transitions
        } )
  | `Eof ->
      (`Eof, streams)
  | `Timeout ->
      (`Timeout, streams)

let process_queue_element ~logger ~verifier ~network ~verified_transition_writer
    ~state (state_hash, event) =
  let old_state_ext = Hashtbl.find state.transition_states state_hash in
  let update_state new_state =
    Hashtbl.update state.transition_states state_hash ~f:(const new_state)
  in
  let handle_processing old_state =
    match (old_state, event) with
    | Transition_state.Downloading_ancestry { header; gossip_data; body_opt }, _
      ->
        ()
    | Verifying_ancestry_chain { header; gossip_data; body_opt; _ }, _ ->
        ()
    | Downloading_body_from_bitswap _, Event.Received_block_through_gossip block
      ->
        ()
    | Downloading_body_from_peers _, Received_block_through_gossip block ->
        ()
    | Downloading_body_from_bitswap _, Timeout ->
        ()
    | Downloading_body_from_bitswap _, Success ->
        ()
    | Downloading_body_from_peers _, Success ->
        ()
    | (Downloading_body_from_peers _ as old_st), Timeout ->
        update_state
          (Failure
             { previous_state = old_st
             ; error = Error.of_string "Timeout downloading header"
             } )
    | Waiting_for_complete_work_verification _, _ ->
        ()
    | Downloading_ancestry_bodies _, _ ->
        ()
    | Verifying_complete_works _, _ ->
        ()
    | Building_breadcrumb _, _ ->
        ()
    | Waiting_for_parent_added_to_frontier _, _ ->
        ()
    | Waiting_to_be_added_to_frontier _, _ ->
        ()
    | (Received _ as st), _
    | (Downloading_body_from_peers _ as st), _
    | (Downloading_body_from_bitswap _ as st), _ ->
        [%log error] "Unexpected $event for $transition_state"
          ~metadata:
            [ ( "transition_state"
              , Transition_state.(Enum.to_yojson @@ to_enum st) )
            ; ("event", Event.(Enum.to_yojson @@ to_enum event))
            ]
  in
  match old_state_ext with
  | None ->
      [%log error] "Element in queue $state_hash has not state registered"
        ~metadata:[ ("state_hash", State_hash.to_yojson state_hash) ]
  | Some (Invalid _) ->
      [%log error] "Element in queue $state_hash has Invalid state"
        ~metadata:[ ("state_hash", State_hash.to_yojson state_hash) ]
  | Some (Failure _) ->
      [%log error] "Element in queue $state_hash has Failure state"
        ~metadata:[ ("state_hash", State_hash.to_yojson state_hash) ]
  | Some (Processing old_state) ->
      handle_processing old_state

let run ~logger ~trust_system ~verifier ~network ~time_controller
    ~collected_transitions ~frontier ~network_transition_reader
    ~producer_transition_reader ~clear_reader:_ ~precomputed_values
    ~verified_transition_writer =
  let consensus_constants =
    Precomputed_values.consensus_constants precomputed_values
  in
  let state =
    { transition_states = State_hash.Table.create ()
    ; processing_queue = Queue.create ()
    ; timeout_queue = Queue.create ()
    }
  in
  let open Deferred.Let_syntax in
  List.iter collected_transitions
    ~f:
      (handle_collected_transition ~logger ~consensus_constants ~frontier
         ~trust_system ~time_controller ~state ) ;
  let init_streams =
    Stream_reader_wrapper.
      { produced_transitions = create producer_transition_reader
      ; network_transitions = create network_transition_reader
      }
  in
  let process_queue ret =
    Queue.iter state.processing_queue
      ~f:
        (process_queue_element ~logger ~verifier ~network
           ~verified_transition_writer ~state ) ;
    Queue.clear state.processing_queue ;
    ret
  in
  process_queue () ;
  Deferred.repeat_until_finished init_streams
  @@ fun streams ->
  let until = Option.map ~f:fst (Queue.peek state.timeout_queue) in
  match%map wait_for_next_event ?until streams with
  | `Eof, _ ->
      process_queue (`Finished ())
  | `Timeout, streams ->
      let _, hash = Queue.dequeue_exn state.timeout_queue in
      Queue.enqueue state.processing_queue (hash, Timeout) ;
      process_queue (`Repeat streams)
  | `No_event, streams ->
      process_queue (`Repeat streams)
  | `Produced t, streams ->
      handle_produced_transition ~logger ~state t ;
      process_queue (`Repeat streams)
  | `Network t, streams ->
      handle_network_transition ~logger ~consensus_constants ~frontier
        ~trust_system ~time_controller ~state t ;
      process_queue (`Repeat streams)
