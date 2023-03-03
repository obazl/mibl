open Core
open Async
open Pipe_lib
open Coda_base
open Coda_state
open Coda_transition
open Signature_lib
open O1trace
open Otp_lib
module Time = Block_time

type Structured_log_events.t += Block_produced
  [@@deriving register_event {msg= "Successfully produced a new block"}]

module Singleton_supervisor : sig
  type ('data, 'a) t

  val create :
    task:(unit Ivar.t -> 'data -> ('a, unit) Interruptible.t) -> ('data, 'a) t

  val cancel : (_, _) t -> unit

  val dispatch : ('data, 'a) t -> 'data -> ('a, unit) Interruptible.t
end = struct
  type ('data, 'a) t =
    { mutable task: (unit Ivar.t * ('a, unit) Interruptible.t) option
    ; f: unit Ivar.t -> 'data -> ('a, unit) Interruptible.t }

  let create ~task = {task= None; f= task}

  let cancel t =
    match t.task with
    | Some (ivar, _) ->
        Ivar.fill ivar () ;
        t.task <- None
    | None ->
        ()

  let dispatch t data =
    cancel t ;
    let ivar = Ivar.create () in
    let interruptible =
      let open Interruptible.Let_syntax in
      t.f ivar data
      >>| fun x ->
      t.task <- None ;
      x
    in
    t.task <- Some (ivar, interruptible) ;
    interruptible
end

module Transition_frontier_validation =
  External_transition.Transition_frontier_validation (Transition_frontier)

let time_to_ms = Fn.compose Time.Span.to_ms Time.to_span_since_epoch

let time_of_ms = Fn.compose Time.of_span_since_epoch Time.Span.of_ms

let lift_sync f =
  Interruptible.uninterruptible
    (Deferred.create (fun ivar -> Ivar.fill ivar (f ())))

module Singleton_scheduler : sig
  type t

  val create : Time.Controller.t -> t

  (** If you reschedule when already scheduled, take the min of the two schedulings *)
  val schedule : t -> Time.t -> f:(unit -> unit) -> unit
end = struct
  type t =
    { mutable timeout: unit Time.Timeout.t option
    ; time_controller: Time.Controller.t }

  let create time_controller = {time_controller; timeout= None}

  let cancel t =
    match t.timeout with
    | Some timeout ->
        Time.Timeout.cancel t.time_controller timeout () ;
        t.timeout <- None
    | None ->
        ()

  let schedule t time ~f =
    let remaining_time = Option.map t.timeout ~f:Time.Timeout.remaining_time in
    cancel t ;
    let span_till_time = Time.diff time (Time.now t.time_controller) in
    let wait_span =
      match remaining_time with
      | Some remaining when Time.Span.(remaining > Time.Span.of_ms Int64.zero)
        ->
          let min a b = if Time.Span.(a < b) then a else b in
          min remaining span_till_time
      | None | Some _ ->
          span_till_time
    in
    let timeout =
      Time.Timeout.create t.time_controller wait_span ~f:(fun _ ->
          t.timeout <- None ;
          f () )
    in
    t.timeout <- Some timeout
end

let generate_next_state ~constraint_constants ~previous_protocol_state
    ~time_controller ~staged_ledger ~transactions ~get_completed_work ~logger
    ~coinbase_receiver ~(keypair : Keypair.t) ~block_data ~scheduled_time
    ~log_block_creation =
  let open Interruptible.Let_syntax in
  let self = Public_key.compress keypair.public_key in
  let previous_protocol_state_body_hash =
    Protocol_state.body previous_protocol_state |> Protocol_state.Body.hash
  in
  let previous_protocol_state_hash =
    Protocol_state.hash_with_body ~body_hash:previous_protocol_state_body_hash
      previous_protocol_state
  in
  let previous_global_slot =
    Protocol_state.body previous_protocol_state
    |> Coda_state.Protocol_state.Body.consensus_state
    |> Consensus.Data.Consensus_state.curr_slot
  in
  let%bind res =
    Interruptible.uninterruptible
      (let open Deferred.Let_syntax in
      let diff =
        measure "create_diff" (fun () ->
            Staged_ledger.create_diff ~constraint_constants staged_ledger ~self
              ~coinbase_receiver ~logger
              ~current_global_slot:previous_global_slot
              ~transactions_by_fee:transactions ~get_completed_work
              ~log_block_creation )
      in
      match%map
        Staged_ledger.apply_diff_unchecked staged_ledger ~constraint_constants
          diff ~logger ~current_global_slot:previous_global_slot
          ~state_and_body_hash:
            (previous_protocol_state_hash, previous_protocol_state_body_hash)
      with
      | Ok
          ( `Hash_after_applying next_staged_ledger_hash
          , `Ledger_proof ledger_proof_opt
          , `Staged_ledger transitioned_staged_ledger
          , `Pending_coinbase_data
              (is_new_stack, coinbase_amount, pending_coinbase_action) ) ->
          (*staged_ledger remains unchanged and transitioned_staged_ledger is discarded because the external transtion created out of this diff will be applied in Transition_frontier*)
          ignore
          @@ Ledger.unregister_mask_exn
               (Staged_ledger.ledger transitioned_staged_ledger) ;
          Some
            ( diff
            , next_staged_ledger_hash
            , ledger_proof_opt
            , is_new_stack
            , coinbase_amount
            , pending_coinbase_action )
      | Error (Staged_ledger.Staged_ledger_error.Unexpected e) ->
          raise (Error.to_exn e)
      | Error e ->
          Logger.error logger ~module_:__MODULE__ ~location:__LOC__
            ~metadata:
              [ ( "error"
                , `String (Staged_ledger.Staged_ledger_error.to_string e) )
              ; ( "diff"
                , Staged_ledger_diff.With_valid_signatures_and_proofs.to_yojson
                    diff ) ]
            "Error applying the diff $diff: $error" ;
          None)
  in
  match res with
  | None ->
      Interruptible.return None
  | Some
      ( diff
      , next_staged_ledger_hash
      , ledger_proof_opt
      , is_new_stack
      , coinbase_amount
      , pending_coinbase_action ) ->
      let%bind protocol_state, consensus_transition_data =
        lift_sync (fun () ->
            let previous_ledger_hash =
              previous_protocol_state |> Protocol_state.blockchain_state
              |> Blockchain_state.snarked_ledger_hash
            in
            let next_ledger_hash =
              Option.value_map ledger_proof_opt
                ~f:(fun (proof, _) ->
                  Ledger_proof.statement proof |> Ledger_proof.statement_target
                  )
                ~default:previous_ledger_hash
            in
            let snarked_next_available_token =
              match ledger_proof_opt with
              | Some (proof, _) ->
                  (Ledger_proof.statement proof).next_available_token_after
              | None ->
                  previous_protocol_state |> Protocol_state.blockchain_state
                  |> Blockchain_state.snarked_next_available_token
            in
            let supply_increase =
              Option.value_map ledger_proof_opt
                ~f:(fun (proof, _) ->
                  (Ledger_proof.statement proof).supply_increase )
                ~default:Currency.Amount.zero
            in
            let blockchain_state =
              (* We use the time of the beginning of the slot because if things
                 are slower than expected, we may have entered the next slot and
                 putting the **current** timestamp rather than the expected one
                 will screw things up.

                 [generate_transition] will log an error if the [current_time]
                 has a different slot from the [scheduled_time]
              *)
              Blockchain_state.create_value ~timestamp:scheduled_time
                ~snarked_ledger_hash:next_ledger_hash
                ~snarked_next_available_token
                ~staged_ledger_hash:next_staged_ledger_hash
            in
            let current_time =
              Time.now time_controller |> Time.to_span_since_epoch
              |> Time.Span.to_ms
            in
            measure "consensus generate_transition" (fun () ->
                Consensus_state_hooks.generate_transition
                  ~previous_protocol_state ~blockchain_state ~current_time
                  ~block_data ~snarked_ledger_hash:previous_ledger_hash
                  ~supply_increase ~logger ~constraint_constants ) )
      in
      lift_sync (fun () ->
          measure "making Snark and Internal transitions" (fun () ->
              let snark_transition =
                Snark_transition.create_value
                  ?sok_digest:
                    (Option.map ledger_proof_opt ~f:(fun (proof, _) ->
                         Ledger_proof.sok_digest proof ))
                  ?ledger_proof:
                    (Option.map ledger_proof_opt ~f:(fun (proof, _) ->
                         Ledger_proof.underlying_proof proof ))
                  ~supply_increase:
                    (Option.value_map ~default:Currency.Amount.zero
                       ~f:(fun (proof, _) ->
                         (Ledger_proof.statement proof).supply_increase )
                       ledger_proof_opt)
                  ~blockchain_state:
                    (Protocol_state.blockchain_state protocol_state)
                  ~consensus_transition:consensus_transition_data
                  ~coinbase_receiver:
                    ( match coinbase_receiver with
                    | `Producer ->
                        self
                    | `Other pk ->
                        pk )
                  ~coinbase_amount ~pending_coinbase_action ()
              in
              let internal_transition =
                Internal_transition.create ~snark_transition
                  ~prover_state:
                    (Consensus.Data.Block_data.prover_state block_data)
                  ~staged_ledger_diff:(Staged_ledger_diff.forget diff)
              in
              let witness =
                { Pending_coinbase_witness.pending_coinbases=
                    Staged_ledger.pending_coinbase_collection staged_ledger
                ; is_new_stack }
              in
              Some (protocol_state, internal_transition, witness) ) )

let run ~logger ~prover ~verifier ~trust_system ~get_completed_work
    ~transaction_resource_pool ~time_controller ~keypairs ~coinbase_receiver
    ~consensus_local_state ~frontier_reader ~transition_writer
    ~set_next_producer_timing ~log_block_creation
    ~(precomputed_values : Precomputed_values.t) =
  trace "block_producer" (fun () ->
      let constraint_constants = precomputed_values.constraint_constants in
      let consensus_constants = precomputed_values.consensus_constants in
      let log_bootstrap_mode () =
        Logger.info logger ~module_:__MODULE__ ~location:__LOC__
          "Pausing block production while bootstrapping"
      in
      let module Breadcrumb = Transition_frontier.Breadcrumb in
      let produce ivar (keypair, scheduled_time, block_data) =
        let open Interruptible.Let_syntax in
        match Broadcast_pipe.Reader.peek frontier_reader with
        | None ->
            log_bootstrap_mode () ; Interruptible.return ()
        | Some frontier -> (
            let open Transition_frontier.Extensions in
            let transition_registry =
              get_extension
                (Transition_frontier.extensions frontier)
                Transition_registry
            in
            let crumb = Transition_frontier.best_tip frontier in
            Logger.trace logger ~module_:__MODULE__ ~location:__LOC__
              ~metadata:[("breadcrumb", Breadcrumb.to_yojson crumb)]
              "Producing new block with parent $breadcrumb%!" ;
            let previous_protocol_state, previous_protocol_state_proof =
              let transition : External_transition.Validated.t =
                Breadcrumb.validated_transition crumb
              in
              ( External_transition.Validated.protocol_state transition
              , External_transition.Validated.protocol_state_proof transition
              )
            in
            let transactions =
              Network_pool.Transaction_pool.Resource_pool.transactions ~logger
                transaction_resource_pool
            in
            trace_event "waiting for ivar..." ;
            let%bind () =
              Interruptible.lift (Deferred.return ()) (Ivar.read ivar)
            in
            let%bind next_state_opt =
              generate_next_state ~constraint_constants ~scheduled_time
                ~coinbase_receiver ~block_data ~previous_protocol_state
                ~time_controller
                ~staged_ledger:(Breadcrumb.staged_ledger crumb)
                ~transactions ~get_completed_work ~logger ~keypair
                ~log_block_creation
            in
            trace_event "next state generated" ;
            match next_state_opt with
            | None ->
                Interruptible.return ()
            | Some
                (protocol_state, internal_transition, pending_coinbase_witness)
              ->
                Debug_assert.debug_assert (fun () ->
                    [%test_result: [`Take | `Keep]]
                      (Consensus.Hooks.select ~constants:consensus_constants
                         ~existing:
                           (Protocol_state.consensus_state
                              previous_protocol_state)
                         ~candidate:
                           (Protocol_state.consensus_state protocol_state)
                         ~logger)
                      ~expect:`Take
                      ~message:
                        "newly generated consensus states should be selected \
                         over their parent" ;
                    let root_consensus_state =
                      Transition_frontier.root frontier
                      |> Breadcrumb.consensus_state
                    in
                    [%test_result: [`Take | `Keep]]
                      (Consensus.Hooks.select ~existing:root_consensus_state
                         ~constants:consensus_constants
                         ~candidate:
                           (Protocol_state.consensus_state protocol_state)
                         ~logger)
                      ~expect:`Take
                      ~message:
                        "newly generated consensus states should be selected \
                         over the tf root" ) ;
                Interruptible.uninterruptible
                  (let open Deferred.Let_syntax in
                  let t0 = Time.now time_controller in
                  match%bind
                    measure "proving state transition valid" (fun () ->
                        Prover.prove prover ~prev_state:previous_protocol_state
                          ~prev_state_proof:previous_protocol_state_proof
                          ~next_state:protocol_state internal_transition
                          pending_coinbase_witness )
                  with
                  | Error err ->
                      Logger.error logger ~module_:__MODULE__ ~location:__LOC__
                        "Prover failed to prove freshly generated transition: \
                         $error"
                        ~metadata:
                          [ ("error", `String (Error.to_string_hum err))
                          ; ( "prev_state"
                            , Protocol_state.value_to_yojson
                                previous_protocol_state )
                          ; ( "prev_state_proof"
                            , Proof.to_yojson previous_protocol_state_proof )
                          ; ( "next_state"
                            , Protocol_state.value_to_yojson protocol_state )
                          ; ( "internal_transition"
                            , Internal_transition.to_yojson internal_transition
                            )
                          ; ( "pending_coinbase_witness"
                            , Pending_coinbase_witness.to_yojson
                                pending_coinbase_witness ) ] ;
                      return ()
                  | Ok protocol_state_proof -> (
                      let span = Time.diff (Time.now time_controller) t0 in
                      Logger.info logger ~module_:__MODULE__ ~location:__LOC__
                        ~metadata:
                          [ ( "proving_time"
                            , `Int (Time.Span.to_ms span |> Int64.to_int_exn)
                            ) ]
                        !"Protocol_state_proof proving time(ms): \
                          $proving_time%!" ;
                      let staged_ledger_diff =
                        Internal_transition.staged_ledger_diff
                          internal_transition
                      in
                      let transition_hash =
                        Protocol_state.hash protocol_state
                      in
                      let previous_state_hash =
                        Protocol_state.hash previous_protocol_state
                      in
                      let delta_transition_chain_proof =
                        Transition_chain_prover.prove
                          ~length:
                            ( Coda_numbers.Length.to_int
                                consensus_constants.delta
                            - 1 )
                          ~frontier previous_state_hash
                        |> Option.value_exn
                      in
                      let error_msg_prefix = "Validation failed: " in
                      let reason_for_failure =
                        " One possible reason could be a ledger-catchup is \
                         triggered before we produce a proof for the produced \
                         transition."
                      in
                      match
                        let open Result.Let_syntax in
                        let%map t =
                          External_transition.Validation.wrap
                            { With_hash.hash= transition_hash
                            ; data=
                                External_transition.create ~protocol_state
                                  ~protocol_state_proof ~staged_ledger_diff
                                  ~validation_callback:Fn.ignore
                                  ~delta_transition_chain_proof () }
                          |> External_transition.skip_time_received_validation
                               `This_transition_was_not_received_via_gossip
                          |> External_transition
                             .skip_protocol_versions_validation
                               `This_transition_has_valid_protocol_versions
                          |> External_transition
                             .validate_genesis_protocol_state
                               ~genesis_state_hash:
                                 (Protocol_state.genesis_state_hash
                                    ~state_hash:(Some previous_state_hash)
                                    previous_protocol_state)
                        in
                        External_transition.skip_proof_validation
                          `This_transition_was_generated_internally t
                        |> External_transition
                           .skip_delta_transition_chain_validation
                             `This_transition_was_not_received_via_gossip
                      with
                      | Error `Invalid_genesis_protocol_state ->
                          let state_yojson =
                            Fn.compose State_hash.to_yojson
                              Protocol_state.genesis_state_hash
                          in
                          Logger.warn logger ~module_:__MODULE__
                            ~location:__LOC__
                            ~metadata:
                              [ ( "expected"
                                , state_yojson previous_protocol_state )
                              ; ("got", state_yojson protocol_state) ]
                            "Produced transition has invalid genesis state hash" ;
                          return ()
                      | Ok t -> (
                        match
                          Transition_frontier_validation
                          .validate_frontier_dependencies ~logger ~frontier t
                            ~consensus_constants
                        with
                        | Error `Already_in_frontier ->
                            Logger.error logger ~module_:__MODULE__
                              ~location:__LOC__
                              ~metadata:
                                [ ( "protocol_state"
                                  , Protocol_state.value_to_yojson
                                      protocol_state ) ]
                              "%sproduced transition is already in frontier"
                              error_msg_prefix ;
                            return ()
                        | Error `Not_selected_over_frontier_root ->
                            Logger.warn logger ~module_:__MODULE__
                              ~location:__LOC__
                              "%sproduced transition is not selected over the \
                               root of transition frontier.%s"
                              error_msg_prefix reason_for_failure ;
                            return ()
                        | Error `Parent_missing_from_frontier ->
                            Logger.warn logger ~module_:__MODULE__
                              ~location:__LOC__
                              "%sparent of produced transition is missing \
                               from the frontier.%s"
                              error_msg_prefix reason_for_failure ;
                            return ()
                        | Ok transition -> (
                            let%bind breadcrumb_result =
                              Breadcrumb.build ~logger ~precomputed_values
                                ~verifier ~trust_system ~parent:crumb
                                ~transition ~sender:None
                            in
                            let exn name =
                              raise
                                (Error.to_exn
                                   (Error.of_string
                                      (sprintf
                                         "Error building breadcrumb from \
                                          produced transition: %s"
                                         name)))
                            in
                            match breadcrumb_result with
                            | Error (`Fatal_error e) ->
                                exn
                                  (sprintf "fatal error -- %s"
                                     (Exn.to_string e))
                            | Error (`Invalid_staged_ledger_hash e) ->
                                exn
                                  (sprintf "Invalid staged ledger hash -- %s"
                                     (Error.to_string_hum e))
                            | Error (`Invalid_staged_ledger_diff e) ->
                                (*Unexpected errors from staged_ledger are captured in `Fatal_error*)
                                Logger.error logger ~module_:__MODULE__
                                  ~location:__LOC__
                                  ~metadata:
                                    [ ("error", `String (Error.to_string_hum e))
                                    ; ( "diff"
                                      , Staged_ledger_diff.to_yojson
                                          staged_ledger_diff ) ]
                                  !"Unable to build breadcrumb from produced \
                                    transition due to invlaid staged ledger \
                                    diff: $error" ;
                                return ()
                            | Ok breadcrumb -> (
                                Logger.Str.trace logger ~module_:__MODULE__
                                  ~location:__LOC__
                                  ~metadata:
                                    [ ( "breadcrumb"
                                      , Breadcrumb.to_yojson breadcrumb ) ]
                                  Block_produced ;
                                let metadata =
                                  [ ( "state_hash"
                                    , State_hash.to_yojson transition_hash ) ]
                                in
                                Coda_metrics.(
                                  Counter.inc_one
                                    Block_producer.blocks_produced) ;
                                let%bind () =
                                  Strict_pipe.Writer.write transition_writer
                                    breadcrumb
                                in
                                Logger.debug logger ~module_:__MODULE__
                                  ~location:__LOC__ ~metadata
                                  "Waiting for block $state_hash to be \
                                   inserted into frontier" ;
                                Deferred.choose
                                  [ Deferred.choice
                                      (Transition_registry.register
                                         transition_registry transition_hash)
                                      (Fn.const `Transition_accepted)
                                  ; Deferred.choice
                                      ( Time.Timeout.create time_controller
                                          (* We allow up to 15 seconds for the transition to make its way from the transition_writer to the frontier.
                                  This value is chosen to be reasonably generous. In theory, this should not take terribly long. But long
                                  cycles do happen in our system, and with medium curves those long cycles can be substantial. *)
                                          (Time.Span.of_ms 20000L)
                                          ~f:(Fn.const ())
                                      |> Time.Timeout.to_deferred )
                                      (Fn.const `Timed_out) ]
                                >>| function
                                | `Transition_accepted ->
                                    Logger.info logger ~module_:__MODULE__
                                      ~location:__LOC__ ~metadata
                                      "Generated transition $state_hash was \
                                       accepted into transition frontier"
                                | `Timed_out ->
                                    (* FIXME #3167: this should be fatal, and more importantly, shouldn't happen. *)
                                    Logger.fatal logger ~module_:__MODULE__
                                      ~location:__LOC__ ~metadata
                                      "Timed out waiting for generated \
                                       transition $state_hash to enter \
                                       transition frontier. Continuing to \
                                       produce new blocks anyway. This may \
                                       mean your CPU is overloaded. Consider \
                                       disabling `-run-snark-worker` if it's \
                                       configured." ) ) ) )) )
      in
      let production_supervisor = Singleton_supervisor.create ~task:produce in
      let scheduler = Singleton_scheduler.create time_controller in
      let rec check_next_block_timing () =
        trace_recurring "check next block timing" (fun () ->
            (* See if we want to change keypairs *)
            let keypairs =
              match Agent.get keypairs with
              | keypairs, `Different ->
                  (* Perform block production key swap since we have new
                     keypairs *)
                  Consensus.Data.Local_state.block_production_keys_swap
                    ~constants:consensus_constants consensus_local_state
                    ( Keypair.And_compressed_pk.Set.to_list keypairs
                    |> List.map ~f:snd |> Public_key.Compressed.Set.of_list )
                    (Time.now time_controller) ;
                  keypairs
              | keypairs, `Same ->
                  keypairs
            in
            (* Begin checking for the ability to produce a block *)
            match Broadcast_pipe.Reader.peek frontier_reader with
            | None ->
                log_bootstrap_mode () ;
                don't_wait_for
                  (let%map () =
                     Broadcast_pipe.Reader.iter_until frontier_reader
                       ~f:(Fn.compose Deferred.return Option.is_some)
                   in
                   check_next_block_timing ())
            | Some transition_frontier -> (
                let consensus_state =
                  Transition_frontier.best_tip transition_frontier
                  |> Breadcrumb.consensus_state
                in
                (* TODO: Re-enable this assertion when it doesn't fail dev demos
                 *       (see #5354)
                 * assert (
                  Consensus.Hooks.required_local_state_sync
                    ~constants:consensus_constants ~consensus_state
                    ~local_state:consensus_local_state
                  = None ) ; *)
                let now = Time.now time_controller in
                let next_producer_timing =
                  measure "asking consensus what to do" (fun () ->
                      Consensus.Hooks.next_producer_timing
                        ~constraint_constants ~constants:consensus_constants
                        (time_to_ms now) consensus_state
                        ~local_state:consensus_local_state ~keypairs ~logger )
                in
                set_next_producer_timing next_producer_timing ;
                match next_producer_timing with
                | `Check_again time ->
                    Singleton_scheduler.schedule scheduler (time_of_ms time)
                      ~f:check_next_block_timing
                | `Produce_now (keypair, data) ->
                    Coda_metrics.(Counter.inc_one Block_producer.slots_won) ;
                    Interruptible.finally
                      (Singleton_supervisor.dispatch production_supervisor
                         (keypair, now, data))
                      ~f:check_next_block_timing
                    |> ignore
                | `Produce (time, keypair, data) ->
                    Coda_metrics.(Counter.inc_one Block_producer.slots_won) ;
                    let scheduled_time = time_of_ms time in
                    Singleton_scheduler.schedule scheduler scheduled_time
                      ~f:(fun () ->
                        ignore
                          (Interruptible.finally
                             (Singleton_supervisor.dispatch
                                production_supervisor
                                (keypair, scheduled_time, data))
                             ~f:check_next_block_timing) ) ) )
      in
      let start () =
        (* Schedule to wake up immediately on the next tick of the producer loop
         * instead of immediately mutating local_state here as there could be a
         * race.
         *
         * Given that rescheduling takes the min of the two timeouts, we won't
         * erase this timeout even if the last run of the producer wants to wait
         * for a long while.
         * *)
        Agent.on_update keypairs ~f:(fun _new_keypairs ->
            Singleton_scheduler.schedule scheduler (Time.now time_controller)
              ~f:check_next_block_timing ) ;
        check_next_block_timing ()
      in
      let genesis_state_timestamp =
        consensus_constants.genesis_state_timestamp
      in
      (* if the producer starts before genesis, sleep until genesis *)
      let now = Time.now time_controller in
      if Time.( >= ) now genesis_state_timestamp then start ()
      else
        let time_till_genesis = Time.diff genesis_state_timestamp now in
        Logger.warn logger ~module_:__MODULE__ ~location:__LOC__
          ~metadata:
            [ ( "time_till_genesis"
              , `Int (Int64.to_int_exn (Time.Span.to_ms time_till_genesis)) )
            ]
          "Node started before genesis: waiting $time_till_genesis \
           milliseconds before starting block producer" ;
        ignore
          (Time.Timeout.create time_controller time_till_genesis ~f:(fun _ ->
               start () )) )
