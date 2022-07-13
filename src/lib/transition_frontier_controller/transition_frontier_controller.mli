val run :
     logger:Logger.t
  -> trust_system:Trust_system.t
  -> verifier:Verifier.t
  -> network:Mina_networking.t
  -> time_controller:Block_time.Controller.t
  -> collected_transitions:Bootstrap_controller.Transition_cache.element list
  -> frontier:Transition_frontier.t
  -> network_transition_reader:
       ( [< `Block of
            Mina_block.Validation.initial_valid_with_block
            Network_peer.Envelope.Incoming.t ]
       * [< `Valid_cb of Mina_net2.Validation_callback.t option ] )
       Pipe_lib.Strict_pipe.Reader.t
  -> producer_transition_reader:
       Frontier_base.Breadcrumb.t Pipe_lib.Strict_pipe.Reader.t
  -> clear_reader:[> `Clear ] Pipe_lib.Strict_pipe.Reader.t
  -> precomputed_values:Precomputed_values.t
  -> verified_transition_writer:
       ( [> `Transition of Mina_block.Validated.t ]
         * [> `Source of [> `Catchup | `Gossip | `Internal ] ]
         * [> `Valid_cb of Mina_net2.Validation_callback.t option ]
       , 'a
       , unit )
       Pipe_lib.Strict_pipe.Writer.t
  -> unit
