% opencog-cogutil.pl
%
% OpenCog CogUtil - Core utility predicates in Prolog
%
% This implementation provides a compact, idiomatic Prolog counterpart to the
% repository's other cogutil demonstrations.  It focuses on the shared utility
% interface described by the OpenCog integration design: logging, configuration,
% exceptions, and concurrent-style work queues expressed as dynamic facts.

:- module(opencog_cogutil,
    [ log_message/2,
      log_message/3,
      set_log_level/1,
      current_log_level/1,
      config_set/2,
      config_get/2,
      config_get/3,
      config_clear/0,
      ensure_or_throw/2,
      queue_create/1,
      queue_push/2,
      queue_pop/2,
      queue_size/2,
      queue_clear/1,
      reset_cogutil/0,
      demo/0
    ]).

:- dynamic log_level/1.
:- dynamic config_value/2.
:- dynamic queue_item/3.

% ==========================================================================
% LOGGING
% ==========================================================================

log_priority(debug, 10).
log_priority(info, 20).
log_priority(warn, 30).
log_priority(error, 40).

log_level(info).

set_log_level(Level) :-
    log_priority(Level, _),
    retractall(log_level(_)),
    assertz(log_level(Level)).

current_log_level(Level) :-
    log_level(Level), !.
current_log_level(info).

log_message(Level, Message) :-
    log_message(Level, cogutil, Message).

log_message(Level, Component, Message) :-
    log_priority(Level, Priority),
    current_log_level(Current),
    log_priority(Current, CurrentPriority),
    Priority >= CurrentPriority,
    !,
    get_time(Now),
    format_time(atom(Timestamp), '%FT%T%z', Now),
    format('[~w] [~w] [~w] ~w~n', [Timestamp, Level, Component, Message]).
log_message(_, _, _).

% ==========================================================================
% CONFIGURATION
% ==========================================================================

config_set(Key, Value) :-
    retractall(config_value(Key, _)),
    assertz(config_value(Key, Value)).

config_get(Key, Value) :-
    config_value(Key, Value).

config_get(Key, Value, Default) :-
    ( config_get(Key, Value) -> true ; Value = Default ).

config_clear :-
    retractall(config_value(_, _)).

% ==========================================================================
% EXCEPTION HELPERS
% ==========================================================================

ensure_or_throw(Goal, ErrorTerm) :-
    ( call(Goal) -> true ; throw(error(ErrorTerm, context(ensure_or_throw/2, Goal))) ).

% ==========================================================================
% CONCURRENT-STYLE QUEUE FACTS
% ==========================================================================

queue_create(Name) :-
    atom(Name),
    queue_clear(Name).

queue_push(Name, Value) :-
    findall(Index, queue_item(Name, Index, _), Indexes),
    ( Indexes = [] -> Next is 1 ; max_list(Indexes, Last), Next is Last + 1 ),
    assertz(queue_item(Name, Next, Value)).

queue_pop(Name, Value) :-
    aggregate_all(min(Index), queue_item(Name, Index, _), First),
    queue_item(Name, First, Value),
    retract(queue_item(Name, First, Value)).

queue_size(Name, Size) :-
    aggregate_all(count, queue_item(Name, _, _), Size).

queue_clear(Name) :-
    retractall(queue_item(Name, _, _)).

reset_cogutil :-
    retractall(log_level(_)),
    assertz(log_level(info)),
    config_clear,
    retractall(queue_item(_, _, _)).

% ==========================================================================
% DEMONSTRATION
% ==========================================================================

demo :-
    reset_cogutil,
    set_log_level(debug),
    log_message(info, 'OpenCog CogUtil Prolog demo starting'),
    config_set(atomspace_endpoint, 'tcp://127.0.0.1:17001'),
    config_get(atomspace_endpoint, Endpoint),
    format('Configured AtomSpace endpoint: ~w~n', [Endpoint]),
    queue_create(agent_jobs),
    queue_push(agent_jobs, tick_ecan),
    queue_push(agent_jobs, run_pln_rule),
    queue_size(agent_jobs, Size),
    format('Queued jobs: ~w~n', [Size]),
    queue_pop(agent_jobs, Job),
    format('Dequeued job: ~w~n', [Job]),
    ensure_or_throw(queue_size(agent_jobs, 1), queue_size_mismatch),
    log_message(info, 'OpenCog CogUtil Prolog demo complete').

:- initialization(demo, main).
