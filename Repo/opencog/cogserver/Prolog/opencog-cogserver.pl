% opencog-cogserver.pl
%
% OpenCog CogServer - Logic-based command registry and REPL in Prolog
%
% This implementation complements the existing Prolog AtomSpace binding by
% exposing CogServer concepts as dynamic predicates: commands, sessions,
% requests, responses, and a small reasoning-oriented dispatch layer.

:- module(opencog_cogserver,
    [ register_command/3,
      unregister_command/1,
      command_registered/1,
      list_commands/1,
      start_session/1,
      end_session/1,
      active_session/1,
      execute_command/3,
      execute_line/3,
      server_status/1,
      reset_cogserver/0,
      demo/0
    ]).

:- dynamic command_spec/3.
:- dynamic session/2.
:- dynamic request_log/4.
:- dynamic demo_node/2.
:- dynamic demo_link/3.

% ==========================================================================
% COMMAND REGISTRY
% ==========================================================================

register_command(Name, Arity, Handler) :-
    atom(Name),
    integer(Arity),
    Arity >= 0,
    callable(Handler),
    retractall(command_spec(Name, _, _)),
    assertz(command_spec(Name, Arity, Handler)).

unregister_command(Name) :-
    retractall(command_spec(Name, _, _)).

command_registered(Name) :-
    command_spec(Name, _, _).

list_commands(Commands) :-
    findall(command(Name, Arity), command_spec(Name, Arity, _), Commands0),
    sort(Commands0, Commands).

% ==========================================================================
% SESSION LIFECYCLE
% ==========================================================================

start_session(SessionId) :-
    get_time(Now),
    retractall(session(SessionId, _)),
    assertz(session(SessionId, Now)).

end_session(SessionId) :-
    retractall(session(SessionId, _)).

active_session(SessionId) :-
    session(SessionId, _).

% ==========================================================================
% DISPATCH
% ==========================================================================

execute_line(SessionId, Line, Response) :-
    atomic_list_concat(Parts, ' ', Line),
    exclude(=(''), Parts, Tokens),
    Tokens = [CommandAtom | Args],
    atom_string(Command, CommandAtom),
    execute_command(SessionId, Command, Args, Response).

execute_command(SessionId, Command, Args, Response) :-
    active_session(SessionId),
    command_spec(Command, Arity, Handler),
    length(Args, Arity),
    !,
    get_time(Now),
    assertz(request_log(SessionId, Command, Args, Now)),
    call(Handler, SessionId, Args, Response).
execute_command(_, Command, _, error(unknown_command(Command))).

% ==========================================================================
% BUILT-IN COMMAND HANDLERS
% ==========================================================================

handle_help(_, _, response(ok, Commands)) :-
    list_commands(Commands).

handle_status(_, _, response(ok, Status)) :-
    server_status(Status).

handle_addnode(_, [Type, Name], response(ok, node(Type, Name))) :-
    assertz(demo_node(Type, Name)).

handle_link(_, [Predicate, Source, Target], response(ok, link(Predicate, Source, Target))) :-
    assertz(demo_link(Predicate, Source, Target)).

handle_query(_, [Type], response(ok, Nodes)) :-
    findall(node(Type, Name), demo_node(Type, Name), Nodes).

handle_inherits(_, [Child, Ancestor], response(ok, true)) :-
    inherits(Child, Ancestor), !.
handle_inherits(_, [_, _], response(ok, false)).

handle_echo(_, Args, response(ok, Args)).

server_status(status{sessions: Sessions, commands: Commands, requests: Requests}) :-
    aggregate_all(count, session(_, _), Sessions),
    aggregate_all(count, command_spec(_, _, _), Commands),
    aggregate_all(count, request_log(_, _, _, _), Requests).

% A small transitive inference rule set for CogServer demonstrations.
inherits(Child, Parent) :-
    demo_link(inherits, Child, Parent).
inherits(Child, Ancestor) :-
    demo_link(inherits, Child, Parent),
    inherits(Parent, Ancestor).

% ==========================================================================
% INITIALIZATION AND DEMO
% ==========================================================================

install_builtin_commands :-
    register_command(help, 0, handle_help),
    register_command(status, 0, handle_status),
    register_command(addnode, 2, handle_addnode),
    register_command(link, 3, handle_link),
    register_command(query, 1, handle_query),
    register_command(inherits, 2, handle_inherits),
    register_command(echo, 1, handle_echo).

reset_cogserver :-
    retractall(command_spec(_, _, _)),
    retractall(session(_, _)),
    retractall(request_log(_, _, _, _)),
    retractall(demo_node(_, _)),
    retractall(demo_link(_, _, _)),
    install_builtin_commands.

demo :-
    reset_cogserver,
    start_session(console),
    execute_command(console, addnode, [concept, 'Socrates'], AddNode),
    execute_command(console, addnode, [concept, human], _),
    execute_command(console, addnode, [concept, mortal], _),
    execute_command(console, link, [inherits, 'Socrates', human], _),
    execute_command(console, link, [inherits, human, mortal], _),
    execute_command(console, inherits, ['Socrates', mortal], Inference),
    execute_command(console, status, [], Status),
    format('addnode => ~w~n', [AddNode]),
    format('inherits(Socrates, mortal) => ~w~n', [Inference]),
    format('status => ~w~n', [Status]),
    end_session(console).

:- initialization(demo, main).
