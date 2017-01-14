% module Erl.Process.Raw
-module(erl_process_raw@foreign).
-export([ eqNative/1, self/0, spawn/1, spawnLink/1, send/1, 'receive'/0, receiveWithTrappedMsg/1, setTrappedExit/1, exit/1, exitPid/1]).

eqNative(X) -> fun (Y) -> X == Y end.

self() -> fun () -> erlang:self() end.

spawn(F) -> fun () -> erlang:spawn(fun () -> F() end) end.

spawnLink(F) -> fun () -> erlang:spawn_link(fun () -> F() end) end.

send(Pid) ->
  fun (X) ->
    fun () ->
      Pid ! X
    end
  end.

'receive'() ->
  fun () ->
    receive X -> X end
  end.

receiveWithTrappedMsg(F) ->
    receive
        {'EXIT', Pid, kill  } -> F({exitMsg, Pid, {kill}});
        {'EXIT', Pid, normal} -> F({exitMsg, Pid, {normal}});
        {'EXIT', Pid, X     } when erlang:is_atom(X)
                              -> F({exitMsg, Pid, {other, erlang:atom_to_list(X)}});
        {'EXIT', Pid, X     } -> F({exitMsg, Pid, {other, io_lib:format("~p",[X])}});
        Msg                   -> Msg
    end.

setTrappedExit(B) -> fun() -> erlang:process_flag(trap_exit, B) end.

exit(Exit) ->
    case Exit of
        {kill}         -> erlang:exit(kill  );
        {normal}       -> erlang:exit(normal);
        {other, Other} -> erlang:exit(Other )
    end.

exitPid(Exit) ->
    case Exit of
        {exitMsg, Pid, {kill}}         -> erlang:exit(Pid, kill  );
        {exitMsg, Pid, {normal}}       -> erlang:exit(Pid, normal);
        {exitMsg, Pid, {other, Other}} -> erlang:exit(Pid, Other )
    end.


