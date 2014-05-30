:- module(spawn, [ async/2
                 , async/3
                 , await/1
                 ]).

:- meta_predicate
    async(0,-),
    async(0,-,+),
    async_policy(+,0,-,+).

async(Goal,Token) :-
    async(Goal,Token,[]).


async(Goal,Token,_Options) :-
    _ = Opts, % parse Options into a record term
    async_policy(ephemeral, Goal, Token, Opts).


async_policy(ephemeral, Goal, Token, _Opts) :-
    % what does the caller need to track this computation?
    term_variables(Goal, Vars),
    message_queue_create(SolutionsQ, [max_size(1)]),
    Token = ephemeral_token(Vars,SolutionsQ),

    % start the worker thread
    Work = work(Goal,Vars,SolutionsQ),
    thread_create(ephemeral_worker(Work), _, [detached(true)]).


ephemeral_worker(work(Goal,Vars,SolutionsQ)) :-
    debug(spawn,"Seeking solutions to: ~q", [Goal]),
    ( catch(call_cleanup(Goal,Done=true),E,true) *->
        ( nonvar(E) ->
            debug(spawn,"Caught exception: ~q", [E]),
            thread_send_message(SolutionsQ,exception(E))
        ; var(Done) ->
            debug(spawn,"Sending solution: ~q", [Vars]),
            thread_send_message(SolutionsQ,solution(Vars)),
            fail  % look for another solution
        ; Done=true ->
            debug(spawn,"Final solution: ~q", [Vars]),
            thread_send_message(SolutionsQ,final(Vars))
        )
    ; % no solutions ->
        debug(spawn, "Found no solutions", []),
        thread_send_message(SolutionsQ,none)
    ).


await(ephemeral_token(Vars,SolutionsQ)) :-
    repeat,
    thread_get_message(SolutionsQ,Solution),
    ( Solution = solution(Vars) ->
        true
    ; Solution = final(Vars) ->
        !,
        true
    ; Solution = none ->
        !,
        fail
    ; Solution = exception(E) ->
        throw(E)
    ; % what? ->
        throw(unexpected_await_solution(Solution))
    ).
