:- module(spawn, [ async/2
                 , async/3
                 , await/1
                 ]).

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
    thread_create(ephemeral_worker(Work), _, []).


ephemeral_worker(work(Goal,Vars,SolutionsQ)) :-
    forall( call(Goal)
          , thread_send_message(SolutionsQ,solution(Vars))
          ),
    thread_send_message(SolutionsQ,done).


await(ephemeral_token(Vars,SolutionsQ)) :-
    thread_get_message(SolutionsQ,Solution),
    Solution = solution(Vars).
