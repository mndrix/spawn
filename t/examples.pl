:- use_module(library(spawn)).

delayed_unify(X,Y) :-
    sleep(1),
    X = Y.

:- use_module(library(tap)).

goes_faster :-
    get_time(Start),
    async(delayed_unify(A,a), TokenA),
    async(delayed_unify(B,b), TokenB),
    get_time(Mid),

    % getting here takes way less than one second
    % so these should both still be variables
    var(A),
    var(B),

    % wait for the computations to finish
    await(TokenA),
    await(TokenB),
    get_time(End),
    A == a,
    B == b,

    % make sure the timings are reasonable
    Mid - Start < 0.1, % async/1 returns quickly
    End - Start < 1.1. % sleep calls happened in parallel


% TODO what if Goal throws an exception?
% TODO what if Goal provides multiple solutions?
% TODO what if Goal provides no solutions?
