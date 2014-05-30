:- use_module(library(spawn)).

delayed_unify(X,Y) :-
    sleep(1),
    X = Y.

one_solution(foo(bar)).

two_solutions(thing(one)).
two_solutions(thing(two)).

exceptional(_) :-
    throw(oh_no).

fails(_) :-
    fail.

thread_count(N) :-
    bagof(Id,S^thread_property(Id,status(S)),Ids),
    length(Ids,N).

:- use_module(library(tap)).

% two spawned goals actually happen in parallel
goes_faster :-
    get_time(Start),
    spawn(delayed_unify(A,a)),
    spawn(delayed_unify(B,b)),
    get_time(Mid),

    % getting here takes way less than one second
    % so these should both still be variables
    var(A),
    var(B),

    % wait for the computations to finish
    A = a,
    B = b,
    get_time(End),

    % make sure the timings are reasonable
    Mid - Start < 0.1, % async/1 returns quickly
    End - Start < 1.1, % sleep calls happened in parallel
    End - Start > 0.9. % sleep calls actually happened


% unification with a single solution leaves no choicepoints
single :-
    spawn(one_solution(X)),
    X = foo(Y),
    Y == bar.


% unification with first or second solution is OK
double_first :-
    spawn(two_solutions(X)),
    X = thing(N),
    N == one,
    !.  % trim choicepoint representing thing(two)

double_second :-
    spawn(two_solutions(X)),
    X = thing(N),
    N == two.


% unification fails when there are no solutions
zero(fail) :-
    spawn(fails(X)),
    X = never_will_suceed.


% unification rethrows its goal's exception
an_exception(throws(oh_no)) :-
    spawn(exceptional(X)),
    X = never_will_suceed.


% make sure we don't leave threads lying around
no_leftover_threads :-
    thread_count(1),  % only the main thread running
    spawn(one_solution(X)),
    X = foo(bar),
    thread_count(1).  % intermediate threads have been reclaimed
