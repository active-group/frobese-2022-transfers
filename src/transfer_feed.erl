-module(transfer_feed).
-include("events.hrl").
-include("data.hrl").
-behaviour(gen_server).
-export([start_link/1, broadcast_new_transfer/1, send_events/2, handle_cast/2, init/1]).

start_link(Start) ->
    gen_server:start_link({local, transfer_feed}, ?MODULE, Start, [{debug, [trace]}]).

broadcast_new_transfer(Transfer) -> gen_server:cast(transfer_feed, Transfer).

send_events([], _) -> [];
send_events(EventsList, Pid) -> 
    lists:foreach(fun (Event) -> 
        gen_server:cast(Pid, {transfer_service, Event#event.number, Event#event.payload}) end, 
        EventsList
    ).

handle_cast({hallo, Pid, _Counter}, {RegisteredProcessesState, Count}) ->
    gen_server:cast(Pid, {selber_hallo}),
    {noreply, {RegisteredProcessesState, Count}};

handle_cast({register, Pid, transfer_service, Counter}, {RegisteredProcessesState, Count}) ->
    NewRegisteredProcessesState = sets:add_element(Pid, RegisteredProcessesState),
    AllEvents = events:get_events_from(Counter + 1),
    send_events(AllEvents, Pid),

    {noreply, {NewRegisteredProcessesState, Count}};

handle_cast(#transfer{id = Id, timestamp = Timestamp, from_acc_nr = From, to_acc_nr = To, amount = Amount}, {RegisteredProcessesState, Count}) -> 
    NewCount = Count + 1,
    RegisteredProcessesAsList = sets:to_list(RegisteredProcessesState),
    lists:foreach(fun (Pid) -> 
        gen_server:cast(Pid, {transfer_service, NewCount, {transferEvent, Id, Timestamp, From, To, Amount}}) end, 
        RegisteredProcessesAsList
    ),
    events:put_event({transferEvent, Id, Timestamp, From, To, Amount}),
    {noreply, {RegisteredProcessesState, NewCount}}.

init(_) ->
    {ok, {sets:new(), 0}}.
