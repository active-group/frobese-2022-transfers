-module(transfer).
-export([start/1, handle_cast/2, init/1, handle_call/3, event1/0, handle_info/2]).

-behavior(gen_server).
-include("events.hrl").
-include("data.hrl").

-record(state, {pid :: number(), count :: number(), set :: sets:set()}).

start(RegistryPid) ->
  gen_server:start({local, transfer_broadcast}, ?MODULE, #state{pid = RegistryPid, count = 0, set = sets:new()}, [{debug, [trace]}]).

init(State) ->
  timer:send_interval(1000, hallo),
  {ok, State}. 

handle_cast({account_service,Count,create_account,{account,Id,_Name,_Surname,Amount}}, 
#state{pid = Pid, count = CurrentCount} = State) -> 
  if
    Count == CurrentCount + 1 -> business_logic:make_account(Id, Amount),

      {noreply, #state{pid = Pid , count = Count}};
    true -> {noreply, State}
  end;
handle_cast({register, Pid, Counter}, {RegisteredProcessesState, Count}) ->
  NewRegisteredProcessesState = sets:add_element(Pid, RegisteredProcessesState),
  AllEvents = events:get_events_from(Counter + 1),
  send_events(AllEvents, Pid),

  {noreply, {NewRegisteredProcessesState, Count}};
handle_cast(Transfer, {RegisteredProcessesState, Count}) -> 
  NewCount = Count + 1,
  RegisteredProcessesAsList = sets:to_list(RegisteredProcessesState),
  lists:foreach(fun (Pid) -> 
      gen_server:cast(Pid, {transfer_service, NewCount, Transfer}) end, 
      RegisteredProcessesAsList
  ),
  {noreply, {RegisteredProcessesState, NewCount}}.


handle_info(hallo, #state{pid = Pid, count = Count} = State) ->
  gen_server:cast(Pid, {register, self(), Count}),
  {noreply, State}.

handle_call(_Get,_From,_State) ->{}.


event1() -> {account_service,1,create_account,{account,1,kevin,tchouente,200}}.


broadcast_new_transfer(Transfer) -> gen_server:cast(transfer_broadcast, Transfer).

send_events([], _) -> [];
send_events(EventsList, Pid) -> 
    lists:foreach(fun (Event) -> 
        gen_server:cast(Pid, {transfer_service, Event#event.number, Event#event.payload}) end, 
        EventsList
    ).
