-module(transfer).
-export([start/1, handle_cast/2, init/1, handle_call/3, event1/0, handle_info/2]).

-behavior(gen_server).

-record(state, {pid :: number(), count :: number()}).

start(RegistryPid) ->
  gen_server:start(?MODULE, #state{pid = RegistryPid, count = 0}, [{debug, [trace]}]).

init(State) ->
  timer:send_interval(1000, hallo),
  {ok, State}. 

handle_cast({account_service,Count,create_account,{account,Id,_Name,_Surname,Amount}}, 
#state{pid = Pid, count = CurrentCount} = State) -> 
  if
    Count == CurrentCount + 1 -> business_logic:make_account(Id, Amount),
      {noreply, #state{pid = Pid , count = Count}};
    true -> {noreply, State}
  end.

handle_info(hallo, #state{pid = Pid, count = Count} = State) ->
  gen_server:cast(Pid, {register, self(), account_service, Count}),
  {noreply, State}.

handle_call(_Get,_From,_State) ->{}.


event1() -> {account_service,1,create_account,{account,1,kevin,tchouente,200}}.

