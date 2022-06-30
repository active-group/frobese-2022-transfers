-module(transfer).
-export([start/1, handle_cast/2]).

-behavior(gen_server).

start(Start) ->
  gen_server:start(?MODULE, Start, [{debug, [trace]}]).

-record(register, {register :: register,service :: pid(), id :: number()}).
-record(account, {id :: number(), name :: string(), surname :: string(), amount :: number()}).
-record(accountEvent, {service :: account_service, 
count :: number(), 
create_account :: create_account,
account :: #account{}}).

init(Start) ->
  {ok, Start}. 

-spec handle_cast(#accountEvent{}, number()) -> {}.

handle_cast(#accountEvent{count = Count, account = #account{id = ID, amount = Amount}}, N) -> {} .


