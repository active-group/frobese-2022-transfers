-type unique_id() :: integer().
-type account_number() :: integer().
-type money() :: number().

-record(account,
    {account_number :: account_number(),
     amount :: money()}).
-record(transfer, 
    {id :: unique_id(), 
     timestamp :: erlang:timestamp(), 
     from_acc_nr :: account_number(), 
     to_acc_nr :: account_number(), 
     amount :: money()}).

-record(transferEvent, {
    id :: unique_id(),
    timestamp :: erlang:timestamp(), 
    from_acc_nr :: account_number(), 
    to_acc_nr :: account_number(), 
    amount :: money()}).

-record(sendEvent, {
    service :: transfer_service, 
    count :: number(), 
    event :: #transferEvent{}}).
