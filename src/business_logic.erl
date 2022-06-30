%% This module represents the business logic layer

-module(business_logic).
-include("data.hrl").
-export([get_account/1, transfer/3, sort_tx/1, get_transfers/1, make_account/2 ]).

-spec get_account(account_number()) -> {ok, #account{}} | {error, any()}.
get_account(AccountNr) -> database:get_account(AccountNr).

-spec make_account(unique_id(), money()) -> #account{}.
make_account(AccNr, Balance) ->
    Acc = #account{account_number = AccNr,
                   amount = Balance},
    database:put_account(Acc),
    Acc.

-spec get_transfers(unique_id()) -> list(#transfer{}).
get_transfers(Id) ->
     database:get_all_transfers(Id).

%% Takes a sender & receiver account number and an amount and transfers 
%% that amount from sender to receiver.
%% Crashes if accounts do not exist.
%% Returns {ok, tid}, where tid is the id of the stored transfer
%% or {error, insufficient_funds} when the sender does not have enough money
%% in his account.

-spec transfer(account_number(), account_number(), money()) -> 
     {error, sender_account_not_found | receiver_account_not_found | insufficient_funds}
   | {ok, unique_id()}.
transfer(SenderAccountNumber, ReceiverAccountNumber, Amount) ->

    Transfer = 
      fun() -> 
        MaybeAccSender = database:get_account(SenderAccountNumber),
        MaybeAccReceiver = database:get_account(ReceiverAccountNumber),
        case {MaybeAccSender, MaybeAccReceiver} of
            {{error, not_found}, _} -> {error, sender_account_not_found};
            {_, {error, not_found}} -> {error, receiver_account_not_found};

            {{ok, AccSender}, {ok, AccReceiver}} ->
                AccSenderAmount = AccSender#account.amount,
                AccReceiverAmount = AccReceiver#account.amount,

                if
                    AccSenderAmount - Amount >= 0 ->
                        TxId = database:unique_tx_id(),
                        Tx = #transfer{id = TxId,
                                            timestamp = erlang:timestamp(),
                                            from_acc_nr = SenderAccountNumber,
                                            to_acc_nr = ReceiverAccountNumber,
                                            amount = Amount},
                        NewAccSender = AccSender#account{amount = (AccSenderAmount - Amount)},
                        NewAccReceiver = AccReceiver#account{amount = (AccReceiverAmount + Amount)},
                        database:put_transfer(Tx),
                        transfer_feed:broadcast_new_transfer(Tx),
                        database:put_account(NewAccSender),
                        database:put_account(NewAccReceiver),
                        {ok, TxId};
                    true ->
                        {error, insufficient_funds}
                end
        end
      end,

    database:atomically(Transfer).

%% Takes a list of transfers and returns them sorted by their id (asc)

sort_tx(Txs) ->
    lists:sort(fun(Tx1, Tx2) -> Tx2#transfer.id < Tx1#transfer.id end, Txs).
