-module(login_manager).
-export ([createAccount/4, closeAccount/4, login/4, logout/4, auth/3, ranking/1]).
-import(files, [writeAccounts/1]).

createAccount(UsersMap, User, Password, From) ->
    case maps:is_key(User, UsersMap) of
        true ->
            From ! user_exists,
            UsersMap; % Retorna map inalterado
        false ->
            if Password == "" ->
                   From ! invalid_password,
                   UsersMap; % Retorna map inalterado
               true ->
                    NewData = {Password, 1, false, 0, 0},
                    NewUsers = maps:put(User, NewData, UsersMap),
                    io:format("Conta criada: User=~p, Data=~p~n", [User, NewData]),
                    writeAccounts(NewUsers),
                    From ! done,
                    NewUsers % Retorna o novo map
            end 
    end. 

closeAccount(UsersMap, User, Password, From) ->
    case maps:find(User, UsersMap) of
        {ok, {Pass, _, _, _, _}} ->
            if Pass == Password ->
                   NewUsers = maps:remove(User, UsersMap),
                   io:format("Conta apagada: ~p~n", [User]),
                   writeAccounts(NewUsers),
                   From ! done,
                   NewUsers; % Retorna novo map
               true ->
                   io:format("Password incorreta para apagar ~p~n", [User]),
                   From ! invalid_password,
                   UsersMap % Retorna map original
            end; 
        error ->
             io:format("Erro ao remover conta inexistente: ~p~n.", [User]),
             From ! invalid_account,
             UsersMap % Retorna map original
    end.

login(UsersMap, User, Password, From) ->
    case maps:find(User, UsersMap) of
        {ok, {Pass, Level, OnlineStatus, Wins, Losses}} ->
            if 
                OnlineStatus == true ->
                    From ! already_auth,
                    UsersMap;
                Pass == Password ->
                   From ! {done, Level, Wins, Losses},
                   UpdatedData = {Pass, Level, true, Wins, Losses}, % Marca como online
                   maps:put(User, UpdatedData, UsersMap); % Retorna map atualizado
               true ->
                   From ! invalid_password,
                   UsersMap
            end;
        error ->
            From ! invalid_account,
            UsersMap
    end.

logout(UsersMap, User, Password, From) ->
    case maps:find(User, UsersMap) of
        {ok, {Pass, Level, _, Wins, Losses}} -> % Lê Wins/Losses
            if Pass == Password ->
                   From ! done,
                   UpdatedData = {Pass, Level, false, Wins, Losses}, % Marca como offline, MANTÉM stats
                   maps:put(User, UpdatedData, UsersMap);
               true ->
                   From ! invalid_password,
                   UsersMap
            end;
        error ->
            From ! invalid_account,
            UsersMap
    end.

auth(UsersMap, User, Password) ->
    case maps:find(User, UsersMap) of
        {ok, {Pass, _, Online, _, _}} -> (Pass == Password) andalso Online;
        error -> false
    end.

ranking(UsersMap) ->
    List = maps:to_list(UsersMap),
    Sorted = lists:sort(fun({_, {_, Level1, _, Wins1, _}}, {_, {_, Level2, _, Wins2, _}}) ->
                            if  Level1 > Level2 -> true;
                                Level1 < Level2 -> false;
                                Level1 == Level2 -> Wins1 >= Wins2
                            end
                        end, List),
    lists:sublist([{User, Level, Wins} || {User, {_, Level, _, Wins, _}} <- Sorted], 10).