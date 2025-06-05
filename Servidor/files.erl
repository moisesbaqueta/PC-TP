-module(files).
-export([readAccounts/0, parseList/2, writeAccounts/1, parse/1, parseUser/1]).
-import(string, [split/2, join/2]).


readAccounts() ->
    ResFile = file:read_file("accounts.txt"),
    case ResFile of
        {error, _Reason} ->
            {ok, NewFile} = file:open("accounts.txt", [write]),
            file:close(NewFile),
            #{};
        {ok, <<>>} -> #{};
        {ok, File} ->
            FileStr = binary_to_list(File),
            AllLines = string:split(FileStr, "\n", all),
            ValidLines = [Line || Line <- AllLines, Line /= ""],
            parseList(ValidLines, #{})
    end.

parseList([], Users) -> Users;
parseList([H|T], Users) ->
        case string:split(H, ".", all) of % Divide a linha inteira por "."
            [UserStr, PassStr, LevelStr, WinsStr, LossesStr] ->
                User = UserStr,
                Password = PassStr, 
                Level = list_to_integer(LevelStr),
                Wins = list_to_integer(WinsStr),
                Losses = list_to_integer(LossesStr),
                NewData = {Password, Level, false, Wins, Losses},
                NewUsersMap = maps:put(User, NewData, Users),
                parseList(T, NewUsersMap)
        end.

writeAccounts(Users) ->
    StringToWrite = parse(maps:to_list(Users)),
    BinaryToWrite = list_to_binary(StringToWrite ++ "\n"), % Adiciona newline e converte
    case file:write_file("accounts.txt",BinaryToWrite) of
        ok -> 
            ok;
        {error, Reason} -> 
            io:format("Erro a escrever no ficheiro: ~p~n", [Reason])
    end.

parseUser({User, {Password, Level, _Online, Wins, Losses}}) ->
    string:join([User, Password, integer_to_list(Level), integer_to_list(Wins), integer_to_list(Losses)], ".").

parse(L) ->
    case L of
        [] -> "";
        [H] -> parseUser(H);
        [H | T] -> string:join([parseUser(H), parse(T)], "\n")
    end.