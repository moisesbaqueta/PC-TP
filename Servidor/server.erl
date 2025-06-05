-module(server).
-import(files, [readAccounts/0, writeAccounts/1]).
-import(login_manager, [createAccount/4, closeAccount/4, login/4, logout/4, auth/3, ranking/1]).
-export([start/1]).

-define(GAME_DURATION_MS, 20000). % 20 segundos (TEMPO DO JOGO)
-define(POINTS_PER_COLLISION, 2). % Pontos por colisão
-define(POINTS_PER_HIT, 1). % Ponto por acerto/tiro

start(Port) ->
    spawn(fun() -> server(Port) end).

server(Port) ->
    {ok, LSock} = gen_tcp:listen(Port, [binary, {packet,line}, {reuseaddr, true}, {active, true}]),
    io:format("Servidor escutando na porta ~p~n", [Port]),
    UsersPid = spawn(fun() -> loop_init(files:readAccounts()) end),
    Lobby = spawn(fun() -> lobby([], maps:new(), UsersPid) end),
    UsersPid ! {set_lobby_pid, Lobby},
    spawn(fun() -> acceptor(LSock, UsersPid) end),
    wait_loop().

wait_loop() ->
   receive
        stop -> ok;
        _ -> wait_loop()
    end.

loop_init(UsersMap) ->
    receive
        {set_lobby_pid, ActualLobby} ->
            io:format("Loop: Lobby (~p) recebido. Iniciando loop principal.~n", [ActualLobby]),
            loop(UsersMap, ActualLobby); % Chama o loop principal com o PID correto

        Other -> % Descarta outras mensagens que cheguem antes do PID
            io:format("Loop Init: Ignorando mensagem inesperada: ~p~n", [Other]),
            loop_init(UsersMap) % Continua a esperar pelo PID
    end.


loop(UsersMap, Lobby) ->
    receive
        {createAccount, User, Password, From} ->
            NewUsers = login_manager:createAccount(UsersMap, User, Password, From),
            loop(NewUsers, Lobby);
        {close_account, User, Password, From} ->
            NewUsers = login_manager:closeAccount(UsersMap, User, Password, From),
            loop(NewUsers, Lobby);
        {login, User, Password, From} ->
            NewUsers = login_manager:login(UsersMap, User, Password, From),
            loop(NewUsers, Lobby);
        {logout, User, Password, From} ->
            NewUsers = login_manager:logout(UsersMap, User, Password, From),
            loop(NewUsers, Lobby);
        {auth, User, Password, From} ->
            case login_manager:auth(UsersMap, User, Password) of
                true ->
                    From ! {done, Lobby};
                false ->
                    From ! invalid_auth
            end,
            loop(UsersMap, Lobby);
        {online, From} ->
            OnlineUsers = [Username || {Username, {_ , _ , true, _, _}} <- maps:to_list(UsersMap)],
            From ! OnlineUsers,
            loop(UsersMap, Lobby);
        {classification, From} ->
            RankingList = login_manager:ranking(UsersMap),
            try
                FormattedEntries = [lists:flatten(io_lib:format("~s ~p ~p",[User,Level, Wins])) || {User,Level, Wins} <- RankingList],
                ResponseString = string:join(FormattedEntries, "|"),
                io:format("Loop: Enviando Ranking String para ~p: ~s~n",[From,ResponseString]),
                From ! {ranking_result, ResponseString} % Envia string com tag específica
            catch
                error:Reason ->
                    io:format("Loop: ERRO formatar ranking: ~p~n",[Reason]),
                    From ! {ranking_result, ""} % Envia string vazia em erro
            end,
            loop(UsersMap, Lobby);
        {join_lobby, User, Password, From} ->
            case maps:find(User, UsersMap) of
                {ok, {Pass, Level, true, _, _}} when Pass == Password ->
                    Lobby ! {join, User, Level, From}, %Envia para o lobby, passa o PID do client
                    From ! searching;
                _ ->
                    From ! invalid_lobby_join
            end,
            loop(UsersMap, Lobby);
        {update_position, Username, X, Y, _FromPid} -> %position?
            Lobby ! {update_position, Username, X, Y},
            loop(UsersMap, Lobby);
        {collision, Username, FromPid} ->
            io:format("Loop: Recebida colisão de ~p (PID: ~p), encaminhando para Lobby~n", [Username, FromPid]),
            Lobby ! {collision_event, Username, FromPid},
            loop(UsersMap, Lobby);
        {fire_projectile, Username, X, Y, DX, DY, Id} ->
            Lobby ! {relay_projectile, Username, X, Y, DX, DY, Id},
            loop(UsersMap, Lobby);
        {hit, Username, Id} ->
            Lobby ! {relay_hit, Username, Id},
            loop(UsersMap, Lobby);
        {mod_spawn, Username, Id, Type, X, Y} ->
            Lobby ! {relay_mod_spawn, Username, Id, Type, X, Y},
            loop(UsersMap, Lobby);
        {modifier_collected, Username, ModId} ->
            Lobby ! {relay_modifier_collected, Username, ModId},
            loop(UsersMap, Lobby);
        {game_result, Winner, Loser} ->
            io:format("Loop: Recebido game_result. Vencedor=~p, Perdedor=~p~n", [Winner, Loser]),
            NewUsersMap =
                try
                    case {maps:find(Winner, UsersMap), maps:find(Loser, UsersMap)} of
                        {{ok, {PassW, LvlW, OnW, WinsW, _LossesW}}, {ok, {PassL, LvlL, OnL, _WinsL, LossesL}}} ->
                            %  Atualiza Vencedor
                            NewWinsW = WinsW + 1, %incrementa
                            NewLossesW = 0, % Reset derrotas do vencedor
                            {NewLvlW, FinalWinsW} = if NewWinsW >= LvlW andalso LvlW > 0 -> % Level up
                                                        io:format("Loop: ~p LEVEL UP! (~p -> ~p)~n", [Winner, LvlW, LvlW+1]),
                                                        {LvlW + 1, 0}; % Sobe nível %% NewWinsW
                                                      true ->
                                                        {LvlW, NewWinsW} % Mantém nível
                                                    end,
                            UpdatedWinnerData = {PassW, NewLvlW, OnW, FinalWinsW, NewLossesW}, % Guarda NewWinsW

                            % Atualiza Perdedor
                            NewWinsL = 0, % Reset vitórias
                            NewLossesL = LossesL + 1, % Incrementa derrotas
                            LossesNeeded = (LvlL + 1) div 2, % N/2
                            {NewLvlL, FinalLossesL} = if NewLossesL >= LossesNeeded andalso LvlL > 1 -> % Level down
                                                        io:format("Loop: ~p LEVEL DOWN! (~p -> ~p)~n", [Loser, LvlL, LvlL-1]),
                                                        {LvlL - 1, 0}; % Desce nível %% NewLossesL
                                                      true ->
                                                        {LvlL, NewLossesL} % Mantém nível
                                                      end,
                            UpdatedLoserData = {PassL, NewLvlL, OnL, NewWinsL, FinalLossesL}, % Guarda NewLossesL

                            % 3. Atualiza Map e guarda
                            TempMap = maps:put(Winner, UpdatedWinnerData, UsersMap),
                            UpdatedMap = maps:put(Loser, UpdatedLoserData, TempMap),
                            io:format("Loop: Stats W[~p]:~p L[~p]:~p~n", [Winner, UpdatedWinnerData, Loser, UpdatedLoserData]),
                            files:writeAccounts(UpdatedMap),
                            UpdatedMap;
                        {ErrorWin, ErrorLoser} ->
                            io:format("Loop ERROR: Dados não encontrados para V[~p] ou D[~p]. Erros: ~p, ~p~n", [Winner, Loser, ErrorWin, ErrorLoser]),
                            UsersMap % Continua com map antigo
                    end
                catch
                    error:Reason:Stacktrace ->
                        io:format("Loop CRASH em game_result: ~p~nStack: ~p~n", [Reason, Stacktrace]),
                        UsersMap
                end,
            loop(NewUsersMap, Lobby);

        {'EXIT', _Pid, _Reason} -> %Se o Lobby terminar, pode ser necessário
            io:format("Loop: Processo ~p terminou: ~p~n", [_Pid, _Reason]),
            loop(UsersMap, Lobby) % Continua por agora
    end.

% Mapa de Jogos (GamesMap): #{ Username => {OpponentPid, OpponentName, CurrentScore} }
lobby(Queue, GamesMap, UsersPid) ->
    receive
        {join, User, Level, FromPid} -> %FromPid é o PID do processo 'client'
            io:format("Lobby: Jogador ~p (PID: ~p) entrou na fila com nível ~p~n", [User, FromPid, Level]),
            % Evitar adicionar o mesmo PID duas vezes
            IsAlreadyInQueue = lists:any(fun({_, P, _}) -> P == FromPid end, Queue),
            IsAlreadyInGame = maps:fold(fun(_, {P, _, _}, Acc) -> Acc or (P == FromPid) end, false, GamesMap),

            if
                IsAlreadyInQueue orelse IsAlreadyInGame ->
                    io:format("Lobby Warning: ~p (PID: ~p) já está na fila ou em jogo.~n", [User, FromPid]),
                    lobby(Queue, GamesMap, UsersPid);
                true ->
                    NewQueue = Queue ++ [{User, FromPid, Level}],
                    case find_match(NewQueue) of
                        {ok, Player1, Player2, RestQueue} ->
                            {User1, Pid1, _} = Player1,
                            {User2, Pid2, _} = Player2,
                            io:format("Lobby: Emparelhando ~p (~p) com ~p (~p)~n", [User1, Pid1, User2, Pid2]),
                             % --- Atualiza GamesMap com nova estrutura e score inicial 0 ---
                            P1Info = {Pid2, User2, 0}, % {OpponentPid, OpponentName, Score}
                            P2Info = {Pid1, User1, 0}, % {OpponentPid, OpponentName, Score}
                            NewGamesMap = GamesMap#{User1 => P1Info, User2 => P2Info},
                            % --- Envia mensagens iniciais (game/2 não é mais necessário para isto) ---
                            Pid1 ! {opponent_name, User2},
                            Pid2 ! {opponent_name, User1},
                            Pid1 ! start_game,
                            Pid2 ! start_game,
                            % --- Envia Score Update inicial (0-0) ---
                            {P1ScoreToSend, P2ScoreToSend} = determine_scores_for_msg(User1, User2, 0, 0),
                            send_score_update(Pid1, Pid2, P1ScoreToSend, P2ScoreToSend),

                            % --- Inicia Timer ---
                            GameId = create_game_id(User1, User2), % ID consistente para o jogo
                            _TimerRef = erlang:send_after(?GAME_DURATION_MS, self(), {timeout, GameId}), % Usa send_after
                            io:format("Lobby: Jogo ~p iniciado. Timer de ~p ms iniciado.~n", [GameId, ?GAME_DURATION_MS]),
                    
                            lobby(RestQueue, NewGamesMap, UsersPid);
                        no_match ->
                            lobby(NewQueue, GamesMap, UsersPid)
                    end
            end;

        {leave, User, FromPid} -> %Sair da fila
            io:format("Lobby: ~p (PID: ~p) saiu da fila.~n", [User, FromPid]),
            NewQueue = remove_user_from_queue_by_pid(FromPid, Queue),
            FromPid ! leave_done, % Envia para o processo client
            lobby(NewQueue, GamesMap, UsersPid);

        {update_position, Username, X, Y} ->
            case maps:find(Username, GamesMap) of
                {ok, {OpponentPid, _OpponentName, _Score}} ->
                    io:format("Lobby: Reenviando posição de ~p para PID ~p~n", [Username, OpponentPid]),
                    OpponentPid ! {opponent_position, X, Y};
                error -> 
                    io:format("Lobby Warning: Jogador ~p não encontrado em GamesMap para pos update.~n", [Username]),
                    ok %Ignora se o jogo já acabou ou o jogador saiu
            end,
            lobby(Queue, GamesMap, UsersPid);

        {collision_event, Username, _CollidingPid} -> % CollidingPid pode ser útil para debug
            io:format("Lobby: Evento de colisão recebido para ~p~n", [Username]),
            case maps:find(Username, GamesMap) of
                {ok, {MyPid, OpponentName, MyScore}} -> % Encontra info do jogador que colidiu
                    case maps:find(OpponentName, GamesMap) of
                        {ok, {OpponentPid, _, OpponentScore}} -> % Encontra info do oponente
                            NewOpponentScore = OpponentScore + ?POINTS_PER_COLLISION,
                            io:format("Lobby: Pontos para ~p! Score ~p -> ~p~n", [OpponentName, OpponentScore, NewOpponentScore]),

                             % Cria a info atualizada do oponente
                            UpdatedOpponentInfo = {OpponentPid, Username, NewOpponentScore},
                             % Atualiza o GamesMap
                            NewGamesMap = maps:update(OpponentName, UpdatedOpponentInfo, GamesMap),

                             % Enviar atualização para AMBOS os jogadores
                            {P1ScoreToSend, P2ScoreToSend} = determine_scores_for_msg(Username, OpponentName, MyScore, NewOpponentScore),
                            send_score_update(MyPid, OpponentPid, P1ScoreToSend, P2ScoreToSend),

                            lobby(Queue, NewGamesMap, UsersPid); % Continua com mapa atualizado
                        error ->
                            io:format("Lobby Error: Oponente ~p de ~p não encontrado para dar pontos.~n", [OpponentName, Username]),
                            lobby(Queue, GamesMap, UsersPid)
                    end;
                error ->
                    io:format("Lobby Warning: Jogador ~p que colidiu não encontrado no GamesMap.~n", [Username]),
                    lobby(Queue, GamesMap, UsersPid)
            end;

        {relay_projectile, Username, X, Y, DX, DY, Id} ->
            case maps:find(Username, GamesMap) of
                {ok, {OpponentPid, _OpponentName, _Score}} ->
                    Msg = io_lib:format("projectile_spawn#~s ~p ~p ~p ~p ~s", [Username, X, Y, DX, DY, Id]),
                    OpponentPid ! {raw_msg, Msg};
                error -> ok
            end,
            lobby(Queue, GamesMap, UsersPid);
        
        {relay_hit, Username, Id} ->
            case maps:find(Username, GamesMap) of
                {ok, {MyPid, OpponentName, MyScore}} -> % Encontra info do jogador que colidiu
                    case maps:find(OpponentName, GamesMap) of
                        {ok, {OpponentPid, _, OpponentScore}} -> % Encontra info do oponente
                            NewOpponentScore = OpponentScore + ?POINTS_PER_HIT,
                            io:format("Lobby: Pontos para ~p! Score ~p -> ~p~n", [OpponentName, OpponentScore, NewOpponentScore]),

                             % Cria a info atualizada do oponente
                            UpdatedOpponentInfo = {OpponentPid, Username, NewOpponentScore},
                             % Atualiza o GamesMap
                            NewGamesMap = maps:update(OpponentName, UpdatedOpponentInfo, GamesMap),

                             % Enviar atualização para AMBOS os jogadores
                            {P1ScoreToSend, P2ScoreToSend} = determine_scores_for_msg(Username, OpponentName, MyScore, NewOpponentScore),
                            %%send_score_update(MyPid, OpponentPid, P1ScoreToSend, P2ScoreToSend),

                            Msg1 = io_lib:format("remove_projectile#~s", [Id]),
                            Msg2 = io_lib:format("score_update#~p ~p", [P1ScoreToSend, P2ScoreToSend]),

                            MyPid ! {relay_hit, Msg1, Msg2},
                            OpponentPid ! {relay_hit, Msg1, Msg2},

                            lobby(Queue, NewGamesMap, UsersPid); % Continua com mapa atualizado
                        error ->
                            io:format("Lobby Error: Oponente ~p de ~p não encontrado para dar pontos.~n", [OpponentName, Username]),
                            lobby(Queue, GamesMap, UsersPid)
                    end;
                error ->
                    io:format("Lobby Warning: Jogador ~p que colidiu não encontrado no GamesMap.~n", [Username]),
                    lobby(Queue, GamesMap, UsersPid)
            end;

        {relay_mod_spawn, Username, Id, Type, X, Y} ->
            case maps:find(Username, GamesMap) of
                {ok, {OpponentPid, _OpponentName, _Score}} ->
                    Msg = io_lib:format("modifier_spawn#~s ~s ~p ~p", [Id, Type, X, Y]),
                    OpponentPid ! {raw_msg, Msg};
                error -> ok
            end,
            lobby(Queue, GamesMap, UsersPid);

        {relay_modifier_collected, Username, ModId} ->
            case maps:find(Username, GamesMap) of
                {ok, {OpponentPid, _OpponentName, _Score}} ->
                        Msg = io_lib:format("modifier_collected#~s", [ModId]),
                        OpponentPid ! {raw_msg, Msg};
                error -> ok
            end,    
            lobby(Queue, GamesMap, UsersPid);
        
        {timeout, GameId = {User1, User2}} ->
            io:format("Lobby: Tempo esgotado para o jogo ~p (~p vs ~p).~n", [GameId, User1, User2]),
            io:format("Lobby DEBUG: Processando timeout para ~p vs ~p~n", [User1, User2]),
            case {maps:find(User1, GamesMap), maps:find(User2, GamesMap)} of
                {{ok, {Pid1, _, Score1}}, {ok, {Pid2, _, Score2}}} -> % Encontrou ambos
                    io:format("Lobby: Fim de Jogo! Score Final: ~p (~p) vs ~p (~p)~n", [User1, Score1, User2, Score2]),

                    % Determina scores finais P1/P2 para a mensagem
                    {FinalP1Score, FinalP2Score} = determine_scores_for_msg(User1, User2, Score1, Score2),
                    if FinalP1Score == -1 orelse FinalP2Score == -1 ->
                        io:format("Lobby: Jogo ~p terminou por desconexão.~n", [GameId]);
                        Score1 == Score2 ->
                            io:format("Lobby: Jogo ~p EMPATE. Level mantido.~n", [GameId]);
                        Score1 > Score2 ->
                            io:format("Lobby: ~p GANHOU. Informando loop.~n", [User1]),
                            UsersPid ! {game_result, User1, User2};
                        Score1 < Score2 -> 
                            io:format("Lobby: ~p GANHOU. Informando loop.~n", [User2]),
                            UsersPid ! {game_result, User2, User1}
                    end, 

                    io:format("Lobby DEBUG: Enviando end_game para ~p e ~p com scores ~p, ~p~n", [Pid1, Pid2, FinalP1Score, FinalP2Score]),
                    % Envia mensagem de fim de jogo para os PIDs dos processos 'client'
                    Pid1 ! {end_game, FinalP1Score, FinalP2Score},
                    Pid2 ! {end_game, FinalP1Score, FinalP2Score},

                    % Limpa o jogo do GamesMap
                    CleanGamesMap = maps:remove(User1, maps:remove(User2, GamesMap)),
                    io:format("Lobby: Jogo ~p removido do GamesMap.~n", [GameId]),
                    lobby(Queue, CleanGamesMap, UsersPid);
                _ -> % Um ou ambos não encontrados (talvez já desconectaram?)
                    io:format("Lobby Warning: Timeout para ~p, mas um ou ambos jogadores não encontrados.~n", [GameId]),
                    CleanGamesMap = maps:remove(User1, maps:remove(User2, GamesMap)), % Limpa o que houver
                    lobby(Queue, CleanGamesMap, UsersPid)
            end;

        {'EXIT', Pid, Reason} ->
            io:format("Lobby: Processando EXIT de ~p: ~p (Fila: ~p, Mapa: ~p)~n",
            [Pid, Reason, Queue, GamesMap]), % Log com estado atual

            % 1. Limpa a Queue
            NewQueue = remove_user_from_queue_by_pid(Pid, Queue),
            io:format("Lobby EXIT: Nova fila: ~p~n", [NewQueue]),

            % 2. Verifica e Limpa o GamesMap
            FoundGame = maps:filter(fun(_, {P, _, _}) -> P == Pid end, GamesMap),
            UpdatedGamesMap = case maps:size(FoundGame) of
                                1 -> % Estava em jogo
                                    [{Username, {_, OpponentName, _}}] = maps:to_list(FoundGame),
                                    io:format("Lobby EXIT: ~p (~p) saiu do jogo vs ~p.~n", [Username, Pid, OpponentName]),
                                    case maps:find(OpponentName, GamesMap) of
                                        {ok, {OppPid, _, _}} ->
                                            io:format("Lobby EXIT: Notificando ~p (~p).~n", [OpponentName, OppPid]),
                                            {P1S,P2S} = determine_scores_for_msg(Username, OpponentName, -1, -1),
                                            OppPid ! {end_game, P1S, P2S};
                                        error -> ok
                                    end,
                                    maps:remove(Username, maps:remove(OpponentName, GamesMap)); % Retorna mapa limpo
                                _ -> % Não estava em jogo
                                    io:format("Lobby EXIT: PID ~p não estava em jogo.~n", [Pid]),
                                    GamesMap % Retorna mapa original
                              end,
            io:format("Lobby EXIT: Novo mapa: ~p~n", [UpdatedGamesMap]),

            % 3. Continua o loop com a fila e mapa atualizados
            lobby(NewQueue, UpdatedGamesMap, UsersPid)
    
    end.

% Garante ordem consistente User1 vs User2
create_game_id(UserA, UserB) ->
    if UserA < UserB -> {UserA, UserB}; true -> {UserB, UserA} end.

% Determina qual score enviar como primeiro e segundo na mensagem (P1 é alfabeticamente menor)
determine_scores_for_msg(UserA, UserB, ScoreA, ScoreB) ->
    if UserA < UserB -> {ScoreA, ScoreB}; % UserA é P1
        true -> {ScoreB, ScoreA} % UserB é P1
    end.

% Envia mensagem de score update para os PIDs dos processos 'client'
send_score_update(Pid1, Pid2, P1Score, P2Score) -> %%%%%%%
    Msg = {score_update, P1Score, P2Score}, % Envia tupla para o processo 'client'
    Pid1 ! Msg,
    Pid2 ! Msg.

find_match([]) -> no_match;
find_match([{User1, Pid1, Level1} | T] = Queue) ->
    io:format("find_match: Tentando par para ~p. Fila atual (~p): ~p~n", [User1, length(Queue), Queue]),
    case find_pair(User1, Pid1, Level1, T, []) of
        {ok, {User2, Pid2, Level2}, RestOfT} ->
            io:format("find_match: Par para ~p ENCONTRADO (~p). Resto de T: ~p~n", [User1, User2, RestOfT]),
            {ok, {User1, Pid1, Level1}, {User2, Pid2, Level2}, RestOfT};
        not_found ->
            io:format("find_match: Par para ~p NÃO encontrado. Chamando recursivo para T: ~p~n", [User1, T]),
            case find_match(T) of
                {ok, PlayerA, PlayerB, RemainingQueue} ->
                    io:format("find_match: Chamada recursiva encontrou par (~p vs ~p). Adicionando ~p de volta. Fila restante: ~p~n", [PlayerA, PlayerB, User1, [{User1, Pid1, Level1} | RemainingQueue]]),
                    {ok, PlayerA, PlayerB, [{User1, Pid1, Level1} | RemainingQueue]};
                no_match ->
                    io:format("find_match: Chamada recursiva NÃO encontrou par.~n"),
                    no_match

            end
    end.

find_pair(_, _, _, [], _) -> not_found;
find_pair(User1, Pid1, Level1, [{User2, Pid2, Level2} | T], Acc) ->
    io:format("find_pair: Comparando ~p(L~p) com ~p(L~p)~n", [User1, Level1, User2, Level2]),
    if User1 =/= User2 andalso abs(Level1 - Level2) =< 1 ->
        io:format("find_pair: PAR ENCONTRADO! ~p vs ~p~n", [User1, User2]),
        {ok, {User2, Pid2, Level2}, Acc ++ T};
    true ->
        find_pair(User1, Pid1, Level1, T, Acc ++ [{User2, Pid2, Level2}])
    end.

%Remove pela igualdade do Pid
remove_user_from_queue_by_pid(_, []) -> [];
remove_user_from_queue_by_pid(Pid, [{_, Pid, _} | T]) ->
     remove_user_from_queue_by_pid(Pid, T);
remove_user_from_queue_by_pid(Pid, [H | T]) ->
     [H | remove_user_from_queue_by_pid(Pid, T)].

acceptor(LSock, UsersPid) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    io:format("Cliente conectado! Socket: ~p~n", [Sock]),
    spawn(fun() -> acceptor(LSock, UsersPid) end),
    client(Sock, UsersPid).

%Processo Client (1 por socket)
client(Sock, UsersPid) ->
    io:format("Client (~p): Nova instância iniciada.~n", [self()]),
    receive
        {tcp, Sock, Data} ->
            try
                String = binary_to_list(string:trim(Data, trailing, "\n")),
                clientInput(String, Sock, UsersPid, self()), % Passa o próprio PID para o Input
                client(Sock, UsersPid) % Continua o loop
            catch
                 error:Reason ->
                    io:format("Client (~p): Erro ao processar input '~p': ~p. Fechando socket.~n", [self(), Data, Reason]),
                    gen_tcp:close(Sock) % Fecha em erro grave
            end;

        {tcp_closed, Sock} ->
            io:format("Client ~p desconectou-se do servidor.~n \n", [self()]),
            ok;

        {error, Sock, Reason} ->
            io:format("Erro na conexão ~p: ~p~n \n", [self(), Reason]),
            ok;

        %  Mensagens recebidas do Lobby 
        {opponent_position, X, Y} ->
            Msg = io_lib:format("opponent_position#~f ~f", [X, Y]), % Usa ~f para float
            % io:format("Client (~p): Enviando para socket: ~s~n", [self(), Msg]),
            gen_tcp:send(Sock, list_to_binary(Msg ++ "\n")),
            client(Sock, UsersPid);
        {opponent_name, Opponent} ->
            Msg = io_lib:format("opponent#~s", [Opponent]), % Usa ~s para string/átomo
            % io:format("Client (~p): Enviando para socket: ~s~n", [self(), Msg]),
            gen_tcp:send(Sock, list_to_binary(Msg ++ "\n")),
            client(Sock, UsersPid);
        start_game ->
            % io:format("Client (~p): Enviando para socket: start_game~n", [self()]),
            gen_tcp:send(Sock, <<"start_game\n">>), % Envia como binário
            client(Sock, UsersPid);
        {score_update, P1Score, P2Score} -> % <<< NOVO para receber do Lobby
             Msg = io_lib:format("score_update#~p ~p", [P1Score, P2Score]),
             io:format("Client (~p): Enviando para socket: ~s~n", [self(), Msg]),
             gen_tcp:send(Sock, list_to_binary(Msg ++ "\n")),
             client(Sock, UsersPid);
        {end_game, FinalP1Score, FinalP2Score} ->
            Msg = io_lib:format("end_game#~p ~p", [FinalP1Score, FinalP2Score]),
            io:format("Client (~p): Enviando para socket: ~s~n", [self(), Msg]),
            gen_tcp:send(Sock, list_to_binary(Msg ++ "\n")),
            io:format("Client (~p): Fim de jogo enviado. Continuando loop.~n", [self()]),
            client(Sock, UsersPid); 
        leave_done -> % Resposta do lobby ao leave
            gen_tcp:send(Sock, <<"leave_done\n">>),
            client(Sock, UsersPid);

        {raw_msg, Msg} ->
            gen_tcp:send(Sock, list_to_binary(Msg ++ "\n")),
            client(Sock, UsersPid);
        {relay_hit, Msg1, Msg2} ->
            gen_tcp:send(Sock, list_to_binary(Msg1 ++ "\n")),
            gen_tcp:send(Sock, list_to_binary(Msg2 ++ "\n")),
            client(Sock, UsersPid);

        % Mensagens de resposta do loop/login_manager
        done -> gen_tcp:send(Sock, <<"done\n">>), client(Sock, UsersPid);
        user_exists -> gen_tcp:send(Sock, <<"user_exists\n">>), client(Sock, UsersPid);
        invalid_password -> gen_tcp:send(Sock, <<"invalid_password\n">>), client(Sock, UsersPid);
        invalid_account -> gen_tcp:send(Sock, <<"invalid_account\n">>), client(Sock, UsersPid);
        invalid_lobby_join -> gen_tcp:send(Sock, <<"invalid_lobby_join\n">>), client(Sock, UsersPid);
        searching -> gen_tcp:send(Sock, <<"searching\n">>), client(Sock, UsersPid);
        already_auth -> gen_tcp:send(Sock, <<"already_auth\n">>), client(Sock, UsersPid);
        {done, Level, Wins, Losses} when is_integer(Level) -> % Resposta Login OK
            Msg=list_to_binary(io_lib:format("Login efetuado com sucesso ~p ~p ~p\n",[Level, Wins, Losses])),
            gen_tcp:send(Sock,Msg),
            client(Sock,UsersPid);
        {done, _} -> % Resposta Auth OK (ignora o Pid do Lobby)
            gen_tcp:send(Sock, <<"done\n">>), client(Sock,UsersPid);

        {ranking_result, RankingString} when is_list(RankingString) ->
            io:format("Client (~p): Recebido ranking_result, enviando string: ~s~n", [self(), RankingString]), % Log
            gen_tcp:send(Sock, list_to_binary(RankingString ++ "\n")),
            client(Sock, UsersPid);

        OnlineUsers when is_list(OnlineUsers) ->
            IsListOfStringsOrAtoms = lists:all(fun(E) -> is_list(E) orelse is_atom(E) end, OnlineUsers),
            if IsListOfStringsOrAtoms andalso OnlineUsers =/= [] ->
                Response = io_lib:format("~p", [OnlineUsers]),
                gen_tcp:send(Sock, list_to_binary(Response ++ "\n"));
                OnlineUsers == [] ->
                    gen_tcp:send(Sock, <<"[]\n">>);
                true -> ok % Ignora outras listas
             end,
             client(Sock, UsersPid);

        % Ignorar mensagens desconhecidas ou irrelevantes aqui
        {set_opponent_pid, _, _} -> client(Sock, UsersPid); % Ignora
        {auth, _, _} -> client(Sock, UsersPid); % Ignora
        {done, _Lobby} -> gen_tcp:send(Sock, <<"done\n">>), client(Sock, UsersPid); % Resposta Auth
        invalid_auth -> gen_tcp:send(Sock, <<"invalid_auth\n">>), client(Sock, UsersPid); % Resposta Auth

         % --- Cláusula EXIT para tratar morte do processo pai/linkado ---
        {'EXIT', _Pid, _Reason} ->
            io:format("Client (~p): Processo linkado morreu: ~p. Fechando socket.~n", [self(), _Reason]),
            gen_tcp:close(Sock),
            ok;

        _Other -> % Mensagem desconhecida
            io:format("Client (~p): Ignorando msg: ~p~n",[self(),_Other]),
            client(Sock, UsersPid)
    end.

%clean(Str) -> string:trim(Str, both, "\r\n").

%Recebe FromPid (o PID do processo 'client')
clientInput(DataString, _Sock, UsersPid, FromPid) ->
    try
        case string:split(DataString, "#") of
            ["createAccount", Info] -> [Username, Password] = string:split(Info, " "), UsersPid ! {createAccount, Username, Password, FromPid};
            ["closeAccount", Info] -> [Username, Password] = string:split(Info, " "), UsersPid ! {close_account, Username, Password, FromPid};
            ["logout", Info] -> [Username, Password] = string:split(Info, " "), UsersPid ! {logout, Username, Password, FromPid};
            ["login", Info] -> [Username, Password] = string:split(Info, " "), UsersPid ! {login, Username, Password, FromPid};
            ["auth", Info] -> [Username, Password] = string:split(Info, " "), UsersPid ! {auth, Username, Password, FromPid};
            ["online", _] -> UsersPid ! {online, FromPid};
            ["classification", _] -> UsersPid ! {classification, FromPid};
            ["join_lobby", Info] -> [Username, Password] = string:split(Info, " "), UsersPid ! {join_lobby, Username, Password, FromPid};
            ["leave_lobby", Username] -> UsersPid ! {leave, Username, FromPid}; % Envia para o loop que encaminha para lobby

            ["position", Info] ->
                % Assume formato "Username X Y"
                case string:lexemes(Info, " ") of
                    [Username, Xstr, Ystr] ->
                        try
                            {X, _} = string:to_float(Xstr),
                            {Y, _} = string:to_float(Ystr),
                            io:format("Input (~p): Posição ~p: ~p, ~p~n", [FromPid, Username, X, Y]),
                            UsersPid ! {update_position, Username, X, Y, FromPid}
                        catch _:_ -> io:format("Input (~p): Erro ao converter posição: ~p~n", [FromPid, Info])
                        end;
                    _ -> io:format("Input (~p): Mensagem de posição mal formatada: ~p~n", [FromPid, Info])
                end;

            % --- Novo Comando de Colisão ---
            ["collision", UsernameRaw] -> % Assume que o cliente envia o username após #
                Username = string:trim(UsernameRaw),
                io:format("Input (~p): Colisão recebida para ~p~n", [FromPid, Username]),
                UsersPid ! {collision, Username, FromPid}; % Envia para o loop

            ["fire", Info] ->
                % Formato: "Username X Y DX DY"
                case string:lexemes(Info, " ") of
                    [Username, Xs, Ys, DXs, DYs, Id] ->
                        {X, _} = string:to_float(Xs),
                        {Y, _} = string:to_float(Ys),
                        {DX, _} = string:to_float(DXs),
                        {DY, _} = string:to_float(DYs),
                        %Id = string:trim(Ids),
                        UsersPid ! {fire_projectile, Username, X, Y, DX, DY, Id};
                    _ -> io:format("fire: Formato inválido: ~p~n", [Info])
                end;

            ["hit", Info] ->
                case string:lexemes(Info, " ") of
                    [UsernameRaw, IdRaw] ->
                        Username = string:trim(UsernameRaw),
                        Id = string:trim(IdRaw),
                        UsersPid ! {hit, Username, Id};
                    _ -> io:format("hit: formato inválido: ~p~n", [Info])
                end;
            
            ["mod_spawn", Info] ->
                case string:lexemes(Info, " ") of
                    [UsernameRaw, IdRaw, TypeRaw, Xs, Ys] ->
                        Username = string:trim(UsernameRaw),
                        Id = string:trim(IdRaw),
                        Type = string:trim(TypeRaw),
                        {X, _} = string:to_float(Xs),
                        {Y, _} = string:to_float(Ys),
                        UsersPid ! {mod_spawn, Username, Id, Type, X, Y};
                    _ -> io:format("mod_spawn: formato inválido: ~p~n", [Info])
                end;

            ["modifier_collected", Info] ->
                case string:lexemes(Info, " ") of
                    [UsernameRaw, ModIdRaw] ->
                        Username = string:trim(UsernameRaw),
                        ModId = string:trim(ModIdRaw),
                        UsersPid ! {modifier_collected, Username, ModId};
                    _ -> io:format("modifier_collected: Formato inválido: ~p~n", [Info])
                end;

            _Outro ->
                io:format("Input (~p): Comando desconhecido ou formato inválido: ~p~n", [FromPid, DataString])
        end
    catch
        error:Reason:Stacktrace ->
            io:format("Input (~p): CRASH ao processar comando '~p': ~p~nStack: ~p~n", [FromPid, DataString, Reason, Stacktrace])
    end.