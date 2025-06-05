public enum State {
  MENU,               // Ecrã inicial
  CREATE_USERNAME,    // Pedir username para registo
  CREATE_PASSWORD,    // Pedir password para registo
  USERNAME,           // Pedir username para login
  PASSWORD,           // Pedir password para login
  LOGIN_MENU,         // Menu após login bem sucedido
  LOGOUT,             // Ação de logout
  REMOVE,             // Ação de remover conta
  RANKING_MENU,       // Ecrã da tabela classificativa
  JOIN,               // Ação de procurar jogo
  TEMPORARY_LOBBY,    // Ecrã "À procura de adversário..."
  GAME,               // Durante o jogo
  END_GAME            // Ecrã de fim de jogo
}
