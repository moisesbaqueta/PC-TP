import java.io.*;
import java.net.*;
import java.util.*;


// --------------------------------


public class Client {

  private Socket s;
  private BufferedReader in;
  private PrintWriter out;
  private Data data;                        //Referencia ao objeto Data partilhado
  private Display displayApp;               //Referencia ao PApplet para chamar métodos de estado
  StringBuilder sb = new StringBuilder();

  //Guarda a Thread que ouve o servidor durante o jogo
  public Thread serverListenerThread;

  // Construtor principal que recebe a referência ao Display
  public Client(String host, int port, Data data, Display displayApp) throws IOException {
    this.s = new Socket(host, port);
    this.in = new BufferedReader(new InputStreamReader(s.getInputStream()));
    // Usa println e autoFlush=true para simplificar envio
    this.out = new PrintWriter(s.getOutputStream(), true);
    this.data = data;
    this.displayApp = displayApp; // Guarda a ref ao display
    System.out.println("Cliente conectado com referência ao Display."); // Log
  }

  // Construtor secundário (usado no setup inicial)
  public Client(String host, int port, Data data) throws IOException {
    // Chama o construtor principal, mas passa null para displayApp
    this(host, port, data, null);
    System.out.println("WARN: Cliente criado sem referência ao Display. Funcionalidade de jogo limitada até recriar.");
  }
  
  
  //------------------------------------------
  

  public void send(String message) {
      if (out != null) {
          out.println(message);
      } else {
          System.out.println("Erro: Tentando enviar mensagem sem conexão ativa");
      }
  }

  // Recebe uma linha do servidor
  public String receive() throws IOException {
      if (in != null) {
          String line = in.readLine();
          if (line == null) {
                throw new IOException("Conexão fechada pelo servidor");
          }
          return line;
      } else {
            throw new IOException("Erro: Tentando receber mensagem sem conexão ativa.");
      }
  }
  
  public boolean isConnected() {
      return s != null && s.isConnected() && !s.isClosed();
  }

  // -------------------------------------------
  // --- Métodos de Conta, Lobby e Ranking -----
  // -------------------------------------------
  
  
  public void createAccount(String username, String password) throws IOException, InvalidPassword, UserExists {
      sb.setLength(0); // Limpa antes de usar
      sb.append("createAccount#").append(username).append(" ").append(password);
      send(sb.toString());

      String response = receive();
      switch (response) {
          case "done": break;
          case "user_exists": throw new UserExists("Username já existente");
          case "invalid_password": throw new InvalidPassword("Password inválida");
          default: System.err.println("WARN: Resposta inesperada createAccount: " + response);
      }
  }

  public void closeAccount(String username, String password) throws IOException, InvalidPassword, InvalidAccount {
        sb.setLength(0);
      sb.append("closeAccount#").append(username).append(" ").append(password);
      send(sb.toString());

      String response = receive();
      switch (response) {
          case "done": break;
          case "invalid_account": throw new InvalidAccount("Conta inválida");
          case "invalid_password": throw new InvalidPassword("Password inválida");
          default: System.err.println("WARN: Resposta inesperada closeAccount: " + response);
      }
  }

  public void login(String username, String password) throws IOException, InvalidPassword, InvalidAccount, UserAlreadyAuth {
        sb.setLength(0);
      sb.append("login#").append(username).append(" ").append(password);
      send(sb.toString());

      String response = receive();

        if (response.startsWith("Login efetuado com sucesso")) {
            try {
                String[] parts = response.split(" ");
                int lossesValue = Integer.parseInt(parts[parts.length - 1]);
                int winsValue = Integer.parseInt(parts[parts.length - 2]);
                int levelValue = Integer.parseInt(parts[parts.length - 3]);
                synchronized(data){
                  data.losses = lossesValue;
                  data.wins = winsValue;
                  data.level = levelValue;
                  data.username = username; // Guarda username/pass em Data
                  data.password = password;
                }
            } catch (NumberFormatException | ArrayIndexOutOfBoundsException e) {
                throw new IOException("Resposta de login inesperada ou mal formatada: " + response, e);
            }
        } else if (response.equals("invalid_account")) {
            throw new InvalidAccount("Conta inválida");
        } else if (response.equals("invalid_password")) {
            throw new InvalidPassword("Password inválida");
        } else if (response.equals("already_auth")) {
            throw new UserAlreadyAuth("Username já autenticado");
        } else {
            throw new IOException("Resposta inesperada do servidor ao login: " + response);
        }
  }

  public void logout(String username, String password) throws IOException, InvalidPassword, InvalidAccount {
        sb.setLength(0);
      sb.append("logout#").append(username).append(" ").append(password);
      send(sb.toString());

      String response = receive();
      switch (response) {
          case "done": break;
          case "invalid_account": throw new InvalidAccount("Conta inválida");
          case "invalid_password": throw new InvalidPassword("Password inválida");
          default: System.err.println("WARN: Resposta inesperada logout: " + response);
      }
  }

  public List<String> getRanking() throws IOException { // <- Retorna List<RankingEntry>
    System.out.println("Client.getRanking: Enviando 'classification#'...");
    send("classification#");
    String response = null;
    List<String> ranking = new ArrayList<>();

    boolean rankingReceived = false;
    int maxAttempts = 5; int attempts = 0;

    System.out.println("Client.getRanking: Aguardando resposta do ranking...");

    while (!rankingReceived && attempts < maxAttempts) {
        attempts++;
        try {
            response = receive();
            System.out.println("Client.getRanking: Tentativa " + attempts + ", Recebido Raw: " + response);
            if (response == null) 
              throw new IOException("Conexão fechada (null).");

            // Verifica se parece ser o ranking (contém espaço)
            if (response.contains(" ") && !response.equals("done") && !response.equals("[]")) {
                rankingReceived = true;
                if (!response.isEmpty()) {
                    String[] entries = response.split("\\|");
                    for (String entry : entries) {
                        if (!entry.trim().isEmpty()) {
                          ranking.add(entry.trim());
                        }
                    }
                }
                System.out.println("Client.getRanking: Ranking parseado com " + ranking.size() + " entradas.");
            } else {
              System.out.println("Client.getRanking: Linha ignorada: '" + response + "'");
              try { 
                Thread.sleep(50); 
              } catch (InterruptedException ie) { 
                Thread.currentThread().interrupt(); 
              }
            }
        } catch (IOException e) { 
          System.out.println("Client.getRanking: IOException T"+attempts+"!"); 
          throw e; 
        } catch (Exception e) { // Captura erros de parse ou outros
            System.out.println("Client.getRanking: Erro inesperado ao processar '" + response + "': " + e.getMessage());
        }
    }
  if (!rankingReceived) 
    System.out.println("Client.getRanking: Não recebido após "+attempts+" T.");
  return ranking; //
}


  public void joinLobby(String username, String password) throws IOException {
    sb.setLength(0); // Limpa antes de usar
    sb.append("join_lobby#").append(username).append(" ").append(password);
    send(sb.toString());

    String response = receive();
    System.out.println("Resposta do servidor ao join_lobby: " + response);

    if (response == null) {
      throw new IOException("Sem resposta do servidor ao join_lobby");
    }

    switch (response.trim()) {
        case "searching":
          break; // Sucesso, continua a esperar na thread de busca
        case "invalid_lobby_join":
          throw new IOException("Falha ao entrar no lobby (inválido)");
        default:
          throw new IOException("Resposta inesperada ao join_lobby: " + response);
    }
  }

  // ------------------------
  // --- Métodos de Jogo ----
  // ------------------------
  
  
  public void sendPosition(String username, float x, float y){
    // Envia com username, conforme esperado pelo clientInput do server.erl fornecido
    String message = "position#" + username + " " + x + " " + y;
    send(message);
  }

  public void sendCollision(String username) {
    String message = "collision#" + username;
    send(message);
  }

  public void sendFire(String username, float x, float y, float dx, float dy, String id){
    String message = "fire#" + username + " " + x + " " + y + " " + dx + " " + dy + " " + id;
    send(message);
  }
  
  public void sendMod(String username, String id, String type, float x, float y){ //long duration
    String message = "mod_spawn#" + username + " " + id + " " + type + " " + x + " " + y;
    send(message);
  }

  public void sendHit(String username, String projectileId){ 
    String message = "hit#" + username + " " + projectileId;
    send(message);
  }

  // Listener principal durante o jogo
  public void startServerListener(Data data) {
    if (serverListenerThread != null && serverListenerThread.isAlive()) {
      System.out.println("Listener já estava ativo. Interrompendo o antigo.");
      serverListenerThread.interrupt();
      try { serverListenerThread.join(100); } catch (InterruptedException e) {} // Espera brevemente
    }

    serverListenerThread = new Thread(() -> {
      System.out.println("Thread listener iniciada.");
      try {
        // Loop principal: lê e processa mensagens do servidor
        while (!Thread.currentThread().isInterrupted()) {
          String response = receive(); // Espera por uma mensagem

          if (response == null) { // Servidor fechou a conexão?
            System.out.println("Listener: null recebido. Conexão fechada?");
            throw new IOException("Conexão fechada pelo servidor.");
          }

          String[] parts = response.split("#");
          if (parts.length < 1) continue; // Ignora linhas vazias

          String command = parts[0];
          String payload = parts.length > 1 ? parts[1] : "";

          // Processa comandos conhecidos
          switch (command) {
            case "opponent_position":
              String[] posParts = payload.split(" ");
              if (posParts.length == 2) {
                try {
                  float oppX = Float.parseFloat(posParts[0]);
                  float oppY = Float.parseFloat(posParts[1]);
                  synchronized (data) { 
                    data.opponentX = oppX; 
                    data.opponentY = oppY; 
                  }
                } catch (NumberFormatException e) {
                    System.out.println("Listener: Erro ao parsear posição: " + payload);
                }
              }
              break;

            case "score_update":
              String[] scoreParts = payload.split(" ");
              if (scoreParts.length == 2) {
                try {
                  int score1 = Integer.parseInt(scoreParts[0]);
                  int score2 = Integer.parseInt(scoreParts[1]);
                  synchronized (data) {
                    if (data.username.compareTo(data.opponentName) < 0) { // Eu sou P1
                      data.myScore = score1; 
                      data.opponentScore = score2;
                    } else { // Eu sou P2
                      data.myScore = score2; 
                      data.opponentScore = score1;
                    }
                  }
                } catch (NumberFormatException e) {
                    System.out.println("Listener: Erro ao parsear scores: " + payload);
                }
              }
              break;

            case "projectile_spawn":
              String[] projParts = payload.split(" ");
              if (projParts.length == 6) {
                try {
                  String ownerID = projParts[0];
                  float px = Float.parseFloat(projParts[1]);
                  float py = Float.parseFloat(projParts[2]);
                  float dx = Float.parseFloat(projParts[3]);
                  float dy = Float.parseFloat(projParts[4]);
                  String pId = projParts[5];
                  Projectile p = new Projectile(ownerID, px, py, dx, dy, pId);
                  synchronized (data) {
                    data.projectiles.add(p);
                  }
                } catch (Exception e) {
                  System.out.println("Erro ao processar projectile_spawn: " + payload);
                }
              }
              break;
            
            case "remove_projectile":
              try{
                synchronized (data) {
                    data.projectiles.removeIf(p -> p.id.equals(payload));
                  }
              } catch (Exception e) {
                System.out.println("Erro ao processar remove_spawn: " + payload);
              }
              break;

            case "modifier_spawn":
              String[] modParts = payload.split(" ");
              if (modParts.length == 4) {
                try {
                  String modID = modParts[0];
                  String modType = modParts[1];
                  float modX = Float.parseFloat(modParts[2]);
                  float modY = Float.parseFloat(modParts[3]);
                  //long duration = Long.parseLong(modParts[4]);
                  Modifier mod = new Modifier(modID, modType, modX, modY);
                  synchronized (data) {
                    data.activeModifiers.add(mod);
                  }
                } catch (Exception e) {
                  System.out.println("Erro ao processar modifier_spawn: " + payload);
                }
              }
              break;

            case "modifier_collected":
              try{
                synchronized (data) {
                    data.activeModifiers.removeIf(m -> m.id.equals(payload));
                  }
              } catch (Exception e) {
                System.out.println("Erro ao processar modifier_collected: " + payload);
              }
              break;

            case "end_game":
              String[] finalScoreParts = payload.split(" ");
              if (finalScoreParts.length == 2) {
                try {
                  int finalScore1 = Integer.parseInt(finalScoreParts[0]);
                  int finalScore2 = Integer.parseInt(finalScoreParts[1]);
                  synchronized(data) {
                    if (data.username.compareTo(data.opponentName) < 0) {
                      data.finalMyScore = finalScore1;
                      data.finalOpponentScore = finalScore2;
                    } else {
                      data.finalMyScore = finalScore2; 
                      data.finalOpponentScore = finalScore1;
                    }

                    if(data.finalMyScore < data.finalOpponentScore){
                      data.losses += 1;
                      data.wins = 0;
                      if(data.losses == (data.level/2)){
                        data.level -= 1;
                        data.losses = 0;
                      }
                    } else if(data.finalMyScore > data.finalOpponentScore){
                      data.wins += 1;
                      data.losses = 0;
                      if(data.wins == data.level){
                        data.level += 1;
                        data.wins = 0;
                      }
                    }
                    System.out.println("Listener: Scores FINAIS: Eu " + data.finalMyScore + ", Adv " + data.finalOpponentScore);
                  }

                  // Notifica o Display para mudar de estado
                  if (displayApp != null) {
                    displayApp.handleEndGameFromServer();
                  } else {
                    System.out.println("Listener: Recebido end_game, mas displayApp é nulo!");
                  }
                  return; // Termina a thread listener

                } catch (NumberFormatException e) {
                    System.out.println("Listener: Erro ao parsear scores finais: " + payload);
                }
              }
              break; // Fim do case end_game
            default:
                break;
          } 
        } 
      } catch (IOException e) {
          // Erros de rede ou conexão fechada são tratados aqui
          if (!Thread.currentThread().isInterrupted()) {
              System.out.println("Listener ERRO IO: " + e.getMessage());
              // Notifica o Display sobre o erro de conexão
          } else {
              System.out.println("Listener: Thread interrompida intencionalmente.");
          }
      } catch (Exception e) { // Captura outros erros inesperados na thread
            System.out.println("Listener ERRO Inesperado: " + e.getMessage());
            e.printStackTrace();
      } finally {
        System.out.println("Thread listener terminada.");
      }
    });
    serverListenerThread.start(); // Inicia a thread
  } 

  public void sendModifierCollision(String username, String modifierID) {
    String msg = "modifier_collected#" + username + " " + modifierID;
    send(msg);
  }

  // -------------------------
  // --- Gestão da Conexão ---
  // -------------------------

  /** Fecha a conexão de forma correta, interrompendo a thread listener. */
  public void closeConnection() {
      System.out.println("Cliente: Fechando conexão...");
      // 1. Interrompe a thread listener se estiver ativa
      if (serverListenerThread != null && serverListenerThread.isAlive()) {
          serverListenerThread.interrupt();
          try {
              // Espera um pouco para a thread terminar
              serverListenerThread.join(500); // Espera até 500ms
          } catch (InterruptedException e) {
              Thread.currentThread().interrupt(); // Restaura o status de interrupção
              System.out.println("Cliente: Interrompido enquanto esperava listener terminar.");
          }
      }
      // 2. Fecha os streams e o socket
      try { 
        if (out != null) 
          out.close(); 
      } catch (Exception e) {}
      try { 
        if (in != null) 
          in.close(); 
      } catch (IOException e) {}
      try { 
        if (s != null && !s.isClosed()) 
          s.close(); 
      } catch (IOException e) { 
          System.out.println("Erro ao fechar socket: " + e.getMessage()); 
      }

      // Indicação de estado desconectado
      out = null;
      in = null;
      s = null;
      serverListenerThread = null; // Garante que a referência à thread antiga seja removida
      System.out.println("Cliente: Conexão fechada.");
  }

  // Método chamado pelo Display.exit() para garantir fecho de conexão
  public void stop() {
    closeConnection();
  }
}
