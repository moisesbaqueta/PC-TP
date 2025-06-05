import processing.core.PApplet;
import java.util.*;
import java.io.*;
import javax.swing.SwingUtilities; // Import necessário para catch (Exception e) { message = "Erro inesperado logout: " + e.getMessage(); state = State.MENU; }
import java.util.ArrayList;

// -----------------------------

public class Display extends PApplet {

  Client client;
  Data data;

  String message = ""; // Mensagens de feedback
  String currentInput = ""; // Input de texto
  boolean typing = false; // Flag de escrita
  long startTime = 0; // Tempo de início do jogo (referência)
  boolean gameEnded = false; // Flag local de fim de jogo

  // Posição e física do avatar local
  float myX, myY;
  float avatarSize = 30;
  float myVX = 0, myVY = 0;
  float myAX = 0, myAY = 0;

  float maxSpeed = 5;
  float acceleration = 0.2;
  float friction = 0.05; // Atrito

  // Cooldown para envio de colisão
  long lastCollisionSendTime = 0;
  final long collisionCooldown = 500; // Meio segundo

  State state = State.MENU; // Estado inicial
  volatile boolean readyToStartGame = false; // Flag sinalizada pela thread de busca

  // Limites da área de jogo (calculados no setup)
  float minXLimit, maxXLimit, minYLimit, maxYLimit;

  //Projéteis
  long lastShotTime = 0;
  int shotCooldown = 1000;
  float projectileSpeed = 1;
  int projectileId = 0;

  // Mods
  long lastModifierSpawn = 0;
  int modifierInterval = 10000;
  int modId = 0;
  long modDuration = 5000;
  String[] mTypes = {"speed_up", "slow_down", "fast_shot", "slow_shot"};

  //-----------------------------------------------------------------------------
  // SETTINGS & SETUP
  //-----------------------------------------------------------------------------

  public void settings() {
    size(600, 600);
  }

  public void setup() {
    textSize(16);
    textAlign(LEFT, BASELINE); // Alinhamento de texto padrão
    frameRate(60); // Define uma taxa de frames

    data = new Data(); // Cria o objeto de dados
    connectInitialClient(); 
    calculateLimits();
    
  }
  
  // Tenta conectar o cliente inicial
  void connectInitialClient() {
      try {
          println("Conectando cliente...");
          // Fecha antigo se existir
          if (client != null) client.closeConnection();
          // Cria novo com referência ao Display
          client = new Client("localhost", 11000, data, this);
          message = "Conectado.";
      } catch (IOException e) {
          message = "Falha ao conectar: " + e.getMessage();
          println("ERRO Conexão: " + message);
          client = null; // Garante que é nulo se falhou
      }
  }

  // Calcula limites baseado no rect de drawGameScreen
  void calculateLimits() {
      float rectX = 20; float rectY = 60;
      float rectW = 560; float rectH = 460;
      minXLimit = rectX + avatarSize / 2;
      maxXLimit = rectX + rectW - avatarSize / 2;
      minYLimit = rectY + avatarSize / 2;
      maxYLimit = rectY + rectH - avatarSize / 2;
  }

  //-----------------------------------------------------------------------------
  // FUNÇÕES DRAW
  //-----------------------------------------------------------------------------

  public void draw() {
    try {
        background(240);
        fill(0); // Cor padrão

        typing = (state == State.CREATE_USERNAME || state == State.CREATE_PASSWORD ||
                  state == State.USERNAME || state == State.PASSWORD);
        
        // Verifica se é hora de iniciar o jogo
        if (readyToStartGame) {
            readyToStartGame = false; // Reseta o flag
            startGame(); // Chama da thread principal da UI
        }

        // Desenha o ecrã correspondente ao estado atual
        switch (state) {
          case MENU: drawMenu(); break;
          case CREATE_USERNAME: case CREATE_PASSWORD: drawCreateAccount(); break;
          case USERNAME: case PASSWORD: drawLogin(); break;
          case LOGIN_MENU: drawPostLogin(); break;
          case LOGOUT: handleLogout(); break;
          case REMOVE: handleDeleteAccount(); break;
          case RANKING_MENU: drawRankingMenu(); break;
          case JOIN: handleSearchGame(); break;
          case TEMPORARY_LOBBY: drawWaitingLobby(); break;
          case GAME: drawGameScreen(); break;
          case END_GAME: drawEndGame(); break;
          default:
            background(200); textAlign(CENTER, CENTER);
            text("Estado desconhecido: " + state, width / 2, height / 2);
            break;
        }

        // Mostra mensagens de feedback em baixo
        fill(50); textAlign(LEFT, BOTTOM);
        text("Mensagem: " + message, 20, height - 10);
        textAlign(LEFT, BASELINE); // Restaura alinhamento

     } catch (Exception e) {
          println("ERRO INESPERADO EM DRAW: " + e.getMessage());
          e.printStackTrace();
          message = "Erro inesperado: " + e.getMessage();
     }
  }

  void drawMenu() {
      background(255, 153, 153); textSize(32); fill(0); textAlign(CENTER);
      text("Duelo", width / 2, 60); textSize(16); textAlign(LEFT);
      drawButton("Login", width / 4, height / 3, State.USERNAME);
      drawButton("Registo", width / 4, height / 3 + 60, State.CREATE_USERNAME);
  }

  void drawCreateAccount() {
      background(153, 255, 204); textAlign(LEFT);
      text("Username: " + data.username, 20, 40);
      text("Password: " + data.password, 20, 80);
      drawButton("Voltar", width / 4, height - 80, State.MENU);
      
      if (state == State.CREATE_USERNAME) drawInputField("Escrever username", 20, 120);
      else if (state == State.CREATE_PASSWORD) drawInputField("Escrever password", 20, 160);
  }

  void drawLogin() {
      background(153, 204, 255); textAlign(LEFT);
      text("Username: " + data.username, 20, 40);
      text("Password: " + data.password, 20, 80);
      drawButton("Voltar", width / 4, height - 80, State.MENU);
      
      if (state == State.USERNAME) drawInputField("Escrever username", 20, 120);
      else if (state == State.PASSWORD) drawInputField("Escrever password", 20, 160);
  }

  void drawPostLogin() {
      background(204, 255, 153); textAlign(LEFT);
      text("Autenticado como: " + data.username, 20, 40);
      text("Nível: " + data.level, 20, 70);
      drawButton("Procurar jogo", 20, 120, State.JOIN);
      drawButton("Logout", 20, 180, State.LOGOUT);
      drawButton("Remover conta", 20, 240, State.REMOVE);
      drawButton("Tabela Classificativa", 20, 300, State.RANKING_MENU);
  }

  void drawRankingMenu() {
    background(200, 255, 255);
    textAlign(CENTER); textSize(18); fill(0);
    text("Top Jogadores:", width / 2, 40);
    textAlign(LEFT); textSize(14);

    // Lê a lista de strings "User Level Wins" diretamente de data.ranking
    synchronized(data) { // Protege acesso concorrente
      if (data.ranking != null && !data.ranking.isEmpty()) {
        int displayRank = 0;
        int previousLevel = -1;

        for (int i = 0; i < data.ranking.size(); i++) {
          String entry = data.ranking.get(i); // Ex: "user1 10 5"
          String[] parts = entry.split(" "); // Divide por espaço
          if (parts.length == 3) { // Espera User, Level, Wins
            String username = parts[0];
            int level = Integer.parseInt(parts[1]);
            int wins = Integer.parseInt(parts[2]); // Faz parse das Wins

            // Lógica para número do Rank (repete em empate de nível)
            if (level != previousLevel) {
              displayRank = i + 1;
            }

            // Exibe: "Rank. User - Nível X (Wins: Y)"
            text(displayRank + ". " + username + " - Nível " + level + " (Wins consecutivas " + wins + ")", width / 2 - 150, 80 + i * 25);
            previousLevel = level;
          } else {
              println("WARN: Ranking entry formato inválido: " + entry);
              text((i + 1) + ". [Erro Formato]", width / 2 - 150, 80 + i * 25);
          }
        }
      } else {
          textAlign(CENTER);
          if (message == null || message.isEmpty() || message.equals("Conectado.")) {
            text("Ranking vazio.", width/2, 100);
          }
          textAlign(LEFT);
      }
    }

    drawButton("Voltar", width / 2 - 90, height - 80, State.LOGIN_MENU);
    textAlign(LEFT, BASELINE);
  }
                  
  void drawWaitingLobby(){
      background(255, 255, 153); textAlign(CENTER, CENTER); fill(0); textSize(18);
      text("À procura de adversário...", width / 2, height / 2);
  }

  void drawGameScreen(){
    background(50, 50, 50); // Fundo escuro

    // Desenha área de jogo
    stroke(255); strokeWeight(4); noFill();
    rect(20, 60, 560, 460);

    // Título e Score
    fill(255); textAlign(CENTER, CENTER); textSize(20);
    text("Jogo a decorrer", width / 2, 30);
    textAlign(CENTER, BASELINE); textSize(18);
    synchronized (data) { // Protege leitura de scores
        text(data.myScore + "  |  " + data.opponentScore, width / 2, 55);
    }

    // Processa física, colisão e envia posição
    processMovementAndCollision();
    if (!gameEnded && frameCount % 5 == 0 && client != null) {
        client.sendPosition(data.username, myX, myY);
    }

    // Desenha avatares
    noStroke(); // Sem contorno para os avatares
    fill(255, 242, 0); // Meu avatar (amarelo)
    ellipse(myX, myY, avatarSize, avatarSize);
    fill(219, 0, 255); // Avatar oponente (roxo)
    synchronized (data) { // Protege leitura da posição do oponente
        ellipse(data.opponentX, data.opponentY, avatarSize, avatarSize);
    }

    fireProjectile();

    synchronized(data){
      for (int i = data.projectiles.size() - 1; i >= 0; i--) {
          Projectile p = data.projectiles.get(i);
          //
          if (p.checkCollision(myX, myY, avatarSize / 2) && p.ownerId.equals(data.opponentName)) {
              client.sendHit(data.username, p.id);
              //
          } else {
            p.update();
            fill(255, 255, 255);
            ellipse(p.x, p.y, p.radius * 2, p.radius * 2);
          }
      }
      data.cleanupProjectiles(minXLimit, minYLimit, maxXLimit, maxYLimit);
    }
    
    createMod();
    
    synchronized(data){
      // Desenhar e verificar colisão com modificadores
      for (int i = data.activeModifiers.size() - 1; i >= 0; i--) {
          Modifier m = data.activeModifiers.get(i);
          switch(m.type){
            case "speed_up": //menor tempo entre tiros
              fill(0, 162, 232); //azul         
              break;
            case "slow_down": //maior tempo entre tiros
              fill(237, 28, 36); //vermelho
              break;
            case "fast_shot": //tiro rápido
              fill(180, 255, 0); //verde
              break;
            case "slow_shot": //tiro lento
              fill(255, 180, 0); //laranja
              break;
            default: break;
          }

          if (m.checkCollision(myX, myY, avatarSize / 2)) {
              client.sendModifierCollision(data.username, m.id);
              applyModifierEffect(m.type);
              m.time = System.currentTimeMillis();
              m.active = true;
          } else if (!m.active) {
            ellipse(m.x, m.y, m.radius * 2, m.radius * 2);
          }

          if(m.active){
            reapplyModEffect(m);
          }
      }
      data.cleanupModifiers();
    }
  
  }

  void drawEndGame() {
      background(255, 204, 204); // Fundo claro

      fill(0); textAlign(CENTER, CENTER);
      textSize(24); text("Fim do jogo!", width / 2, 80);
  
      textSize(20);
      String myFinalScoreText = "Erro";
      String oppFinalScoreText = "Erro";
      String result = "Indefinido";
  
      // Lê scores e determina resultado de forma segura
      synchronized(data) {
          myFinalScoreText = String.valueOf(data.finalMyScore);
          oppFinalScoreText = String.valueOf(data.finalOpponentScore);
          if (data.finalMyScore == -1 || data.finalOpponentScore == -1) { result = "Oponente desconectou!"; }
          else if (data.finalMyScore > data.finalOpponentScore) { result = "Foste o vencedor!"; }
          else if (data.finalMyScore < data.finalOpponentScore) { result = "Foste derrotado!"; }
          else { result = "Empate!"; }
      }
      // Mostra scores e resultado
      text("Minha Pontuação Final: " + myFinalScoreText, width / 2, 150);
      text("Pontuação do Adversário: " + oppFinalScoreText, width / 2, 190);
      textSize(22); text(result, width / 2, 260);
  
      // Botão Voltar
      drawButton("Voltar ao Menu", width / 2 - 90, height - 100, State.LOGIN_MENU, true);
  
      // Restaura defaults de texto
      textAlign(LEFT, BASELINE); textSize(16);
  }
  
  // Desenha campo de input de texto
  void drawInputField(String label, int x, int y) {
      fill(255); stroke(0); rect(x, y, 300, 30);
      fill(0); textSize(14); textAlign(LEFT, CENTER);
      if (currentInput.isEmpty()) {
          fill(150); text(label, x + 10, y + 15);
      } else { // Mostra texto atual com cursor "|" se estiver escrevendo
          fill(0); text(currentInput + (typing ? "|" : ""), x + 10, y + 15);
      }
      textAlign(LEFT, BASELINE); textSize(16); // Restaura defaults
  }

  // Verifica se o mouse está sobre uma área retangular
  boolean over(int x, int y, int w, int h) {
    return mouseX >= x && mouseX <= x + w && mouseY >= y && mouseY <= y + h;
  }
  
  void drawButtonEndGame(String label, int x, int y, State nextState) {
    stroke(0); fill(200); rect(x, y, 180, 30, 5);
    fill(0); textAlign(CENTER, CENTER); textSize(16); text(label, x + 90, y + 15);
    textAlign(LEFT, BASELINE);

    if (mousePressed && over(x, y, 180, 30)) {
        mousePressed = false; 
        message = "Reconectando...";

        // --- LÓGICA DE RECONEXÃO NO CLIQUE ---
        try {
            // 1. Fecha conexão antiga (do jogo) se existir
            if (client != null) {
                println("Botão Fim Jogo: Fechando conexão do jogo...");
                client.closeConnection(); // Garante que listener e socket são fechados
            }

            // 2. Cria IMEDIATAMENTE uma nova instância/conexão
            System.out.println("Botão Fim Jogo: Recriando cliente para o menu...");
            client = new Client("localhost", 11000, data, this); // Cria nova conexão principal
            message = "Conectado."; // Atualiza mensagem

            // 3. Muda para o próximo estado APÓS reconectar com sucesso
            if (nextState != null) {
               state = nextState;
               println("Estado mudado para: " + nextState);
            }

        } catch (IOException e) {
            // Falha ao recriar cliente/reconectar
            System.out.println("ERRO ao recriar cliente no fim do jogo: " + e.getMessage());
            message = "Falha ao voltar ao menu: " + e.getMessage();
        } catch (Exception e) {
             println("ERRO inesperado ao recriar cliente: " + e.getMessage());
             message = "Erro inesperado ao voltar ao menu.";
             state = State.MENU;
        }
    }
  }

  void drawButton(String label, int x, int y, State nextState, boolean interruptListener) {
    stroke(0); fill(200); rect(x, y, 180, 30, 5);
    fill(0); textAlign(CENTER, CENTER); textSize(16); text(label, x + 90, y + 15);
    textAlign(LEFT, BASELINE);

    if (mousePressed && over(x, y, 180, 30)) {
        mousePressed = false;

        // --- Lógica do InterruptListener (APENAS interrompe thread) ---
        if (interruptListener && client != null && client.serverListenerThread != null && client.serverListenerThread.isAlive()) {
            println("Botão: Interrompendo listener thread (se ativa)...");
            client.serverListenerThread.interrupt();
        }
        
        if (state == State.END_GAME && nextState == State.LOGIN_MENU) {
        println("Display: Saindo do END_GAME, limpando dados da partida.");
        synchronized(data) {
            data.cleanupAfterGame();
        }
        // Reseta também variáveis locais do Display relacionadas ao jogo, se necessário
        this.shotCooldown = 1000; // Valor padrão
        this.projectileSpeed = 1.0f; // Valor padrão
        // etc., para outros modificadores de estado do jogo no Display
    }

    message = ""; // Limpar a mensagem

        // Limpa dados se necessário
        if (nextState == State.MENU || nextState == State.USERNAME || nextState == State.CREATE_USERNAME) {
            synchronized(data){ 
              data.username=""; 
              data.password=""; 
            } 
            currentInput=""; 
            typing=false;
        }
        
         // Muda estado para outros botões
        if (nextState != null) {
           this.state = nextState;
           typing = (nextState==State.USERNAME||nextState==State.PASSWORD||nextState==State.CREATE_USERNAME||nextState==State.CREATE_PASSWORD);
           if(typing) 
             currentInput="";
           // Chama fetchRanking ao ENTRAR no estado
           if (state == State.RANKING_MENU) {
             fetchRanking();
           }
        }

        // Chama ações correspondentes
        if (nextState == State.RANKING_MENU) { fetchRanking(); return; }
        if (nextState == State.LOGOUT) { handleLogout(); return; }
        if (nextState == State.REMOVE) { handleDeleteAccount(); return; }
        if (nextState == State.JOIN) { handleSearchGame(); return; }
    }
  }

  void drawButton(String label, int x, int y, State nextState) {
      drawButton(label, x, y, nextState, false); // Chama a versão principal com interrupt=false
  }
  
  //-----------------------------------------------------------------------------
  // LÓGICA DE MOVIMENTO E COLISÃO
  //-----------------------------------------------------------------------------

  void processMovementAndCollision() {
    // 1. Calcula aceleração baseada nas teclas pressionadas
    myAX = 0; myAY = 0;
    if (keyPressed) {
      if (key == 'w' || key == 'W') myAY -= acceleration;
      if (key == 's' || key == 'S') myAY += acceleration;
      if (key == 'a' || key == 'A') myAX -= acceleration;
      if (key == 'd' || key == 'D') myAX += acceleration; // Direita
    }

    // 2. Aplica aceleração e atrito à velocidade
    myVX += myAX; myVY += myAY;
    if (myAX == 0) myVX *= (1 - friction); // Atrito X
    if (myAY == 0) myVY *= (1 - friction); // Atrito Y

    // 3. Limita a velocidade máxima
    myVX = constrain(myVX, -maxSpeed, maxSpeed);
    myVY = constrain(myVY, -maxSpeed, maxSpeed);

    // 4. Calcula a próxima posição teórica
    float nextX = myX + myVX;
    float nextY = myY + myVY;

    // 5. Verifica se a próxima posição está fora dos limites definidos
    boolean collided = false;
    if (nextX < minXLimit || nextX > maxXLimit || nextY < minYLimit || nextY > maxYLimit) {
        collided = true;
    }

    // 6. Se colidiu:
    if (collided) {
        long now = millis();
        // Envia mensagem de colisão para o servidor (apenas se cooldown passou)
        if (now - lastCollisionSendTime > collisionCooldown) {
            if (client != null && data.username != null) { // Verifica se pode enviar
               client.sendCollision(data.username);
               lastCollisionSendTime = now; // Atualiza tempo do último envio
            } else {
               println("Colisão detectada mas não pode enviar (cliente/user nulo).");
            }
        }
        // SEMPRE reseta a posição LOCAL para a inicial ao colidir
        synchronized(data){
           myX = data.startX;
           myY = data.startY;
        }
        myVX = 0; myVY = 0; // Para o movimento
    } else {
        // 7. Se não colidiu, atualiza a posição
        myX = nextX;
        myY = nextY;
    }
  }

  void fireProjectile() {
    if (millis() - lastShotTime > shotCooldown) {
        float dx = mouseX - myX;
        float dy = mouseY - myY;
        float mag = sqrt(dx * dx + dy * dy);
        float speed = 10 * projectileSpeed;
        dx = dx / mag * speed;
        dy = dy / mag * speed;
        String id = data.username + projectileId;
        projectileId += 1;
        synchronized(data){
          data.projectiles.add(new Projectile(data.username, myX, myY, dx, dy, id));
        }
        lastShotTime = millis();
        client.sendFire(data.username, myX, myY, dx, dy, id);
    }
  }

  void createMod(){ //rect(20, 60, 560, 460);
    if (millis() - lastModifierSpawn > modifierInterval) {
        float mx = random(25, 575);
        float my = random(65, 515);
        int index = (int) random(0, 4);
        String type = mTypes[index];
        String id = data.username + modId;
        modId += 1;
        synchronized (data){
          data.activeModifiers.add(new Modifier(id, type, mx, my));
        }
        lastModifierSpawn = millis();
        client.sendMod(data.username, id, type, mx, my);
    }
  }

  void applyModifierEffect(String type) {
    switch(type) {
        case "speed_up":
          shotCooldown = max(500, shotCooldown - 100);
          break;
        case "slow_down": 
          shotCooldown = min(2000, shotCooldown + 200);
          break;
        case "fast_shot": 
          projectileSpeed = min(1.5, projectileSpeed + 0.1);
          break;
        case "slow_shot": 
          projectileSpeed = max(0.5, projectileSpeed - 0.1);
          break;
        default:
          break;
    }
  }

  void reapplyModEffect(Modifier m) {
    long t = System.currentTimeMillis() - m.time;
    if (t >= 1000) {
      m.duration -= t;
      switch(m.type) {
          case "speed_up":
            shotCooldown = min(1000, shotCooldown + 100);
            break;
          case "slow_down": 
            shotCooldown = max(1000, shotCooldown - 200);
            break;
          case "fast_shot": 
            projectileSpeed = max(1, projectileSpeed - 0.1);
            break;
          case "slow_shot": 
            projectileSpeed = min(1, projectileSpeed + 0.1);
            break;
          default:
            break;
      }
    }
    m.time = System.currentTimeMillis();
    if(m.duration <= 0){
      data.activeModifiers.removeIf(mod -> mod.id.equals(m.id));
    }
  }

  //-----------------------------------------------------------------------------
  // TRATAMENTO DE INPUT (TECLADO) E AÇÕES (LOGIN, LOGOUT, ETC.)
  //-----------------------------------------------------------------------------

  public void keyPressed() {
    
    //Ainda não funcional
    if (key == ESC) { // Permite fechar com ESC
        key = 0; // Consome a tecla ESC
        exit();
        return;
    }

    if (!typing) return; // Ignora outras teclas se não estiver num campo de texto

    if (keyCode == BACKSPACE) {
        if (currentInput.length() > 0) {
            currentInput = currentInput.substring(0, currentInput.length() - 1);
        }
    } else if (keyCode == ENTER || keyCode == RETURN) {
        handleEnterPress(); // Processa o Enter
    } else if (key != CODED && key >= ' ' && key <= '~') { // Aceita caracteres imprimíveis
        currentInput += key;
    }
  }

  // Lida com a tecla Enter nos campos de texto
  void handleEnterPress() {
      message = ""; // Limpa mensagens antigas
      String userOrPass = currentInput; // Guarda valor antes de limpar
      currentInput = ""; // Limpa input visualmente

      try {
          switch (state) {
              case CREATE_USERNAME:
                  if (userOrPass.isEmpty()) { 
                    message = "Username não pode ser vazio."; currentInput = userOrPass; break; // Re-exibe input
                  }
                  synchronized(data){
                    data.username = userOrPass;
                  }
                  state = State.CREATE_PASSWORD; // Avança para password
                  break;
              case CREATE_PASSWORD:
                  if (userOrPass.isEmpty()) { 
                    message = "Password não pode ser vazia."; currentInput = userOrPass; break; 
                  }
                  synchronized(data){
                    data.password = userOrPass;
                  }
                  if(!ensureConnection()){
                    currentInput=userOrPass; break;
                  } // Tenta conectar ANTES
                  client.createAccount(data.username, data.password); // Tenta criar conta
                  message = "Conta criada com sucesso!";
                  synchronized(data){
                    data.username = "";
                    data.password = "";
                  }
                  state = State.MENU; // Volta ao menu
                  typing = false;
                  break;
              case USERNAME:
                  if (userOrPass.isEmpty()) { 
                    message = "Username não pode ser vazio."; currentInput = userOrPass; break; 
                  }
                  synchronized(data){
                    data.username = userOrPass;
                  }
                  state = State.PASSWORD; // Avança para password
                  break;
              case PASSWORD:
                  if (userOrPass.isEmpty()) { 
                    message = "Password não pode ser vazia."; currentInput = userOrPass; break; 
                  }
                  synchronized(data){
                    data.password = userOrPass;
                  }
                  if(!ensureConnection()){
                    currentInput=userOrPass; break;
                  } // Tenta conectar ANTES
                  client.login(data.username, data.password); // Tenta fazer login
                  message = "Login efetuado com sucesso!";
                  state = State.LOGIN_MENU; // Vai para menu pós-login
                  typing = false;
                  break;
              default: // Enter pressionado noutro estado
                  typing = false;
                  break;
          }
      } catch (IOException e) { // Erros de rede
          message = "Erro de rede: " + e.getMessage();
          typing = false; // Para de escrever em caso de erro
          state = state.MENU;
          currentInput = "";
      } catch (UserAlreadyAuth e) {
          message = e.getMessage();
          currentInput = userOrPass;
      } catch (InvalidPassword | UserExists | InvalidAccount e) { // Erros de lógica de conta
          message = "Erro: " + e.getMessage();
          currentInput = userOrPass; // Restaura input para correção
      } catch (Exception e) { // Outros erros
          message = "Erro inesperado: " + e.getMessage();
          e.printStackTrace();
          state = State.MENU; // Volta ao menu por segurança
          typing = false;
      }
  }

  // Lida com a ação de Logout
  void handleLogout() {
    println("handleLogout chamado...");
    String logoutMessage = "Logout local efetuado.";
    boolean connectionError = false;

    try {
        // Tenta enviar logout para o servidor APENAS se conectado
        if (client != null && client.isConnected()) {
           println("Tentando logout no servidor...");
           // Garantir que data.username e data.password estão corretos neste ponto
           if (data.username != null && !data.username.isEmpty() && data.password != null) {
               client.logout(data.username, data.password); // Usa os dados da sessão
               logoutMessage = "Logout efetuado (servidor notificado)";
               println("Logout enviado para o servidor.");
           } else {
                logoutMessage = "Logout local";
                println("Tentando logout com dados locais inválidos.");
           }
        } else {
           logoutMessage = "Logout local (sem conexão ativa)";
           println("WARN: Logout sem cliente conectado.");
        }
    } catch (IOException e) {
        // Erro de rede durante a tentativa de logout
        println("Logout IOErr: " + e.getMessage());
        logoutMessage = "Logout local (erro de rede)";
        connectionError = true; // Marca que houve erro de rede
    } catch (InvalidPassword | InvalidAccount e) {
        println("Logout Logic Err: " + e.getMessage());
        logoutMessage = "Erro no logout: " + e.getMessage();
        message = logoutMessage; // Define a mensagem de erro
        state = State.LOGIN_MENU;  // Permanece no menu logado
        return; // Sai da função aqui, não limpa dados nem fecha conexão
    } catch (Exception e) {
        // Outro erro inesperado
        println("Logout Err Geral: " + e.getMessage());
        logoutMessage = "Erro inesperado no logout.";
        connectionError = true; // Assume erro de conexão
    }

    message = logoutMessage; // Define a mensagem final

    // Limpa dados locais SEMPRE
    println("Limpando dados locais...");
    synchronized(data){ data.username = ""; data.password = ""; data.level = 0; }

    println("Mudando para State.MENU");
    state = State.MENU;

    // Fecha a conexão SEMPRE ao fazer logout (mesmo se houve erro de rede antes)
    if (client != null) {
        println("Fechando conexão no final do logout...");
        client.closeConnection();
        client = null; // Define como nulo para forçar reconexão se necessário
    }
  }
          
  // Lida com a ação de Remover Conta
  void handleDeleteAccount() {
      println("handleDeleteAccount chamado...");
      if (!connectIfNeeded()) { 
        message = "Não conectado."; return; 
      } // Conecta primeiro
      try {
          client.closeAccount(data.username, data.password); // Usa os dados atuais
          message = "Conta removida";
          synchronized(data){ 
            data.username=""; data.password=""; data.level=0; 
          } 
          state = State.MENU;
          if (client != null) 
            client.closeConnection(); 
          client=null;
      } catch (IOException e) { 
        message = "Erro rede remover."; 
        println("Delete IOErr:"+e.getMessage());
      } // Fica logado
        catch (InvalidPassword | InvalidAccount e) { 
          message = "Erro remover: "+e.getMessage();
      } // Fica logado
        catch (Exception e) { 
          message = "Erro inesperado remover."; 
          println("Delete Err:"+e.getMessage()); 
          state = State.MENU;
      }
  }

  // Busca o ranking (inicia a thread)
  void fetchRanking() {
    println("fetchRanking chamado...");
    message = "A carregar ranking...";
    if (!connectIfNeeded()) { 
      message = "Não conectado."; 
        if (state == State.RANKING_MENU) { // Evita mudar se já saiu do ranking
          state = State.LOGIN_MENU;
        }
      return; 
    }
    
    new Thread(() -> {
      List<String> receivedRanking = null; // Lista de "User Level Wins"
      String threadMessage = "";
      try {
        println("fetchRanking Thread: Chamando client.getRanking()...");
        receivedRanking = client.getRanking(); // Recebe List<String>
        if (receivedRanking != null) {
          println("fetchRanking Thread: Ranking recebido, " + receivedRanking.size() + " entradas.");
        } else {
            println("fetchRanking Thread: Ranking recebido era null.");
        }
        if (receivedRanking != null) {
          // --- Guarda a List<String> em data.ranking ---
           synchronized(data) {
             data.ranking = receivedRanking;
           }
           
           threadMessage = ""; // Sucesso
        } else { 
          threadMessage = "Erro: Resposta nula."; 
        }
     } catch (IOException e) { 
       threadMessage = "Erro ao carregar ranking."; 
       println("Ranking IOErr:"+e.getMessage());
     } catch (Exception e) { 
       threadMessage = "Erro ao carregar ranking."; 
       println("Ranking Err:"+e.getMessage()); 
       e.printStackTrace();
     } finally {
         final String finalMessage = threadMessage;
         invokeLater(() -> { 
           message = finalMessage; 
         });
     }
   }).start();
  }

  //-----------------------------------------------------------------------------
  // LÓGICA DE BUSCA E INÍCIO DE JOGO (THREADS)
  //-----------------------------------------------------------------------------

  // Chamado ao clicar no botão "Procurar jogo"
  void handleSearchGame(){
       println("handleSearchGame chamado...");
      if (state != State.TEMPORARY_LOBBY && state != State.GAME) {
          if (!connectIfNeeded()) { 
            message = "Não conectado."; return; 
          } // Garante conexão
          message = "A procurar jogo...";
          state = State.TEMPORARY_LOBBY;
          readyToStartGame = false;
          new Thread(this::searchGameTaskSimplified).start(); // Inicia thread
      } else { 
        println("Busca já em progresso"); 
      }
  }

  // Tarefa de busca simplificada (assume conexão existe)
  void searchGameTaskSimplified() {
     // Assume que connectIfNeeded foi chamado antes, usa this.client
     try {
          println("SearchTaskSimplified: Enviando join_lobby...");
          client.joinLobby(data.username, data.password);
          println("SearchTaskSimplified: Aguardando info...");
          boolean opponentFound=false; 
          boolean gameCanStart=false;
          while(!gameCanStart && !Thread.currentThread().isInterrupted()){
               String msg = client.receive(); 
               if(msg==null) 
                 throw new IOException("Conexão fechada.");
               if(msg.startsWith("opponent#")){
                 synchronized(data){
                   data.opponentName=msg.split("#")[1];
               } 
               opponentFound=true; 
               println("Opponent:"+data.opponentName);}
               else if(msg.equals("start_game")){
                 if(opponentFound){
                   gameCanStart=true; 
                   println("Start game OK!");
                 }else{
                   println("Start antes de opponent?");
                 }
               }
          }
          if(gameCanStart){
            println("Pronto! Sinalizando UI."); 
            readyToStartGame=true;
          }
          else{
            println("Busca terminada sem iniciar."); 
            if(!gameCanStart) 
              invokeLater(()->{
                message="Falha ao encontrar par."; 
                state=State.LOGIN_MENU;
              });
          } // Volta se não começou
     } catch(IOException e){
       println("SearchTask ERRO IO:"+e.getMessage()); 
       invokeLater(()->{
         message="Erro procurar: "+e.getMessage(); 
         state=State.LOGIN_MENU;
       });
     }
     catch(Exception e){
       println("SearchTask ERRO:"+e.getMessage());
       e.printStackTrace(); 
       invokeLater(()->{
         message="Erro busca."; 
         state=State.LOGIN_MENU;
       });
     }
  }
  
  //------------------------------------------------- 
  // GESTÃO DE CONEXÃO
  //-------------------------------------------------
   
  boolean connectIfNeeded() {
      if (client != null && client.isConnected()) {
          println("Conexão já ativa.");
          return true; // Já conectado
      }
      // Se não estava conectado, tenta (re)conectar
      println("Tentando estabelecer conexão...");
      return reconnectClient();
  }
  
  boolean reconnectClient() {
    message = "Conectando..."; println("Tentando reconectar...");
    try {
        if (client != null) { 
          client.closeConnection(); 
        } // Fecha antiga
        client = new Client("localhost", 11000, data, this); // Tenta nova
        message = "Conectado."; 
        println("Reconexão OK.");
        return true;
    } catch (IOException e) {
        println("ERRO ao reconectar: " + e.getMessage());
        message = "Falha conexão: " + e.getMessage();
        client = null;
        return false;
    }
  }

  boolean ensureConnection() {
    if (client != null && client.isConnected()) {
        return true;
    } else {
        println("Conexão necessária. Tentando reconectar...");
        return reconnectClient();
    }
  }

  //-----------------------------------------------------------------------------
  // INÍCIO JOGO / FIM JOGO
  //-----------------------------------------------------------------------------

  // Chamado por 'draw()' quando 'readyToStartGame' é true
  void startGame(){
      println("Executando startGame()...");
      // 1. Reseta estado do jogo e scores
      synchronized(data) {
          data.myScore = 0; 
          data.opponentScore = 0;
          data.finalMyScore = 0; 
          data.finalOpponentScore = 0;
          // Define posições iniciais baseado na ordem alfabética
          if (data.username.compareTo(data.opponentName) < 0) { // Eu sou P1
            data.startX = 100; 
            data.startY = height/2; 
            data.opponentX = width-100; 
            data.opponentY = height/2;
          } else { // Eu sou P2
            data.startX = width-100; 
            data.startY = height/2; 
            data.opponentX = 100; 
            data.opponentY = height/2;
          }
          myX = data.startX; 
          myY = data.startY; // Define posição atual
          println("Posição inicial local: " + myX + ", " + myY);
      }
      startTime = millis(); // Guarda tempo inicial
      gameEnded = false; // Marca jogo como a decorrer
      lastCollisionSendTime = 0; // Reseta cooldown de colisão

      // 2. Inicia o listener principal para o cliente ATUAL
      if (client != null) {
           client.startServerListener(data); // Inicia a thread que ouve o servidor
           println("Listener principal do servidor iniciado para o cliente atual.");
      } else {
          System.out.println("Tentando iniciar jogo mas 'client' é nulo!");
          return; // Não continua se não há cliente
      }

      // 3. Muda para o estado de jogo
      state = State.GAME;
      message = "Jogo iniciado contra " + data.opponentName + "!";
      println("Estado mudado para GAME.");
  }

  // Chamado pelo Client listener quando recebe "end_game#"
  public void handleEndGameFromServer() {
     println("Display: handleEndGameFromServer chamado!");
     // Garante execução na thread
     invokeLater(() -> {
        // Só muda de estado se ainda estiver em jogo
        if (state == State.GAME) {
            gameEnded = true; // Marca localmente
            message = "O jogo terminou!";
            state = State.END_GAME; // Muda para o ecrã final
            println("Estado mudado para END_GAME.");
        } else {
             println("handleEndGameFromServer chamado mas estado atual é " + state);
        }
     });
  }

  // Helper para executar código na thread
  void invokeLater(Runnable runnable) {
    SwingUtilities.invokeLater(runnable);
  }

  //-----------------------------------------------------------------------------
  // CICLO DE VIDA DA APLICAÇÃO (exit, main)
  //-----------------------------------------------------------------------------

  public void exit() {
    // Chamado quando a janela Processing é fechada
    println("Display: Aplicação a fechar...");
    // Garante que a conexão do cliente é fechada de forma limpa
    if (client != null) {
      client.stop();
    }
    super.exit(); // Chama o método exit da superclasse
  }

  // Ponto de entrada principal
  public static void main(String[] args) {
    PApplet.main("Display"); // Inicia o sketch Processing
  }
}
