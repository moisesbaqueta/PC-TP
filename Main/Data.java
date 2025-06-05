import java.util.List;
import java.util.ArrayList;
import java.util.Collections;

//-----------------------------

public class Data {
    public String username = "";
    public String password = "";
    public int level = 0;
    public int wins = 0; public int losses = 0;
    public boolean inLobby = false;

    //Informação de jogo
    public String opponentName = "";
    public float opponentX = 0, opponentY = 0; //Posição inicial
    public float startX = 0, startY = 0;       // Posição inicial do jogador local
    public int myScore = 0;                    // Score durante o jogo
    public int opponentScore = 0;              // Score durante o jogo
    public int finalMyScore = 0;               // Score no fim do jogo
    public int finalOpponentScore = 0;         // Score no fim do jogo

    //
    public List<Modifier> activeModifiers = Collections.synchronizedList(new ArrayList<>());
    public List<Projectile> projectiles = Collections.synchronizedList(new ArrayList<>());
    //
    
    public List<String> ranking = new ArrayList<>();

    public Data() {}

    //Remove projéteis fora da tela
    public synchronized void cleanupProjectiles(float minX, float minY, float maxX, float maxY) {
        projectiles.removeIf(p -> p.isOutOfBounds(minX, minY, maxX, maxY));
    }
    
    //Remove modificadores muito antigos
    public synchronized void cleanupModifiers() {
        activeModifiers.removeIf(m -> m.isExpired() && !m.active);
    }

    // "Renova" os dados para a próxima partida
    public synchronized void cleanupAfterGame() {
        opponentName = "";
        opponentX = 0; opponentY = 0;
        startX = 0; startY = 0;
        myScore = 0; opponentScore = 0;
        finalMyScore = 0; finalOpponentScore = 0;
        activeModifiers = Collections.synchronizedList(new ArrayList<>());
        projectiles = Collections.synchronizedList(new ArrayList<>());
    }
}

 
