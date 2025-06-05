public class Modifier {
    final float radius = 10;
    boolean active = false;

    String id;
    String type;        
    float x, y;
    long duration;       // em ms
    long spawnTime;      // marca o tempo de criação
    long time;      // marca o tempo de colisão
    
    public Modifier(String id, String type, float x, float y) { //long duration
        this.id = id;
        this.type = type;
        this.x = x;
        this.y = y;
        this.duration = 5000;
        this.spawnTime = System.currentTimeMillis();
    }   
    
    public boolean isExpired() {
        return System.currentTimeMillis() - spawnTime >= duration;
    }

    public boolean checkCollision(float px, float py, float playerRadius) {
        float dist = dist(px, py, x, y);
        return dist < (radius + playerRadius);
    }

    float dist(float px, float py, float x, float y) {
        return (float) Math.sqrt((px - x) * (px - x) + (py - y) * (py - y));
    }
}
