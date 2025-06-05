public class Projectile {
    float x, y;
    float vx, vy;
    String ownerId; // ID do jogador que disparou
    
    final float radius = 5;
    //boolean active = true;
    float projectileSpeed = 1;

    String id;

    public Projectile(String ownerID, float x, float y, float dx, float dy, String id) {
        this.ownerId = ownerID;
        this.x = x;
        this.y = y;
        this.vx = dx;
        this.vy = dy;
        this.id = id;
    }

    public void update() {
        x += vx;
        y += vy;
        // Verifica se saiu da arena
        //if (isOutOfBounds(0, 0, 600, 800)) active = false;
    }

    public boolean isOutOfBounds(float minX, float minY, float maxX, float maxY) {
        return x < minX || x > maxX || y < minY || y > maxY;
    }

    public boolean checkCollision(float px, float py, float playerRadius) {
        float dist = dist(px, py, x + vx, y + vy);
        return dist <= (playerRadius + radius);
    }

    float dist(float px, float py, float x, float y) {
        return (float) Math.sqrt((px - x) * (px - x) + (py - y) * (py - y));
    }
}
