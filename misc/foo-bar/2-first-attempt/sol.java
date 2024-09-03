import java.util.*;

public class Answer {
  private Integer[][] board = new Integer[8][8];
  private int row[] = { 2, 2, -2, -2, 1, 1, -1, -1 };
  private int col[] = { -1, 1, 1, -1, 2, -2, 2, -2 };
  private int N = 8

  Answer() {
    int count = 0;
    for (int i = 0; i < N; i++) {
      for (int j = 0; j < N; j++) {
        board[i][j] = count;
        count++;
      }
    }
  }

  private boolean isValid (int x, int y) {
    return !(x < 0 || y < 0 || x >= N || y >= N);
  }

  static class Node {
    int x;
    int y;
    int dist;

    boolean isEqual (Node o) {
      return x == o.x && y == o.y;
    }
  }

  int BFS (Node src, Node dest) {
    Map<Node,Boolean> visited = new HashMap<>();
    Queue<Node> queue = new LinkedList<>();

    queue.add(src);

    while (!queue.isEmpty()){
      Node node = queue.remove();

      int x = node.x;
      int y = node.y;
      int dist = node.dist;

      if (node.isEqual(dest)) return dist;

      if (visited.get(node) == null) visited.put(node, false);

      if (!visited.get(node)) {
        visited.put(node, true);

        for (int i = 0; i < N; i++) {
          int x1 = x + row[i];
          int y1 = y + col[i];
          if (isValid(x1, y1)) {
            Node n = new Node();
            n.x = x1;
            n.y = y1;
            n.dist = dist + 1;
            queue.add(n);
          }
        }
      }
    }
    return -1;
  }

  public Integer[] getPos(int src,int dest){
    Integer[] coordinates = new Integer[4];
    if (src >= 0 && src <= 63 && dest >= 0 && dest <= 63) {
      for (int i = 0; i < N; i++) {
        for (int j = 0; j < N; j++) {
          if (board[i][j] == src) {
            coordinates[0] = j;
            coordinates[1] = i;
          }
          if (board[i][j] == dest) {
            coordinates[2] = j;
            coordinates[3] = i;
          }
        }
      }
    } else {
      coordinates[0] = -1;
    }

    return coordinates;
  }

  public static int answer (int src, int dest) {
    Answer answer = new Answer();
    Integer[] coordinates = answer.getPos(src,dest);
    if (coordinates[0] == -1) return -1;
    Answer.Node source = new Node();
    Answer.Node destination = new Node();
    source.x = coordinates[0];
    source.y = coordinates[1];
    destination.x = coordinates[2];
    destination.y = coordinates[3];
    return answer.BFS(source,destination);
  }
}
