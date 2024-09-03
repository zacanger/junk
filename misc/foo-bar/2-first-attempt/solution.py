# this probably isn't very idiomatic.
# i'm not all that handy with python, but i'm even less familiar with java.

counter = 0
dist = {}
ways = {}
board = {}
valid_moves = [(1, 2), (2, 1), (-1, -2), (-2, -1), (1, -2), (-1, 2), (-2, 1), (2, -1)]

# generate a board
for row in range(8):
    for col in range(8):
        board[counter] = (row, col)
        counter += 1

# Int -> (Int, Int)
def get_node(n):
    return board.keys()[board.values().index(n)]

def bfs(source, dst):
    src = min(source, dst)
    dest = max(source, dst)
    queue = [src]
    dist[src] = 0
    ways[src] = 1

    while len(queue):
        curr = queue[0]
        queue.pop(0)
        if curr == dest:
            return dist[curr]

        for move in valid_moves:
            next_pos = curr[0] + move[0], curr[1] + move[1]
            if next_pos[0] > dest[0] or next_pos[1] > dest[1] or next_pos[0] < 1 or next_pos[1] < 1:
                # pass
                continue
            if next_pos in dist and dist[next_pos] == dist[curr] + 1:
                ways[next_pos] += ways[curr]
            if next_pos not in dist:
                dist[next_pos] = dist[curr] + 1
                ways[next_pos] = ways[curr]
                queue.append(next_pos)

def is_invalid(n):
    return n < 0 or n > 63

def answer(src, dest):
    if is_invalid(src) or is_invalid(dest):
        return None
    s = get_node(src)
    d = get_node(dest)
    return bfs(s, d)

print(board)
# print(answer(0, 27))

# for a in range(64):
    # print answer(a, 0)
# print(answer(27, 0))
# for a in board.values():
    # for b in board.values():
         # print(answer(a, b))
