def answer(l, t):
    curr = 0
    start = 0
    length = len(l)
    for i in range(length):
        curr = sum(l[start:i + 1])
        while curr > t:
            curr = curr - l[start]
            start +=1
        if curr == t: return [start, i]
    return [-1,-1]
