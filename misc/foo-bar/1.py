def answer(data, n):
    for a in data:
        if data.count(a) > n:
            data = filter(lambda x: x != a, data)
    return data
