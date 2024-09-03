# thanks, internet
def convert(a,b):
    add = a % b
    if a <= 1: return str(a)
    else: return str(convert(a // b, b)) + str(add)

def task_ids(n,b):
    k = len(str(n))
    x = int(''.join(sorted(str(n), reverse = True)), b)
    y = int(''.join(sorted(str(n))), b)
    z = x - y
    z_string = convert(z, b)[-k:]
    if len(z_string) < k: z_string.zfill(k)
    return z_string


def floyd(f, x0, b):
    tortoise = f(x0, b)
    hare = f(f(x0, b),b)
    while tortoise != hare:
        tortoise = f(tortoise, b)
        hare = f(f(hare, b), b)

    mu = 0
    tortoise = x0
    while tortoise != hare:
        tortoise = f(tortoise,b)
        hare = f(hare,b)
        mu += 1

    lam = 1
    hare = f(tortoise, b)
    while tortoise != hare:
        hare = f(hare, b)
        lam += 1

    return lam, mu

def answer(n, b):
    x0 = task_ids(n, b)
    return floyd(task_ids, x0, b)[0]
