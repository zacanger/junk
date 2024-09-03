def memoize(fn):
    cache = dict()

    def memoized_fn(*args):
        if args in cache:
            return cache[args]
        res = fn(*args)
        cache[args] = res
        return res

    return memoized_fn
