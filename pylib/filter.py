def filter(f, l):
    new_l = []
    def rec_filter(f, l, i):
        if i == len(l):
            return

        if f:
            new_l += f(l[i])
        else:
            new_l += l[i]
        i += 1
    rec_filter(f, l, 0)

    return new_l


