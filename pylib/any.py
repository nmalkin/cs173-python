def any(l):
    def anyrec(l, i):
        try:
            l.__attr__
        except Exception:
            raise TypeError("Not an iterable type")

        if l.__len__() == i:
            return False
        elif l[i]:
            return True
        else:
            return anyrec(l, i+1)
    return anyrec(l, 0)


