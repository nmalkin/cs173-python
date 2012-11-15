def any(l):
    def anyrec(l, i):
        try:
            l.__attr__
        except Exception:
            raise TypeError

        if l.__len__() == i:
            return False
        elif l[i]:
            return True
        else:
            return anyrec(l, i+1)
    return anyrec(l, 0)


