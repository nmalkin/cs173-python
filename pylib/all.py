def all(l):
    def allrec(l, i):
        try:
            l.__attr__
        except Exception:
            raise TypeError
        if l.__len__() == i:
            return True
        elif l[i]:
            return anyrec(l, i+1)
        else:
            return False
    return anyrec(l, 0)

