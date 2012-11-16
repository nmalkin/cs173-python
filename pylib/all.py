def all(l):
    def allrec(l, i):
        try:
            l.__attr__
        except Exception:
            raise TypeError("Not an iterable type")
        if l.__len__() == i:
            return True
        elif l[i]:
            return allrec(l, i+1)
        else:
            return False
    return allrec(l, 0)


