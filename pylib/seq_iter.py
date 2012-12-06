class SeqIter:
    def __init__(self,l):
        self.l = l
        self.i = 0
        return self

    def __len__(self):
        return len(self.l)

    def __list__(self):
        l = []
        while True:
            try:
                l.append(self.__next__())
            except StopIteration:
                break
        return l



    def __iter__(self):
        return self

    def __next__(self):
        has_length = True
        ret = None
        try:
            len(self.l)
        except AttributeError:
            has_length = False

        try:
            if has_length and self.i >= len(self.l):
                raise StopIteration("Stop")
            ret = self.l[self.i]
        except IndexError:
            raise StopIteration("Stop")
        except StopIteration:
            raise StopIteration("Stop")

        self.i += 1
        return ret

def iter(l, *args):
    if len(args) == 1:
        stopwhen = args[0]
        return FuncIter(l, stopwhen)
    else:
        return l.__iter__()

class FuncIter:
    def __init__(self, func, stopwhen):
        self.func = func
        self.stopwhen = stopwhen
        self.stopped = False
        return self

    def __list__(self):
        l = []
        while not self.stopped:
            try:
                l.append(self.__next__())
            except StopIteration:
                break
        return l

    def __next__(self):
        f = self.func
        v = f()

        if v == self.stopwhen:
            self.stopped = True
            raise StopIteration("Stop")
        else:
            return v



