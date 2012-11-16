def ___assertRaises(exc, fun, *args):
    try:
        fun(*args)
    except exc:
        pass
    else:
        print("Assert failure!")

def f(a):
    raise RuntimeError("asdf")
    #print("lol")

___assertRaises(TypeError, f, 1)

