def ___assertRaises(exc, fun, *args):
    try:
        fun(*args)
    except exc:
        pass
    except:
      	print("Assert failure !")
    else:
        print("Assert failure!")

def f(a):
    raise RuntimeError("asdf")
    #print("lol")

___assertRaises(TypeError, f, 1)

