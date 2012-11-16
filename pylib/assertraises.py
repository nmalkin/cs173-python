def ___assertRaises(exc, fun, *args):
    try:
        fun(*args)
    except exc:
        pass
    else:
        print("Assert failure!")

