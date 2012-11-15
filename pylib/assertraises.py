def ___assertRaises(exc, fun, *args):
    try:
        fun(*args)
    except TypeError:
        pass
    else:
        raise RuntimeError("Assert failure")

