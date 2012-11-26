def ___assertRaises(exc, fun, *args):
    try:
        fun(*args)
    except exc:
        pass
    except:
      	print("Assert failure!")
    else:
        print("Assert failure!")
