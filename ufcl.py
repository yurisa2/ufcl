import numpy as np

def MSUB(x, i, j, n):
    return x[i + n * j]


d = 0
dwrk = 0
dwrk_w = 0
iwrk = 0


# NO need for cmeans_setup once it's all about allocating memory


def cmeans_sign(x):
    if x == 0:
        return 0
    ret_sign = 1 if x > 0 else -1
    return ret_sign

def cmeans_weighted_median(x, w, len):
    for i in range(0,10):
        iwrk[i] = i
