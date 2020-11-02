from __future__ import division, print_function
import numpy as np
import matplotlib.pyplot as plt
import skfuzzy as fuzz
import pandas as pd





data = pd.read_csv("data/data.csv")

data =data.drop(['year'], axis=1)

cntr, u, u0, d, jm, p, fpc = fuzz.cmeans(data, 3, 2, 4, maxiter=10044400, init=None)


u * max(data['appl'])

cntr
u
u0
jm
p
fpc

d
