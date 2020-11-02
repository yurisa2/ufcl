from __future__ import division, print_function
import numpy as np
import matplotlib.pyplot as plt
import skfuzzy as fuzz
import pandas as pd





data = pd.read_csv("data.csv")



max(data['appl'])

cntr, u, u0, d, jm, p, fpc = fuzz.cmeans(data, 3, 2, error=0.005, maxiter=30, init=None)

cntr * max(data['appl'])
u * max(data['appl'])
u0
jm
p
fpc

d
