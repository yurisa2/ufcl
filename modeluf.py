import ufcl
import pandas as pd

import warnings
warnings.filterwarnings('ignore')

data = pd.read_csv('data/data.csv')
data = data['appl']

xrows = len(data)
xcols = 1
centers = 3
ncenters = 3
weights = range(1, xrows)
m = 2
dist = 2
reltol = 1
verbose = True
rate_par = 0.3
u = xrows * ncenters
ermin = 1
iter = 1


ufcl.cmeans(data,
            xrows,
            1,
            centers,
            ncenters,
            weights,
            m,
            dist - 1,
            100, #itermax
            reltol,
            verbose,
            u,
            ermin,
            iter)
#
# import numpy as np
# test = np.array
# test[0] = 0
#
# teste = list()
# teste[0] = 0
#
# deste = dict()
# deste[0] = 123
