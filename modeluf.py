import ufcl
import pandas as pd

import warnings
warnings.filterwarnings('ignore')

data = pd.read_csv('data/data.csv')
data = data['appl']

xrows = len(data)
xcols = 1
centers = 2
ncenters = 2
weights = range(1, xrows)
m = 2
dist = 2
reltol = 1
verbose = True
rate_par = 0.3
u = xrows * ncenters
ermin = 1
iter = 1

weights = 1
weights = ufcl.np.asarray(weights)


weights = ufcl.np.repeat(weights, xrows)



ufcl.cmeans(data,
            xrows,
            1,
            centers,
            ncenters,
            weights,
            m,
            dist - 1,
            5, #itermax
            reltol,
            verbose,
            u,
            ermin,
            iter)

ufcl.u
ufcl.print_d()



# d = {345: 'value'}
# print(d)
# # {'key': 'value'}https://pt.pornhub.com/view_video.php?viewkey=ph5fcbf46bb1e1d
# d[3345] = 'mynewvalue'
# print(d)
# # {'key': 'value', 'mynewkey': 'mynewvalue'}
#
# d = dict()


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
