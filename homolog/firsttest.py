
import pandas as pd
from pyFTS.partitioners import Custom, FCM, CMeans, Grid, Util as pUtil
from pyFTS.common import Membership as mf
from pyFTS.common import FLR
from pyFTS.models import chen, cheng, ismailefendi, sadaei, hofts, hwang
from pyFTS.common import Util
from sklearn.metrics import mean_squared_error

from pyFTS.common import Transformations


import itertools
import datetime

import warnings
warnings.filterwarnings('ignore')

import matplotlib.pylab as plt

diff = Transformations.Differential(1)





df = pd.read_csv('homolog/contral.csv', sep=",")
df

data = df['appl'].values



fig, ax = plt.subplots(nrows=2, ncols=1,figsize=[15,10])

ax[0].plot(data)
ax[1].plot(diff.apply(data))


fs = FCM.FCMPartitioner(data=data, npart=7, transformation=diff )
print(dir(fs))
# fs = Grid.GridPartitioner(data=data, npart=7 )
model = chen.ConventionalFTS(partitioner=fs)
model.fit(data)
forecasts = model.predict(data)
y_true = data
y_pred = forecasts
error_mse = 0

error_mse = mean_squared_error(y_true, y_pred)
