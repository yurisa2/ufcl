from pyFTS.partitioners import Grid, CMeans, Entropy, Huarng, FCM, Util as pUtil
from pyFTS.common import Membership as mf
from pyFTS.models import chen
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.metrics import mean_squared_error
from math import sqrt
import pandas as pd

data = pd.read_csv('quali/contral.csv')

zero_perc = (len(data[data.appl == 0]) / len(data.appl)) * 100

X_train = data.appl


part = pUtil.explore_partitioners(X_train, 10, methods=[Grid.GridPartitioner, CMeans.CMeansPartitioner,
                                                        FCM.FCMPartitioner, Entropy.EntropyPartitioner],
                                  mf=[mf.trimf])


fs = Grid.GridPartitioner(data=X_train, npart=10)
fig, ax = plt.subplots(nrows=1, ncols=1, figsize=[15, 5])
fs.plot(ax)
plt.savefig('quali/plots/Grid.svg', format='svg')

fs = CMeans.CMeansPartitioner(data=X_train, npart=12)
fig, ax = plt.subplots(nrows=1, ncols=1, figsize=[15, 5])
fs.plot(ax)
plt.savefig('quali/plots/CMeans.svg', format='svg')

fs = FCM.FCMPartitioner(data=X_train, npart=9)
fig, ax = plt.subplots(nrows=1, ncols=1, figsize=[15, 5])
fs.plot(ax)
plt.savefig('quali/plots/FCM.svg', format='svg')


fs = Entropy.EntropyPartitioner(data=X_train, npart=16)
fig, ax = plt.subplots(nrows=1, ncols=1, figsize=[15, 5])
fs.plot(ax)
plt.savefig('quali/plots/Entropy.svg', format='svg')


# Plotar um por um, deixa de preguiça

plt.plot(part[1])
plt.savefig('partitioner.eps', format='eps')
