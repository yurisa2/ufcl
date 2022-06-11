from pyFTS.partitioners import Grid, CMeans, Entropy, Huarng, FCM, Util as pUtil
from pyFTS.common import Membership as mf
from pyFTS.models import chen
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.metrics import mean_squared_error
from math import sqrt
import pandas as pd

data = pd.read_csv('contral.csv')

zero_perc = (len(data[data.appl == 0]) / len(data.appl)) * 100

X_train = data.appl


part = pUtil.explore_partitioners(X_train, 10, methods=[Grid.GridPartitioner, CMeans.CMeansPartitioner,
                                                        FCM.FCMPartitioner, Entropy.EntropyPartitioner],
                                  mf=[mf.trimf])

type(part)

plt.plot(part[1])
