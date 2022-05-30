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


ratio = 80
split_fac = round(len(data.appl) * (ratio/100))

X_train = data.year[:split_fac]


part = pUtil.explore_partitioners(X_train[:split_fac], 100, methods=[Grid.GridPartitioner, CMeans.CMeansPartitioner,
                                                                     FCM.FCMPartitioner, Entropy.EntropyPartitioner,
                                                                     Huarng.HuarngPartitioner],
                                  mf=[mf.trimf])

plt.plot(part)
