from pyFTS.partitioners import CMeans, FCM
from pyFTS.common import FLR
from pyFTS.models import chen
from pyFTS.common import Util
from sklearn.metrics import mean_squared_error

import pandas as pd

import warnings
warnings.filterwarnings('ignore')


import matplotlib.pylab as plt

%pylab inline



data = pd.read_csv('data/data.csv')
data = data['appl']


# fs = CMeans.CMeansPartitioner(data=data,npart=3, max_iter=10)




sort(CMeans.c_means(3, data, 3, max_iter=1000, patience=100))
