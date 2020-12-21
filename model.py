from pyFTS.partitioners import CMeans, FCM
from pyFTS.common import FLR
from pyFTS.models import chen
from pyFTS.common import Util
from sklearn.metrics import mean_squared_error

import pandas as pd

import warnings
warnings.filterwarnings('ignore')


%pylab inline



data = pd.read_csv('data/data.csv')
data = data['appl']

data['appl'][28]

# fs = CMeans.CMeansPartitioner(data=data,npart=3, max_iter=10)




sort(CMeans.c_means(7, data, 1, max_iter=1000, patience=100))

from sklearn.cluster import KMeans

datak = data.array

datak = np.array(datak)
datak = datak.reshape(-1, 1)

kmeans = KMeans(n_clusters=7, random_state=0).fit(datak)
np.sort(kmeans.cluster_centers_)
