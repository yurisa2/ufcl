# from pyFTS.common import FLR
# from pyFTS.common import Util

import pandas as pd


data = pd.read_csv('data/data.csv')
data = data['appl']



from pyFTS.partitioners import Custom

cus = Custom.CustomPartitioner([0, 200, 1500, 2000, 3000, 3500,4000],data=data )

cus.sets
