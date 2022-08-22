import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
from sklearn.svm import SVR

from sklearn.preprocessing import StandardScaler



ratio = 80
data = pd.read_csv('final/acre.csv')
split_fac = round(len(data.appl) * (ratio/100))

X_train = data.year[:split_fac]
Y_train = data.appl[:split_fac].to_list()

X_test = np.array(data.year[split_fac:].to_list()).reshape(-1, 1)
Y_test = np.array(data.appl[split_fac:].to_list()).reshape(-1, 1)


regressor = SVR(kernel='rbf')
regressor = regressor.fit(X_test,Y_test)

forecasts_svr = regressor.predict()
