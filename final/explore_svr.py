import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
from sklearn.svm import SVR

from sklearn.preprocessing import StandardScaler
sc_X = StandardScaler()
sc_y = StandardScaler()
X = sc_X.fit_transform(X)
y = sc_y.fit_transform(y)

regressor = SVR(kernel='rbf')
regressor.fit(X,y)

y_pred = regressor.predict(6.5)
