from sklearn.model_selection import GridSearchCV
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
from sklearn.svm import SVR
from sklearn.metrics import mean_squared_error

from sklearn.preprocessing import StandardScaler
# get the libraries
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd


ratio = 80
data = pd.read_csv('/Users/yurisa2/quantzed/ufcl/data/contral.csv')
data['date'] = pd.to_datetime(data.year)
data['year'] = data.index + 1


split_fac = round(len(data.appl) * (ratio/100))


X_train = np.array(data.appl[:split_fac]).reshape(-1, 1)[:-1]
Y_train = np.array(data.appl[:split_fac].shift(-1)).reshape(-1, 1)[:-1]


X_test = np.array(data.appl[split_fac:]).reshape(-1, 1)[:-1]
Y_test = np.array(data.appl[split_fac:].shift(-1)).reshape(-1, 1)[:-1]


#
# svr_rbf = SVR(kernel='rbf', C=1e4, gamma=0.1)
# svr_lin = SVR(kernel='linear', C=1e4)
# svr_poly = SVR(kernel='poly', C=1e4, degree=2)
# y_rbf = svr_rbf.fit(X_train, Y_train).predict(X_test)
# y_lin = svr_lin.fit(X_train, Y_train).predict(X_test)
# y_poly = svr_poly.fit(X_train, Y_train).predict(X_test)


parameters = {'kernel': ('linear', 'rbf', 'poly'), 'C': [
                         0.01, 0.5, 1, 10, 50, 100], 'epsilon': [0.1, 1, 30]}
svr = SVR()
clf = GridSearchCV(svr, parameters, n_jobs=-1)

results = clf.fit(X_train, Y_train)
best_par = clf.best_params_
print(best_par)

predictions = clf.predict(X_test)

rmse = mean_squared_error(Y_test, predictions)
print(rmse)

plt.figure(figsize=(16, 4))
plt.plot(data['year'][-44:], Y_test, label='True')
plt.plot(data['year'][-44:],
         predictions, label='SVR')
plt.title('Support Vector Regression (Demanda diária de Pallets)')
plt.xticks(data['year'][-44:], rotation=45)
plt.legend()
plt.savefig('final/plots/svr_results_contral.png')
plt.show()
