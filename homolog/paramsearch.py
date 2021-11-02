from sklearn.model_selection import RandomizedSearchCV
import numpy as np
from sklearn.datasets import load_iris
from sklearn.ensemble import RandomForestRegressor
iris = load_iris()
rf = RandomForestRegressor(random_state=35)


X = iris.data
y = iris.target


n_estimators = [int(x) for x in np.linspace(start=1, stop=20, num=20)]
max_features = ['auto', 'sqrt']
max_depth = [int(x) for x in np.linspace(10, 120, num=12)]
min_samples_split = [2, 6, 10]
min_samples_leaf = [1, 3, 4]
bootstrap = [True, False]

random_grid = {'n_estimators': n_estimators,
               'max_features': max_features,
               'max_depth': max_depth,
               'min_samples_split': min_samples_split,
               'min_samples_leaf': min_samples_leaf,
               'bootstrap': bootstrap}

rf_random = RandomizedSearchCV(estimator=rf,

                               param_distributions=random_grid,
                               n_iter=100, cv=5, verbose=2, random_state=35, n_jobs=-1)
rf_random.fit(X, y)

# this prints the contents of the parameters in the random grid
print('Random grid: ', random_grid, '\n')

# print the best parameters
print('Best Parameters: ', rf_random.best_params_, ' \n')

rf_dev = RandomForestRegressor(n_estimators=10, min_samples_split=10,
                               min_samples_leaf=4, max_features='auto', max_depth=70, bootstrap=True)
rf_dev.fit(X, y)
