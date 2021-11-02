import matplotlib.pylab as plt
import pandas as pd
from pyFTS.partitioners import Simple, CMeans, FCM
from pyFTS.common import Membership as mf
from pyFTS.models import chen
from sklearn.metrics import mean_squared_error
from sklearn.model_selection import RandomizedSearchCV
from skopt import gp_minimize


import warnings
warnings.filterwarnings('ignore')


df = pd.read_csv('homolog/contral.csv', sep=",")

data = df['appl'].values

x_train = data[:111]
x_test = data[112:]


class ModelFuzzy(object):

    def generate_sets(self, center_list, data, partitioner):
        data_min = min(data)
        data_max = max(data) + (max(data) / 10)

        prefix = 'A'

        def_list = []
        def_list.append(['A0', [data_min, data_min, center_list[0]]])

        counter = 0
        for centroid_idx in range(0, len(center_list)-2):
            def_list.append([prefix + str(counter+1),
                            [
                            center_list[centroid_idx],
                            center_list[centroid_idx+1],
                            center_list[centroid_idx+2],
                            ]])
            counter += 1
        def_list.append([prefix + str(counter + 1),
                        [center_list[-2], data_max, data_max]])

        for value in def_list:
            partitioner.append(value[0], mf.trimf, value[1])

        return def_list

    def score(self, clustering):
        fs1 = Simple.SimplePartitioner()

        self.generate_sets(clustering, x_train, fs1)

        model1 = chen.ConventionalFTS(partitioner=fs1)

        model1.fit(x_train)
        forecasts1 = model1.predict(x_test)
        y_true1 = x_test
        y_pred1 = forecasts1
        error_mse1 = 0

        error_mse1 = mean_squared_error(y_true1, y_pred1)

        return error_mse1


modelo_fuzzy = ModelFuzzy()
print(modelo_fuzzy.score((1, 2, 3, 4, 5, 6, 7)))
res = gp_minimize(modelo_fuzzy.score, [
                  (1, 2000),
                  (1000, 4000),
                  (2000, 4000),
                  (3000, 5000),
                  (3000, 5000),
                  (4000, 7000),
                  (4000, 7000),
                  ], n_calls=200)
