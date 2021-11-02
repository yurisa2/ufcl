import matplotlib.pylab as plt
import pandas as pd
from pyFTS.partitioners import Simple, CMeans, FCM
from pyFTS.common import Membership as mf
from pyFTS.models import chen
from sklearn.metrics import mean_squared_error


import warnings
warnings.filterwarnings('ignore')


df = pd.read_csv('homolog/contral.csv', sep=",")
df

data = df['appl'].values

len((data))/3

x_train = data[:111]
x_test = data[112:]

# fig, ax = plt.subplots(nrows=2, ncols=1, figsize=[15, 10])
#
# ax[0].plot(data)
# ax[1].plot(diff.apply(data))

max(x_train)


def generate_sets(center_list, data, partitioner):
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


results = []
for value in range(0, 500):

    # cmeanspartitions = CMeans.CMeansPartitioner(data=data, npart=7)
    cmeanspartitions = FCM.FCMPartitioner(data=data, npart=7)

    initial_clustering = []
    for set in cmeanspartitions.sets:
        initial_clustering.append(cmeanspartitions.sets[set].centroid)

    final_clustering = []
    for idx in range(0, len(initial_clustering) - 1):
        final_clustering.append(
                                (initial_clustering[idx]
                                 + initial_clustering[idx+1]) / 2
                                )

    final_clustering = initial_clustering

    fs1 = Simple.SimplePartitioner()

    generate_sets(final_clustering, x_train, fs1)

    # # fs = Grid.GridPartitioner(data=data, npart=7 )
    # print(fs1.partitions)
    # print(fs1.sets['A4'])
    # dir(fs1)

    model1 = chen.ConventionalFTS(partitioner=fs1)

    model1.fit(x_train)
    forecasts1 = model1.predict(x_test)
    y_true1 = x_test
    y_pred1 = forecasts1
    error_mse1 = 0

    error_mse1 = mean_squared_error(y_true1, y_pred1)
    results.append(error_mse1)
    # print('mse: ' + str(round(error_mse1)))

min(results)
