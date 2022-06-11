from pyFTS.partitioners import CMeans, Grid, FCM, Entropy
from pyFTS.models import song, cheng, sadaei
import pandas as pd
from sklearn.metrics import mean_squared_error
from math import sqrt
import pandas as pd
import time
import threading
import pickle

ratio = 80


data = pd.read_csv('contral.csv')
split_fac = round(len(data.appl) * (ratio/100))

X_train = data.year[:split_fac]
Y_train = data.appl[:split_fac].to_list()

X_test = data.year[split_fac:]
Y_test = data.appl[split_fac:].to_list()

zero_perc = (len(data[data.appl == 0]) / len(data.appl)) * 100

# plt.plot(data.appl)
# plt.boxplot(data.appl)


def run_model(model_name, partitioner_name, parts, train, test):

    if(partitioner_name == 'grid'):
        partitioner = Grid.GridPartitioner(data=Y_train, npart=parts)
    if(partitioner_name == 'cmeans'):
        partitioner = CMeans.CMeansPartitioner(data=Y_train, npart=parts)
    if(partitioner_name == 'fcm'):
        partitioner = FCM.FCMPartitioner(data=Y_train, npart=parts)
    if(partitioner_name == 'entropy'):
        partitioner = Entropy.EntropyPartitioner(data=Y_train, npart=parts)

    if(model_name == 'song'):
        model = song.ConventionalFTS(partitioner=partitioner)
    elif(model_name == 'cheng'):
        model = cheng.TrendWeightedFTS(partitioner=partitioner)
    elif(model_name == 'sadaei'):
        model = sadaei.ExponentialyWeightedFTS(partitioner=partitioner)

    temp_dict = {}
    model.fit(Y_train)
    forecasts = model.predict(Y_test)

    rmse = sqrt(mean_squared_error(Y_test, forecasts))
    temp_dict['model'] = model_name
    temp_dict['partitioner'] = partitioner_name
    temp_dict['rmse'] = rmse
    temp_dict['mse'] = mean_squared_error(Y_test, forecasts)
    temp_dict['parts'] = parts
    temp_dict['forecasts'] = forecasts

    # result_df = result_df.append(temp_dict, ignore_index=True, sort=False)

    return temp_dict


models = ['song', 'cheng', 'sadaei']
partitioners = ['grid', 'cmeans', 'fcm', 'entropy']
df = pd.DataFrame()

params_list = []

counter = 0

for model in models:
    for partitioner in partitioners:
        for parts in range(5, 500, 5):
            counter += 1
            print(counter, model, partitioner, parts)
            # threading.Thread(target=run_model, args=(
            #     model, partitioner, parts, Y_train, Y_test, df)).start()
            try:
                result = run_model(model, partitioner, parts, Y_train, Y_test)
                df = df.append(result, ignore_index=True, sort=False)
            except Exception as e:
                print(e, counter, model, partitioner, parts)

#
# with open('full_dataset.pickle', 'wb') as f:
#     # Pickle the 'data' dictionary using the highest protocol available.
#     pickle.dump(df, f, pickle.HIGHEST_PROTOCOL)
