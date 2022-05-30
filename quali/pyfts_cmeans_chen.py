from pyFTS.partitioners import Grid, CMeans
from pyFTS.models import chen
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.metrics import mean_squared_error
from math import sqrt
import pandas as pd

data = pd.read_csv('quali/contral.csv')

zero_perc = (len(data[data.appl == 0]) / len(data.appl)) * 100

# plt.plot(data.appl)
# plt.boxplot(data.appl)


df = pd.DataFrame()

for ratio in range(10, 90, 20):
    for parts in range(10, 500, 10):
        split_fac = round(len(data.appl) * (ratio/100))

        X_train = data.year[:split_fac]
        Y_train = data.appl[:split_fac].to_list()

        X_test = data.year[split_fac:]
        Y_test = data.appl[split_fac:].to_list()

        temp_dict = {}
        partitioner = CMeans.CMeansPartitioner(data=Y_train, npart=parts)
        model = chen.ConventionalFTS(partitioner=partitioner)
        model.fit(Y_train)
        forecasts = model.predict(Y_test)

        rmse = sqrt(mean_squared_error(Y_test, forecasts))
        temp_dict['rmse'] = rmse
        temp_dict['mse'] = mean_squared_error(Y_test, forecasts)
        temp_dict['parts'] = parts
        temp_dict['ratio'] = ratio

        df = df.append(temp_dict, ignore_index=True, sort=False)

max(df.mse)
min(df.mse)

df.sort_values(by='mse', ascending=False)

round(df.mse)

plt.plot(df[["rmse", "parts", "ratio"]])
plt.legend(df[["rmse", "parts", "ratio"]])
#
# fig, ax = plt.subplots(nrows=1, ncols=1, figsize=[15, 5])
#
# forecasts = model.predict(Y_test)
# forecasts.insert(0, None)
#
# orig, = plt.plot(Y_test, label="Original data")
# pred, = plt.plot(forecasts, label="Forecasts")
