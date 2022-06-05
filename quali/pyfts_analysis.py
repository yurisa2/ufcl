import pickle
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

ratio = 80
data = pd.read_csv('contral.csv')
split_fac = round(len(data.appl) * (ratio/100))

X_train = data.year[:split_fac]
Y_train = data.appl[:split_fac].to_list()

X_test = data.year[split_fac:]
Y_test = data.appl[split_fac:].to_list()

with open('full_dataset.pickle', 'rb') as f:
    # The protocol version used is detected automatically, so we do not
    # have to specify it.
    data = pickle.load(f)


data.sort_values(by='rmse', ascending=True)

select_data = data.loc[data['model'] == 'sadaei']
# select_data.set_index('parts', inplace=True)

select_data.boxplot(by='partitioner', column=['rmse'])
select_data.hist(column='rmse')
#
# select_data.groupby('partitioner').plot(
#     None, 'rmse', subplots=True, legend=True)

select_data.pivot(index='parts', columns='partitioner', values='rmse').plot()


winners = data.loc[data.groupby('model').rmse.idxmin()]  # Winners

forecasts_chen = np.array(
    winners[winners['model'] == 'chen']['forecasts']).tolist()[0]
forecasts_sadaei = np.array(winners[winners['model'] == 'sadaei']['forecasts']).tolist()[
                                                                             0]
forecasts_cheng = np.array(
    winners[winners['model'] == 'cheng']['forecasts']).tolist()[0]

plt.plot(X_test, Y_test, label='True')
plt.plot(X_test, forecasts_chen, label='Chen')
plt.plot(X_test, forecasts_cheng, label='Cheng')
plt.plot(X_test, forecasts_sadaei, label='Sadaei')
plt.legend()
plt.show()
