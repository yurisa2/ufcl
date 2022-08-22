import pickle
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np


def specific_plots(model_name):

    select_data = data.loc[data['model'] == model_name]

    select_data.boxplot(by='partitioner', column=['rmse'], figsize=(16, 4))
    plt.savefig('final/plots/'+model_name+'_pyfts_analysis_boxplot_rmse.png')
    plt.show()

    select_data.hist(column='rmse', figsize=(16, 4))
    plt.savefig('final/plots/'+model_name+'_pyfts_analysis_hist_rmse.png')
    plt.show()

    select_data.pivot(index='parts', columns='partitioner',
                      values='rmse').plot(figsize=(16, 4))
    plt.savefig('final/plots/'+model_name+'_pyfts_analysis_pivot_partitioner.png')
    plt.show()


ratio = 80
data = pd.read_csv('final/acre.csv')
split_fac = round(len(data.appl) * (ratio/100))

X_train = data.year[:split_fac]
Y_train = data.appl[:split_fac].to_list()

X_test = data.year[split_fac:]
Y_test = data.appl[split_fac:].to_list()

with open('final/full_dataset.pickle', 'rb') as f:
    # The protocol version used is detected automatically, so we do not
    # have to specify it.
    data = pickle.load(f)

# Just to check some vars
data.sort_values(by='rmse', ascending=True)

specific_plots('song')
specific_plots('sadaei')
specific_plots('cheng')

winners = data.loc[data.groupby('model').rmse.idxmin()]  # Winners

forecasts_song = np.array(
    winners[winners['model'] == 'song']['forecasts']).tolist()[0]

forecasts_sadaei = np.array(winners[winners['model'] == 'sadaei']['forecasts']).tolist()[
                                                                             0]
forecasts_cheng = np.array(
    winners[winners['model'] == 'cheng']['forecasts']).tolist()[0]


plt.figure(figsize=(16, 4))
plt.plot(X_test, Y_test, label='True')
plt.plot(X_test, forecasts_song, label='Song')
plt.plot(X_test, forecasts_cheng, label='Cheng')
plt.plot(X_test, forecasts_sadaei, label='Sadaei')
plt.legend()
plt.savefig('final/plots/full_results_pyfts_analysis_pivot_partitioner.png')
plt.show()
