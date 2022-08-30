from statsmodels.graphics.tsaplots import plot_acf
import matplotlib.pyplot as plt
import pandas as pd


data_acre = pd.read_csv('final/acre.csv')


plot_acf(data_acre.appl, lags=200)
# Show the AR as a plot
plt.show()


data_contral = pd.read_csv('/Users/yurisa2/quantzed/ufcl/data/contral.csv')


plot_acf(data_contral.appl, lags=200)
# Show the AR as a plot
plt.show()
