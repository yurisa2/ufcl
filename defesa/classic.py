import pandas as pd
import matplotlib.pyplot as plt
from statsmodels.tsa.statespace.sarimax import SARIMAX
import numpy as np
from sklearn.metrics import mean_squared_error


data = pd.read_csv('defesa/contral.csv')

zero_perc = (len(data[data.appl == 0]) / len(data.appl)) * 100


split_fac = round(len(data.appl) * 0.8)

X_train = data.year[:split_fac]
Y_train = data.appl[:split_fac]

X_test = data.year[split_fac:]
Y_test = data.appl[split_fac:]

plt.plot(Y_train, color="black")
plt.plot(Y_test, color="red")
plt.ylabel('BTC Price')
plt.xlabel('Date')
plt.xticks(rotation=45)
plt.title("Train/Test split for BTC Data")
plt.show()


ARMAmodel = SARIMAX(Y_train, order=(1, 0, 1))
ARMAmodel = ARMAmodel.fit()

y_pred = ARMAmodel.get_forecast(len(Y_test.index))
y_pred_df = y_pred.conf_int(alpha=0.05)
y_pred_df["Predictions"] = ARMAmodel.predict(
    start=y_pred_df.index[0], end=y_pred_df.index[-1])
y_pred_df.index = Y_test.index
y_pred_out = y_pred_df["Predictions"]
y_pred_out = y_pred_df["Predictions"]


plt.plot(y_pred_out, color='green', label='Predictions')
plt.legend()

arma_rmse = np.sqrt(mean_squared_error(
    Y_test.values, y_pred_df["Predictions"]))
print("RMSE: ", arma_rmse)
print("MSE: ", arma_rmse ** 2)
