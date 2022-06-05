from sklearn.linear_model import LinearRegression
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.metrics import mean_squared_error
import numpy as np


df = pd.read_csv('contral.csv')

zero_perc = (len(df[df.appl == 0]) / len(df.appl)) * 100


ratio = 80
split_fac = round(len(df.appl) * (ratio/100))

X_train = df.appl[:split_fac]
Y_train = X_train.shift(periods=1)


X_test = df.appl[split_fac:]
Y_test = X_test.shift(periods=1)

# Training data
X = X_train.to_numpy().reshape(-1, 1)[1:]  # features
y = Y_train.to_numpy().reshape(-1, 1)[1:]  # target


# Training data
X_test = X_test.to_numpy().reshape(-1, 1)[1:]  # features
Y_test = Y_test.to_numpy().reshape(-1, 1)[1:]  # target


# Train the model
model = LinearRegression()
model.fit(X, y)

# Store the fitted values as a time series with the same time index as
# the training data
y_pred = model.predict(X_test)


plt.plot(Y_test, color="black")
plt.plot(y_pred, color="red")
plt.ylabel('BTC Price')
plt.xlabel('Date')
plt.xticks(rotation=45)
plt.title("Train/Test split for BTC Data")
plt.show()


mse = mean_squared_error(Y_test, y_pred)
rmse = np.sqrt(mse)
