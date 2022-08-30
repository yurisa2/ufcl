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


data = data.rename(columns={'appl': 'Demanda'})
data = data.rename(columns={'year': 'Dias'})


data.plot('Dias', 'Demanda', figsize=(16, 10))
plt.show()


data.boxplot('Demanda', figsize=(16, 4), vert=False)
plt.xlabel('Quantidade')
plt.show()


data.hist('Demanda')
plt.show()

plt.figure(figsize=(16, 4))
plt.plot(X_train, Y_train, label='Train')
plt.plot(X_test, Y_test, label='Test')
plt.legend()
plt.xlabel('Dias')
plt.ylabel('Demanda')
plt.show()
