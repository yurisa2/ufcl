import pandas as pd
import matplotlib.pyplot as plt
import numpy as np


ratio = 80
data = pd.read_csv('final/acre.csv')
split_fac = round(len(data.appl) * (ratio/100))

X_train = data.year[:split_fac]
Y_train = data.appl[:split_fac].to_list()

X_test = data.year[split_fac:]
Y_test = data.appl[split_fac:].to_list()


data = data.rename(columns={'appl': 'Incendios'})
data = data.rename(columns={'year': 'Datas'})


data.plot('Datas', 'Incendios', figsize=(16, 10))
plt.show()


data.boxplot('Incendios', figsize=(16, 4), vert=False)
plt.xlabel('Quantidade')
plt.show()


data.hist('Incendios')
plt.show()

plt.figure(figsize=(16, 4))
plt.plot(X_train, Y_train, label='Train')
plt.plot(X_test, Y_test, label='Test')
plt.legend()
plt.xlabel('Datas')
plt.ylabel('Incendios')
plt.show()
