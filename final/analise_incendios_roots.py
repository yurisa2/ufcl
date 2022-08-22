import pandas as pd

read_data = pd.read_csv('/Users/yurisa2/quantzed/ufcl/final/final_incendios_acre.csv', sep=';')

read_data["Date"] = pd.to_datetime(read_data["Date"])
read_data = read_data.sort_values(by=['Date'])
read_data = read_data.set_index(read_data["Date"])



read_data.Numero.plot()


data = pd.DataFrame()
data['year'] = read_data['Date']
data['appl'] = read_data['Numero']

data.to_csv('final/acre.csv')


data.appl.plot()
