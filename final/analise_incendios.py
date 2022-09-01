import pandas as pd
from io import StringIO

# https://dados.agricultura.gov.br/dataset/snif/resource/f192c841-b95f-4e6e-9a32-70db6838e6f1?view_id=8401fe05-5c45-4fb4-ba36-e9c16ce287b1

with open('/Users/yurisa2/quantzed/ufcl/final/incendios.tsv', 'r') as file:
    data_raw = file.read().replace('"', '')
data_str = StringIO(data_raw)

read_data = pd.read_csv(data_str, sep='\t')

for i in read_data.Estado.unique():
    temp_data = read_data[read_data['Estado'] == i]
    print(i, (len(temp_data[temp_data['Numero'] == 0]) / len(temp_data)) * 100)

acre_data = read_data[read_data['Estado'] == 'Acre']

acre_data["Periodo"] = acre_data['Ano'].astype(str) +"-"+ acre_data["Mes"].astype(str) + '-01'

acre_data["Periodo"] = pd.to_datetime(acre_data["Periodo"])
acre_data = acre_data.sort_values(by=['Periodo'])
acre_data = acre_data.set_index(acre_data["Periodo"])



acre_data.Numero.plot()


data = pd.DataFrame()
data['year'] = acre_data['Periodo']
data['appl'] = acre_data['Numero']
