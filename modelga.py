# -*- coding: utf-8 -*-
import sys
if sys.version_info[0] < 3:
    from StringIO import StringIO
else:
    from io import StringIO

import pandas as pd
from pyFTS.partitioners import Custom
from pyFTS.common import Membership as mf
from pyFTS.common import FLR
from pyFTS.models import chen, cheng, ismailefendi, sadaei, hofts, hwang
from pyFTS.common import Util
from sklearn.metrics import mean_squared_error

import itertools
import datetime

import warnings
warnings.filterwarnings('ignore')

import matplotlib.pylab as plt

from geneticalgorithm import geneticalgorithm as ga
import numpy as np


# %pylab inline

"""# Data Source"""

data_source = StringIO("""appl,year
2385,1
3162.5,2
0,3
2930,4
2895,5
2385,6
2385,7
3330,8
3360,9
765,10
2752.5,11
1950,12
2885,13
3405,14
2820,15
3272.5,16
2280,17
2010,18
1200,19
5074,20
3210,21
2310,22
2665.5,23
2494,24
1275,25
2550,26
0,27
0,28
3150,29
4760,30
2055,31
1920,32
2601,33
3336,34
2300,35
1850,36
3431,37
1075,38
2040,39
2392.5,40
2752.5,41
2202.5,42
3912.5,43
3142.5,44
0,45
3585,46
0,47
3300,48
1642.5,49
3855,50
2040,51
2820,52
4860,53
3140,54
2701.5,55
0,56
1350,57
0,58
2625,59
2085,60
1805,61
2545,62
0,63
0,64
6105,65
1350,66
0,67
3675,68
2625,69
0,70
2040,71
2295,72
4035,73
0,74
0,75
5289,76
1275,77
2655,78
5052,79
0,80
1350,81
2625,82
0,83
2625,84
3525,85
0,86
300,87
1530,88
1530,89
4995,90
0,91
2115,92
0,93
1416.5,94
3645,95
0,96
2155,97
980,98
0,99
2330,100
0,101
2040,102
0,103
840,104
2880,105
2880,106
2560,107
2700,108
2298,109
617.5,110
875,111
1725,112
0,113
0,114
4100,115
3275,116
0,117
2487.5,118
0,119
650,120
1042,121
5595,122
0,123
4254,124
875,125
0,126
4061.5,127
1221,128
4769,129
480,130
2002,131
3390,132
0,133
0,134
3530,135
1350,136
3250,137
2180,138
0,139
0,140
0,141
900,142
2100,143
2300,144
0,145
0,146
1800,147
0,148
3480,149
0,150
0,151
1380,152
0,153
0,154
3450,155
0,156
0,157
0,158
2760,159
2300,160
0,161
0,162
0,163
50,164
75,165
1380,166
1380,167
0,168
0,169
2300,170
75,171
2300,172
0,173
300,174
3905,175
0,176
1275,177
3575,178
0,179
560,180
1380,181
700,182
1380,183
0,184
2425,185
0,186
1380,187
1850,188
1410,189
1380,190
0,191
2470,192
2197.5,193
0,194
1580,195
2070,196
0,197
2760,198
2415,199
1150,200
805,201
2350,202
2760,203
600,204
0,205
1380,206
1780,207
2350,208
1875,209
2642.5,210
1372,211
2738.5,212
3372.5,213
1020.5,214
6170,215
1380,216
1380,217
2760,218
0,219
3143,220
0,221
1380,222
2300,223
""")

df = pd.read_csv(data_source, sep=",")
df

data = df['appl'].values


centros = [0, 200, 1500, 2000, 3000, 3500,4000]

minimo = min(data)
maximo = max(data)

varbound=np.array([[minimo,maximo]]*7)

def runModel(centros):
    fs = Custom.CustomPartitioner(centros,data=data )
    model = chen.ConventionalFTS(partitioner=fs)
    model.fit(data)
    forecasts = model.predict(data)
    y_true = data
    y_pred = forecasts
    error_mse = 0

    try:
        error_mse = mean_squared_error(y_true, y_pred)
    except:
        error_mse = -1

    return error_mse

runModel(centros)


model=ga(function=runModel,dimension=7,variable_type='real',variable_boundaries=varbound)
model.run()
