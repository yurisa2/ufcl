import numpy as np
import matplotlib.pyplot as plt
from skcmeans.algorithms import Probabilistic
from sklearn.datasets import make_blobs
import pandas as pd


data, labels = make_blobs(n_samples=300, centers=n_clusters, random_state=1)

data = pd.read_csv("data/data.csv")

data = data.drop(['year'], axis=1)

data

n_clusters = 4

clusterer = Probabilistic(n_clusters=3, n_init=20)
clusterer.fit(data['appl'])
