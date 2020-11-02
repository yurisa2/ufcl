from fuzzycmeans import fuzzy_clustering
import numpy as np
import pandas as pd

fuzzy_clustering.FCM

data = pd.read_csv('data/data.csv')
data = data.drop(['year'], axis=1)


X = np.array([[0, 1], [0, 2], [0, 2], [0, 10], [0, 10], [0, 9], [0, 9], [0,20]])
fcm = fuzzy_clustering.FCM(n_clusters=3)
fcm.fit(data, [0, 0, 0, 1, 1, 1, 1, 2])

fcm.compute_cluster_centers(data)



# fs = CMeans.CMeansPartitioner(data=data,npart=3, max_iter=10)


sort(CMeans.c_means(5, data, 1, max_iter=1000, patience=100))
