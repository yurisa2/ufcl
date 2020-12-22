"""
"""
import numpy as np
import math
import random as rnd
import functools, operator
from pyFTS.common import FuzzySet, Membership
from pyFTS.partitioners import partitioner

class CustomPartitioner(partitioner.Partitioner):
    """

    """

    def __init__(self, cents, **kwargs):
        # Modern problems require modern solutions
        for index, item in enumerate(cents):
            if item == 0:
                cents[index] = 0.0000000000000000001
            pass

        self.cents = cents

        super(CustomPartitioner, self).__init__(name="Custom", **kwargs)




    def build(self, data):
        sets = {}

        kwargs = {'type': self.type, 'variable': self.variable}

        centroids = self.cents

        centroids.append(self.max)
        centroids.append(self.min)
        centroids = list(set(centroids))
        centroids.sort()
        for c in np.arange(1, len(centroids) - 1):
            _name = self.get_name(c)
            if self.membership_function == Membership.trimf:
                sets[_name] = FuzzySet.FuzzySet(_name, Membership.trimf,
                                                [round(centroids[c - 1], 3), round(centroids[c], 3),
                                                 round(centroids[c + 1], 3)],
                                                round(centroids[c], 3), **kwargs)
            elif self.membership_function == Membership.trapmf:
                q1 = (round(centroids[c], 3) - round(centroids[c - 1], 3)) / 2
                q2 = (round(centroids[c + 1], 3) - round(centroids[c], 3)) / 2
                sets[_name] = FuzzySet.FuzzySet(_name, Membership.trimf,
                                                [round(centroids[c - 1], 3), round(centroids[c], 3) - q1,
                                                 round(centroids[c], 3) + q2, round(centroids[c + 1], 3)],
                                                round(centroids[c], 3), **kwargs)

        return sets
