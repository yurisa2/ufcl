import numpy as np


def division(measure, previousI):
  return measure / previousI


def power(measure):
  return np.power(measure, 2)


def summation(measure):
  return np.sum(measure)


def mean(N, measure):
  return (1/N) * measure


def sqrt(measure):
  return np.sqrt(measure)


def computeTheilU2(y_true, y_pred):
    N = len(y_true)

    subtractionNumerator = y_pred[1:] - y_true[1:]
    divisionNumerator = division(subtractionNumerator, y_true[:-1])
    powerNumerator = power(divisionNumerator)
    summationNumerator = summation(powerNumerator)
    meanNumerator = mean(N, summationNumerator)
    numerator = sqrt(meanNumerator)

    subtractionDenominator = y_true[1:] - y_true[:-1]
    powerDenominator = power(division(subtractionDenominator, y_true[:-1]))
    denominator = sqrt(mean(N, summation(powerDenominator)))

    theilU2 = numerator / denominator

    return theilU2
