import torch
import torchvision
import torchvision.transforms as transforms
from torch.utils.data import DataLoader, TensorDataset
from torchvision.utils import make_grid
from sklearn.base import BaseEstimator
from sklearn.ensemble import RandomForestRegressor
from sklearn.ensemble import AdaBoostRegressor
from sklearn.linear_model import LinearRegression
from sklearn.model_selection import GridSearchCV
from sklearn.pipeline import Pipeline
from joblib import dump

#neural net imports
import torch.nn as nn
import torch.nn.functional as F
import torch.optim as optim
from torch.autograd import Variable

#import external libraries
import pandas as pd
import numpy as np
from scipy import stats
import matplotlib.pyplot as plt
from sklearn.preprocessing import StandardScaler
from sklearn.model_selection import train_test_split
from sklearn.metrics import r2_score, mean_squared_error
import os
import math
import time

class DummyEstimator(BaseEstimator):
    def fit(self): pass
    def score(self): pass

#measure execution time
start = time.time()

parameter_dict = {"pm25": 0, "pm10": 1, "co": 2, "no2": 3, "so2": 4, "o3": 6, "bc": 7}


#load data
data = pd.read_csv("data/pollution_wheather_unified_units.csv", encoding="utf-8")
data = data.drop(["city", "station", "unit", "name", "gust", "visib", "slp", "dewp"], axis=1)

# ensure that data is unique
data = data.groupby(["country", "state", "location", "parameter", "date"]).mean().reset_index()

#convert to row by station
# data = pd.merge(left=pd.pivot_table(data, index=["date", "country", "state", "location"], columns="parameter", values="value", aggfunc=np.mean).reset_index(), right=data.drop_duplicates(subset=["date", "country", "state", "location"]), on=["date", "country", "state", "location"], how="left")
# data = data.drop(columns=["pm25"])
# data = data.dropna(subset= ['pm10', 'co', 'no2', 'so2', 'o3', 'bc'], how='all')

#split get month for further processing
data["date"] = pd.DatetimeIndex(data["date"])
data["month"] = data["date"].dt.month

#fill empty values for input variables
data["wdsp"] = data["wdsp"].fillna(data.groupby('state')['wdsp'].transform('mean'))
data["mxspd"] = data["mxspd"].fillna(data.groupby('state')['mxspd'].transform('mean'))
data["max"] = data["max"].fillna(data.groupby('state')['max'].transform('mean'))
data["min"] = data["min"].fillna(data.groupby('state')['min'].transform('mean'))
data["sndp"] = data["sndp"].fillna(0)

#drop columns where data is missing
data = data.dropna()

#move non numeric columns to the start
cols = list(data)
cols.insert(0, cols.pop(cols.index("date")))
cols.insert(0, cols.pop(cols.index("parameter")))
cols.insert(0, cols.pop(cols.index("location")))
cols.insert(0, cols.pop(cols.index("state")))
cols.insert(0, cols.pop(cols.index("country")))
cols.insert(len(cols), cols.pop(cols.index("value")))
data = data.reindex(cols, axis=1)
data = data.reset_index(drop=True)


results = list()

for parameter, supgroup in data.groupby('parameter'):
    for state, group in supgroup.groupby('state'):
        length = len(group)

        if length >= 1000:
            pipe = Pipeline([
                ('reg', DummyEstimator()),
            ])

            param_grid = [
                          {'reg': [LinearRegression(normalize=True)]},
                          {'reg': [RandomForestRegressor()],
                          'reg__n_estimators': [128, 256, 512],
                          'reg__max_depth': [16, 64, 256]},
                          ]

            search = GridSearchCV(pipe, param_grid, cv=2, scoring='r2').fit(group.iloc[:, range(5, len(data.columns)-1)].values, group.iloc[:, len(data.columns)-1].values)

            print("{} ({}) {}: {} ({})".format(state, length, parameter, search.best_score_, search.best_params_["reg"]))
            results.append([state, parameter, search.best_params_["reg"], search.best_score_])

            np.save("results/results" + parameter + state + ".npy", search.cv_results_)
            dump(search.best_estimator_, "estimators/" + parameter + "_" + state + ".joblib")

np.save("results_summary.npy", np.array(results))