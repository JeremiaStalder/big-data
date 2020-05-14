import torch
import torchvision
import torchvision.transforms as transforms
from torch.utils.data import DataLoader, TensorDataset
from torchvision.utils import make_grid
from sklearn.ensemble import RandomForestRegressor
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

#measure execution time
start = time.time()

parameter_dict = {"pm25": 0, "pm10": 1, "co": 2, "no2": 3, "so2": 4, "o3": 6, "bc": 7}


#load data
data = pd.read_csv("data/pollution_wheather_unified_units.csv", encoding="utf-8")
data = data.drop(["city", "station", "unit", "name", "gust", "visib", "slp", "dewp"], axis=1)

# ensure that data is unique
data = data.groupby(["country", "state", "parameter", "date"]).mean().reset_index()

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
cols.insert(0, cols.pop(cols.index("state")))
cols.insert(0, cols.pop(cols.index("country")))
cols.insert(len(cols), cols.pop(cols.index("value")))
data = data.reindex(cols, axis=1)
data = data.reset_index(drop=True)


for parameter, group in data.groupby('parameter'):

    train_X, test_X, train_y, test_y = train_test_split(group.iloc[:, range(4, len(data.columns)-1)].values, group.iloc[:, len(data.columns)-1].values, test_size=0.30)

    max_depth = [64, 128, 256, 512, 1024]
    n_estimators = [128, 256, 512, 1024]

    results = np.zeros((6, 5))
    results[1:, 0] = max_depth
    results[0, 1:] = n_estimators

    for i in range(len(max_depth)):
        for j in range(len(n_estimators)):
            rf_regr = RandomForestRegressor(max_depth=max_depth[i], n_estimators=n_estimators[j], random_state=42)
            rf_regr.fit(train_X, train_y)
            r2 = rf_regr.score(test_X, test_y)
            print("{}: {}".format(parameter, r2))
            results[i+1, j+1] = r2

    np.save("results/parameter/results_state" + parameter + ".npy", results)
    #dump(rf_regr, "model/parameter/" + parameter + ".joblib")