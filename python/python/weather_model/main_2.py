import torch
import torchvision
import torchvision.transforms as transforms
from torch.utils.data import DataLoader, TensorDataset
from torchvision.utils import make_grid

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

#check for cuda
print(torch.cuda.is_available())
print(torch.backends.cudnn.enabled)

if torch.cuda.is_available():
    device = torch.device("cuda")
    print(torch.cuda.get_device_name(0))
else:
    device = "cpu"
    print("cpu")

use_preloaded_data = True
parameter_dict = {"pm25": 0, "pm10": 1, "co": 2, "no2": 3, "so2": 4, "o3": 6, "bc": 7}

if not use_preloaded_data:

    #load data
    data = pd.read_csv("data/pollution_wheather_unified_units.csv", encoding="utf-8")
    data = data.drop(["city", "station", "unit", "name", "gust", "visib", "slp", "dewp"], axis=1)

    #limit scope to Germany for faster processing
    data = data.loc[data["location"]=="richmond"]

    # ensure that data is unique
    data = data.groupby(["country", "state", "location", "parameter", "date"]).mean().reset_index()

    #convert to row by station
    # data = pd.merge(left=pd.pivot_table(data, index=["date", "country", "state", "location"], columns="parameter", values="value", aggfunc=np.mean).reset_index(), right=data.drop_duplicates(subset=["date", "country", "state", "location"]), on=["date", "country", "state", "location"], how="left")
    # data = data.drop(columns=["pm25"])
    # data = data.dropna(subset= ['pm10', 'co', 'no2', 'so2', 'o3', 'bc'], how='all')

    #split get month for further processing
    data["date"] = pd.DatetimeIndex(data["date"])
    data["month"] = data["date"].dt.month

    #transform parameter into numeric value
    data.loc[:,"param_num"] = data.loc[:,"parameter"].map(parameter_dict)

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

    #create sequences
    lag= 7

    X = np.zeros((data.shape[0], lag, len(data.columns)-5))
    # X[:, :, :-1] = np.nan
    length = np.zeros(data.shape[0])

    scaler = StandardScaler()
    X[:, 0, 3:-3] = scaler.fit_transform(data.iloc[:, range(8, len(data.columns)-3)].values)
    X[:, 0, -3:-1] = data.iloc[:, -3:-1].values
    X[:, 0, 0:3] = data.iloc[:, 5:8].values
    xcols = data.iloc[:, range(5, len(data.columns)-1)].columns
    xcols = np.append(xcols, "lag to day before")

    y = data.iloc[:, len(data.columns)-1].values

    data = data.iloc[:, range(0,5)]

    j = 0
    max_j = len(data.groupby(["country", "state", "location", "parameter"]))
    for name, group in data.groupby(["country", "state", "location", "parameter"]):

        group = group.sort_values(by=['date'], ascending=False)
        group ["timedelta"] = group["date"].diff(periods=1).dt.days * -1
        group = group.fillna(1)
        group = group.reset_index(drop=False)
        for index, row in group.iterrows():
            i = 1
            k = 1
            while i <= lag-1 and index+i < group.shape[0]:
                new = X[group.iloc[index + i]["index"], 0, :]
                new[-1] = group.iloc[index + i]["timedelta"]
                X[row["index"], k, :] = new
                i += int(group.iloc[index + i]["timedelta"])
                k += 1
            length[row["index"]] = k

        if j % 100 == 0:
            print("{} / {}".format(j, max_j))
        if j % 1000 == 0:
            np.save("data/sequenzializedData", X)
            np.save("data/sequenzializedLength", length)
        j+= 1

    lags_needed = 7
    length_filter = length >= lags_needed
    length = length[length_filter]
    X = X[length_filter]
    y = y[length_filter]

    np.save("data/sequenzializedData_bavaria.npy", X)
    np.save("data/sequenzializedLength_bavaria.npy", length)
    np.save("data/sequenzializedY_bavaria.npy", y)
    np.save("data/XColNames.npy", np.array(xcols))

X = np.load("data/sequenzializedData_bavaria.npy")
length = np.load("data/sequenzializedLength_bavaria.npy")
y = np.load("data/sequenzializedY_bavaria.npy")
xcols = np.load("data/XColNames.npy", allow_pickle=True)

#filter for one particle
particle = "bc"
particle_filter = X[:, 0, 17] == parameter_dict[particle]
X = X[particle_filter, : , :]
length = length[particle_filter]
y = y[particle_filter]

#inverse X
X = np.flip(X, axis=1)

for i in range(0,19):
    fig, ax1 = plt.subplots()
    ax1.set_xlabel('days')
    ax1.set_ylabel(xcols[i])
    ax1.plot(X[:,0, i], color="blue")
    ax2 = ax1.twinx()
    ax2.set_ylabel(particle)
    ax2.plot(y, color="orange")
    plt.title(xcols[i])
    plt.show()

X = X.astype('float32')
length = length.astype('int32')
y = y.astype('float32')

#Training, Validation and Test Split
train_X, test_X, train_length, test_length, train_y, test_y = train_test_split(X, length, y, test_size=0.20)
train_X, val_X, train_length, val_length, train_y, val_y = train_test_split(train_X, train_length, train_y, test_size=0.20)

simple_train_X = train_X[:, 0, :]
simple_val_X = val_X[:, 0, :]

from sklearn.ensemble import RandomForestRegressor
rf_regr = RandomForestRegressor(max_depth=40, n_estimators=250, random_state=0)
rf_regr.fit(simple_train_X, train_y)
print(rf_regr.score(simple_val_X, val_y))

del X, length, y

#Convert to tensor and normalize
#train
train_X = torch.tensor(train_X)
train_length = torch.tensor(train_length)
train_y = torch.tensor(train_y)
train_tensor = TensorDataset(train_X, train_length, train_y)

#val
val_X = torch.tensor(val_X)
val_length = torch.tensor(val_length)
val_y = torch.tensor(val_y)
val_tensor = TensorDataset(val_X, val_length, val_y)

#test
test_X = torch.tensor(test_X)
test_length = torch.tensor(test_length)
test_tensor = TensorDataset(test_X, test_length)

#generate dataloader
train_loader = DataLoader(train_tensor, batch_size=128, num_workers=0, shuffle=True)
val_loader = DataLoader(val_tensor, batch_size=128, num_workers=0, shuffle=True)
test_loader = DataLoader(test_tensor, batch_size=128, num_workers=0, shuffle=False)

#network
class Weather_Net(nn.Module):
    def __init__(self, embedding_dim):
        super(Weather_Net, self).__init__()

        self.lstm = nn.LSTM(embedding_dim, 256, num_layers=2, bidirectional=False, batch_first=True)

        self.linear_block1 = nn.Sequential(
            nn.Dropout(p=0.25),
            nn.Linear(256 * 2, 512),
            nn.BatchNorm1d(512),
            nn.ReLU(inplace=True),
        )

        self.linear_block2 = nn.Sequential(
            nn.Dropout(p=0.25),
            nn.Linear(512, 512),
            nn.BatchNorm1d(512),
            nn.ReLU(inplace=True),
        )

        self.linear_block3 = nn.Sequential(
            nn.Dropout(0.25),
            nn.Linear(512, 128),
            nn.BatchNorm1d(128),
            nn.ReLU(inplace=True),
        )
        self.linear_block_final = nn.Sequential(
            nn.Linear(128, 1),
            nn.ReLU(inplace=True)
        )

    def forward(self, X, length):
        #X = torch.nn.utils.rnn.pack_padded_sequence(X, length, batch_first=True, enforce_sorted=False)

        lstm_out, (ht, ct) = self.lstm(X)
        X = torch.flatten(ct.permute(1, 0, 2), start_dim=1)

        X = self.linear_block1(X)
        X = self.linear_block2(X)
        X = self.linear_block3(X)
        X = self.linear_block_final(X)

        X = X.squeeze()

        return X

class Weather_Net2(nn.Module):
    def __init__(self, embedding_dim):
        super(Weather_Net2, self).__init__()

        self.linear_block1 = nn.Sequential(
            nn.Dropout(p=0.25),
            nn.Linear(133, 512),
            nn.BatchNorm1d(512),
            nn.ReLU(inplace=True),
        )

        self.linear_block2 = nn.Sequential(
            nn.Dropout(p=0.25),
            nn.Linear(512, 512),
            nn.BatchNorm1d(512),
            nn.ReLU(inplace=True),
        )

        self.linear_block3 = nn.Sequential(
            nn.Dropout(0.25),
            nn.Linear(512, 128),
            nn.BatchNorm1d(128),
            nn.ReLU(inplace=True),
        )
        self.linear_block_final = nn.Sequential(
            nn.Linear(128, 1),
            nn.ReLU(inplace=True)
        )

    def forward(self, X, length):
        X = torch.flatten(X, start_dim=1)

        X = self.linear_block1(X)
        X = self.linear_block2(X)
        X = self.linear_block3(X)
        X = self.linear_block_final(X)

        X = X.squeeze()

        return X

#initialize model
model = Weather_Net(embedding_dim=19)


#setup optimizer
optimizer = optim.Adam(params=model.parameters(), lr=0.01)
criterion = nn.MSELoss()

exp_lr_scheduler = optim.lr_scheduler.StepLR(optimizer, step_size=3, gamma=0.1)

#reset the cache
torch.cuda.empty_cache()

if torch.cuda.is_available():
    model = model.cuda()
    criterion = criterion.cuda()

#implement training routine
def train_model(num_epoch):
    model.train()

    for batch_idx, (data, length, target) in enumerate(train_loader):
        #data = data.unsqueeze(1)

        if torch.cuda.is_available():
            data = data.cuda()
            length = length.cuda()
            target = target.cuda()

        optimizer.zero_grad()
        output = model(data, length)
        loss = criterion(output, target)
        loss.backward()
        optimizer.step()
        exp_lr_scheduler.step()

        if (batch_idx + 1) % 10 == 0:
            print('Train Epoch: {} [{}/{} ({:.0f}%)]\tLoss: {:.6f}'.format(
                num_epoch, (batch_idx + 1) * len(data), len(train_loader.dataset),
                           100. * (batch_idx + 1) / len(train_loader), loss.item()))


def evaluate(data_loader):
    global performance
    model.eval()
    loss = 0
    r2 = np.zeros(len(data_loader))

    for batch_idx, (data, length, target) in enumerate(data_loader):
        #data = data.unsqueeze(1)

        if torch.cuda.is_available():
            data = data.cuda()
            length = length.cuda()
            target = target.cuda()

        output = model(data, length)

        loss += F.mse_loss(output, target, reduction='sum').item()

        r2[batch_idx] = r2_score(target.cpu().detach().numpy(), output.cpu().detach().numpy())

    loss /= len(data_loader.dataset)

    r2 = r2.mean()

    performance.append(r2)

    print('\nAverage Val Loss: {:.4f}, Val R2: {:.4f}\n'.format(
        loss, r2))


#do training
num_epochs = 25
global performance
performance = []

for n in range(num_epochs):
    start_round = time.time()
    train_model(n)
    evaluate(val_loader)
    end_round = time.time()
    print("Time needed for Round {}: {}".format(n,(end_round - start_round)))

#save the model
torch.save(model.state_dict(), "model/weather_model.pt")

#create test routine
def make_predictions(data_loader):
    model.eval()
    test_preds = torch.LongTensor()

    for i, (data, length) in enumerate(data_loader):
        #data = data.unsqueeze(1)

        if torch.cuda.is_available():
            data = data.cuda()
            length = length.cuda()

        output = model(data, length)

        preds = output.cpu().data.max(1, keepdim=True)[1]
        test_preds = torch.cat((test_preds, preds), dim=0)

    return test_preds

test_set_preds = make_predictions(test_loader)


r2 = r2_score(test_y, test_set_preds)
print(r2)
mse = mean_squared_error(test_y, test_set_preds)
print(mse)

plt.plot(range(1, len(performance) +1), performance)
plt.ylabel('Accuracy')
plt.xlabel('Epoche')
plt.show()

end = time.time()
print("Time needed for Training: {}".format(end - start))