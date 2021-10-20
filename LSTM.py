# File for Long-Short Term Model forecasting of Time-Series Data

import numpy as np
import scipy.linalg as la
import pandas as pd
import matplotlib.pyplot as plt

# import from keras and sklearn
from sklearn.preprocessing import MinMaxScaler

from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import Dense
from tensorflow.keras.layers import LSTM
from tensorflow.keras.layers import Dropout



### read and transform data

df = pd.read_csv('data/CBBTCUSD.csv')
# remove earlier dates than 2015-01-19 (as they contain lots of NA)
df = df.loc[df.DATE >= '2015-01-19', :]
# fix NAN at "2020-09-04"
df.loc[df.DATE == '2020-09-04', 'CBBTCUSD'] = '10493.77'
# rename columns
df = df.rename(columns={'DATE': 'date', 'CBBTCUSD': 'BTC'})
# convert to correct datatypes
df.date = pd.to_datetime(df.date)
df.BTC = pd.to_numeric(df.BTC)
# fix so index represents days after bitcoins creation (2009-01-03)
df.index = df.index + 2158
# transform the data using log-transformation (Box-Cox with lambda=0)
df.loc[:,'logBTC'] = np.log(df.BTC)



### remove trend component, assume linear trend m(t) = beta0 + beta1 * t

# design matrix and response (y = X beta + eps)
X_matr = np.array([np.ones(df.shape[0]), df.index]).T
y_vec = np.array(df.logBTC)
# MLE estimate for beta (beta = (X^T X)^(-1) X^T y)
beta0, beta1 = la.solve(X_matr.T @ X_matr, X_matr.T @ y_vec)
# trend component
df.loc[:,'m'] = beta0 + beta1 * df.index
def trend(x):
  return beta0 + beta1 * x
df.loc[:,'Y'] = df.logBTC - df.m



### plot the data with trend component

#plt.plot(df.logBTC)
#plt.plot(df.m)
#plt.show()



### split into training and test data set (use last 365 days for test)

#split_idx = np.shape(df)[0] - 365
#df_train = df[:split_idx]
#df_test = df[split_idx:]



### create Long Short-Term model
def split_sequence(seq, n_steps):
    X = []
    y = []
    for i in range(len(seq)):
        # find the end of this pattern
        end_idx = i + n_steps
        # check if we are beyond the sequence
        if end_idx > len(seq)-1:
            break
        # gather input and output parts of the pattern
        seq_x, seq_y = seq[i:end_idx], seq[end_idx]
        X.append(seq_x)
        y.append(seq_y)
	
    return np.array(X), np.array(y)
 

def predict_lstm_hstep(model, seq, h=1):
    '''
    Predicting h steps into the future
    '''
    values = np.zeros(n_steps + h)
    values[:n_steps] = seq[-n_steps:] # set first n_steps elements
    for i in range(h):
        x_input = values[i : i+n_steps].reshape((1, n_steps, n_features))
        values[n_steps+i] = model.predict(x_input, verbose=0)
        
    prediction = values[-h:]
    
    return prediction




# Arcitecture
n_steps    = 100              # choose a number of inputs nodes
n_lstm     = 20               # choose the number of LSTM nodes
n_features = 1                # choose number of outputs nodes
n_epochs   = 20               # number of training epochs
h          = 365              # forecast one year forwards
n_models   = 5                # number og models fitted

# create training data
raw_seq = np.array(df.Y)
# define input time series; split into samples
X, y = split_sequence(raw_seq, n_steps)
# reshape from [samples, timesteps] into [samples, timesteps, features]
X = X.reshape((X.shape[0], X.shape[1], n_features))

models = []
predictions = []

for i in range(5):
    # define model
    model = Sequential()
    model.add(LSTM(n_lstm, activation='relu', input_shape=(n_steps, n_features)))
    model.add(Dense(1))
    model.compile(optimizer='adam', loss='mse')
    
    # fit model
    model.fit(X, y, epochs=n_epochs, verbose=0)
    model.summary()
    
    # predict one year forward
    prediction = predict_lstm_hstep(model, raw_seq, h)
    
    # add to list
    models.append(model)
    predictions.append(prediction)



plt.figure(figsize=(8, 6), dpi=80)

# plot prediction with the training input
plt.plot(df.index - 2158, df.logBTC - df.m, color='black', linewidth=1)
for model in models:
    plt.plot(df.index[n_steps:] - 2158, model.predict(X), color='r--', linewidth=1)
plt.xlabel('t')
plt.ylabel('Y')
plt.savefig('LSTM_Ys.pdf')
plt.show()

pred_plot_index = range(df.index[-1]+1 - 2158, df.index[-1]+1+h - 2158)
pred_index = range(df.index[-1]+1, df.index[-1]+1+h)

plt.plot(df.index - 2158, df.logBTC, color='black', linewidth=1)
for prediction in predictions:
    plt.plot(pred_plot_index, trend(pred_index) + prediction, color='red', linewidth=1)
plt.plot()
plt.xlabel('t')
plt.ylabel('log(USD)')
plt.savefig('LSTM_predictions.pdf')
plt.show()


