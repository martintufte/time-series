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

print(df.index[0])

### remove trend component, assume linear trend m(t) = beta0 + beta1 * t

# design matrix and response (y = X beta + eps)
X = np.array([np.ones(df.shape[0]), df.index]).T
y = np.array(df.logBTC)
# MLE estimate for beta (beta = (X^T X)^(-1) X^T y)
beta0, beta1 = la.solve(X.T@X, X.T@y)
# trend component
df.loc[:,'logTrend'] = beta0 + beta1 * df.index
def trend(x): return beta0 + beta1 * x


### plot the data with trend component
plt.plot(df.logBTC)
plt.plot(df.logTrend)
plt.show()

### scale data and reshape to use in LSTM?

#scaler = MinMaxScaler(feature_range = (-1, 1)).fit(train.CBBTCUSD)
#train.CBBTCUSD = scaler.transform(train.CBBTCUSD)
#test.CBBTCUSD = scaler.transform(test.CBBTCUSD)




### define Long-Short Term model

# split a univariate sequence
def split_sequence(sequence, n_steps):
	X, y = list(), list()
	for i in range(len(sequence)):
		# find the end of this pattern
		end_idx = i + n_steps
		# check if we are beyond the sequence
		if end_idx > len(sequence)-1:
			break
		# gather input and output parts of the pattern
		seq_x, seq_y = sequence[i:end_idx], sequence[end_idx]
		X.append(seq_x)
		y.append(seq_y)
	return np.array(X), np.array(y)
 
# define input sequence
raw_seq = np.array(df.logBTC - df.logTrend)

# choose a number of time steps
n_steps = 100

# split into samples
X, y = split_sequence(raw_seq, n_steps)

# reshape from [samples, timesteps] into [samples, timesteps, features]
n_features = 1
X = X.reshape((X.shape[0], X.shape[1], n_features))

# define model
model = Sequential()
model.add(LSTM(20, activation='relu', input_shape=(n_steps, n_features)))
model.add(Dense(1))
model.compile(optimizer='adam', loss='mse')

# fit model
model.fit(X, y, epochs=20, verbose=0)
model.summary()

plt.plot(df.index, df.logBTC - df.logTrend)
plt.plot(df.index[n_steps:], model.predict(X))
plt.show()


def predict_lstm_hstep(model, seq, h=1):
    values = np.zeros(n_steps + h)
    values[:n_steps] = seq[-n_steps:] # set first n_steps elements
    for i in range(h):
        x_input = values[i : i+n_steps].reshape((1, n_steps, n_features))
        values[n_steps+i] = model.predict(x_input, verbose=0)
        
    prediction = values[-h:]
    
    return prediction


### plot prediction
h = 365

prediction = predict_lstm_hstep(model, raw_seq, h)
p_index = range(df.index[-1]+1, df.index[-1]+1+h)

plt.plot(df.index, df.logBTC)
plt.plot(p_index, trend(p_index) + prediction)
plt.show()


h = 1000

prediction = predict_lstm_hstep(model, raw_seq, h)
p_index = range(df.index[-1]+1, df.index[-1]+1+h)

plt.plot(df.index, df.logBTC)
plt.plot(p_index, trend(p_index) + prediction)
plt.show()




#'''
