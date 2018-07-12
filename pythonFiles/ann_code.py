import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
import sklearn.preprocessing as sk

from numpy.random import seed
seed(1)

# import dataset
dataset = pd.read_csv('./diagSet_quasiTS_balanced.csv')
X = dataset.iloc[:, 0:19].values
y = dataset.iloc[:, 19].values

# generate PolynomialFeatures from numerical values
Xpoly = dataset.iloc[:, 3:19].values
poly = sk.PolynomialFeatures(interaction_only=True)
Xpoly_derived = poly.fit_transform(Xpoly)

# enocde categorical data
from sklearn.preprocessing import LabelEncoder, OneHotEncoder
labelencoder_X_1 = LabelEncoder()
X[:, 1] = labelencoder_X_1.fit_transform(X[:, 1])
labelencoder_X_2 = LabelEncoder()
X[:, 2] = labelencoder_X_2.fit_transform(X[:, 2])
onehotencoder = OneHotEncoder(categorical_features = [1])
X = onehotencoder.fit_transform(X).toarray()
X = X[:, 1:]

Xtrunc = X[:, 0:23] # remove the simple numerical columns

# concatenate the final X array
Xexp = np.concatenate((Xtrunc, Xpoly_derived, ), axis = 1)

X = Xexp

# split
from sklearn.model_selection import train_test_split
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=1)
X_train, X_val, y_train, y_val = train_test_split(X_train, y_train, test_size=0.2, random_state=1)


# X_train, X_test, y_train, y_test = train_test_split(X, y, test_size = 0.2, random_state = 0)

# feature scaling
from sklearn.preprocessing import StandardScaler
sc = StandardScaler()
X_train = sc.fit_transform(X_train)
X_val = sc.fit_transform(X_val)
X_test = sc.transform(X_test)

# ANN
# import keras libraries
import keras
from keras.models import Sequential
from keras.layers import Dense
from keras.layers import Dropout


# ann

dense_node_n = 512
ann_dropout = 0.8

n_epochs = 20
n_batch_size = 128

import numpy as np
from keras.models import Sequential
from keras.layers import Dense, Dropout

model = Sequential()
model.add(Dense(dense_node_n, input_dim=160, activation='relu'))
model.add(Dropout(ann_dropout))
model.add(Dense(dense_node_n, activation='relu'))
model.add(Dropout(ann_dropout))
model.add(Dense(dense_node_n, activation='relu'))
model.add(Dropout(ann_dropout))
model.add(Dense(1, activation='sigmoid'))

model.compile(loss='binary_crossentropy', optimizer='adam', metrics=['accuracy'])

history = model.fit(X_train, y_train, epochs=n_epochs, batch_size=n_batch_size, validation_data = (X_val, y_val))

# score = model.evaluate(X_test, y_test, batch_size=128)
y_pred_asNumber = model.predict(X_test)


# plot losses
import matplotlib.pyplot as plt
loss = history.history['loss']
val_loss = history.history['val_loss']
#acc = history.history['acc']
#val_acc = history.history['val_acc']
epochs = range(1, len(loss) + 1)

plt.plot(epochs, loss, 'bo', label = 'Training loss')
plt.plot(epochs, val_loss, 'b', label = 'Validation loss')
plt.legend()
plt.savefig('loss_valLoss.png', dpi = 300)
plt.clf()

# plot ROC
from sklearn import metrics
import matplotlib.pyplot as plt

fpr, tpr, _ = metrics.roc_curve(y_test, y_pred_asNumber)

fpr = fpr # false_positive_rate
tpr = tpr # true_positive_rate

# This is the ROC curve
plt.plot(fpr,tpr)
# plt.show()
plt.savefig('roc_mortality.png')
plt.clf()
auc = np.trapz(tpr, fpr)

print(auc)
