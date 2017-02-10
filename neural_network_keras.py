import numpy as np
from keras.models import Sequential
from keras.layers import Dense, Activation
from keras.optimizers import SGD, RMSprop

import pandas as pd



### Desnormalizado	
#df = pd.DataFrame.from_csv("dados-Ibov.csv", index_col=False);

#for col in list(df.columns):
#	df[col] = (df[col] - df[col].mean())/df[col].std()


####

#### normalizado
df = pd.DataFrame.from_csv("normalizado-Ibov.csv", index_col=False);


msk = np.random.rand(len(df)) < 0.8
#####



# Use only one feature
X = df['MA(Open)']

# Split the data into training/testing sets
X_train = X[msk]
X_test = X[~msk]

# Split the targets into training/testing sets
y_train = df['Close'][msk]
y_test = df['Close'][~msk]




train = df[msk]

test = df[~msk]


# KERAS
model = Sequential()



model.add(Dense(output_dim=100, input_dim=train.shape[1]))
model.add(Activation("tanh"))
model.add(Dense(output_dim=1))


optim = RMSprop(lr=0.001, rho=0.9, epsilon=1e-08, decay=0.0)

model.compile(loss='mse', optimizer='sgd', metrics=['mse'])

#from keras.optimizers import SGD
#model.compile(loss='categorical_crossentropy', optimizer=SGD(lr=0.01, momentum=0.9, nesterov=True))

# Create linear regression object
#regr = MLPRegressor(hidden_layer_sizes=(100), activation='tanh', solver='lbfgs', early_stopping = True)

# Train the model using the training sets
model.fit(train.as_matrix(), y_train.as_matrix(), validation_split=0.1,  nb_epoch=20, batch_size=32)

loss_and_metrics = model.evaluate(test.as_matrix(), y_test.as_matrix(), batch_size=32)

# The coefficients
print('results: \n', np.sqrt(loss_and_metrics))

