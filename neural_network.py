import numpy as np
from sklearn.neural_network import MLPRegressor
import pandas as pd



### Desnormalizado	
#df = pd.DataFrame.from_csv("dados-Ibov.csv", index_col=False);

#for col in list(df.columns):
#	df[col] = (df[col] - df[col].mean())/df[col].std()


####

#### normalizado
df = pd.DataFrame.from_csv("normalizado-Ibov.csv", index_col=False);
#df = pd.DataFrame.from_csv("normalizados-Dolar.csv", index_col=False);


msk = np.random.rand(len(df)) < 0.8
#####

#df = df.drop(df.columns[[0, -1,-2]], axis=1)

# Use only one feature
X = df[['Close(-1)', 'Volume(-1)']]

# Split the data into training/testing sets
X_train = X[msk]
X_test = X[~msk]

# Split the targets into training/testing sets
y_train = df['return_diif_last_day'][msk]
y_test = df['return_diff_last_day'][~msk]




train = df[msk]

test = df[~msk]

# Create linear regression object
regr = MLPRegressor(hidden_layer_sizes=(4), activation='tanh', solver='lbfgs', early_stopping = True)

# Train the model using the training sets
regr.fit(X_train, y_train)

# The coefficients
print('Coefficients: \n', regr.coefs_)
# The mean squared error
print("Mean squared error: %.2f"
      % np.mean((regr.predict(X_test) - y_test) ** 2))
# The root mean squared error
print("Root Mean squared error: %.2f"
      % np.sqrt(np.mean((regr.predict(X_test) - y_test) ** 2)))
# Explained variance score: 1 is perfect prediction
print('Variance score: %.2f' % regr.score(X_test, y_test))
