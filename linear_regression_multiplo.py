import numpy as np
from sklearn import datasets, linear_model
import pandas as pd

# Load the diabetes dataset
	
df = pd.DataFrame.from_csv("previsao-erro-normalizado-NN-Ibov.csv", index_col=False);

msk = np.random.rand(len(df)) < 0.8



# Use only one feature
diabetes_X = df['MA(Open)']

# Split the data into training/testing sets
diabetes_X_train = diabetes_X[msk]
diabetes_X_test = diabetes_X[~msk]

# Split the targets into training/testing sets
diabetes_y_train = df['Close'][msk]
diabetes_y_test = df['Close'][~msk]


df = df.drop(df.columns[[0, -1,-2]], axis=1)

train = df[msk]

test = df[~msk]

# Create linear regression object
regr = linear_model.LinearRegression()

# Train the model using the training sets
regr.fit(train, diabetes_y_train.reshape(-1,1))

# The coefficients
print('Coefficients: \n', regr.coef_)
# The mean squared error
print("Mean squared error: %.2f"
      % np.mean((regr.predict(test) - diabetes_y_test.reshape(-1,1)) ** 2))
# The root mean squared error
print("Root Mean squared error: %.2f"
      % np.sqrt(np.mean((regr.predict(test) - diabetes_y_test.reshape(-1,1)) ** 2)))
# Explained variance score: 1 is perfect prediction
print('Variance score: %.2f' % regr.score(test, diabetes_y_test.reshape(-1,1)))
