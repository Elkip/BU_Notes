import csv
import numpy as np
from numpy.linalg import inv
import pandas as pd


# INPUT:
# data_file - Input dataset location
# out_var - Outcome variable
# num_pred - Number of predictors allowed to vary
# OUTPUT:
# - Parameter Estimates for predictors
# - Standard errors for all predictors
# - R-squared goodness of fit measure
def linreg(data_file, out_var, num_pred):
    # Load the data and extract headers
    with open(data_file) as csv_file:
        data = csv.reader(csv_file, delimiter=',')
        dat_df = pd.DataFrame(data)
        try:
            dat_df.rename(columns=dat_df.loc[0].str.strip(), inplace=True)
            dat_df.drop(0, inplace=True)
            dat_df = dat_df.apply(pd.to_numeric)
        except:
            print("The data is formatted incorrectly.\n "
                  "The file should contain a list of comma seperated numbers with the first line being column headers.")
            exit(1)

    # Extract the predictor column
    if out_var in dat_df.columns:
        y_df = dat_df[out_var]
        dat_df = dat_df.drop(columns=out_var)
    else:
        print("The outcome variable you are searching for is not present.")
        exit(1)

    # Estimation of Predictors
    # ^beta = (X'X)^-1 * X' Y
    var = inv(dat_df.T.dot(dat_df))
    var2 = var.dot(dat_df.T)
    beta = var2.dot(y_df)
    print(beta)

    # Standard error of predictors
    # RSS = Y'Y - β'X'Y
    # s^2 = RSS/(n−p−1)
    # var(^β) = s^2 * (X'X)^-1
    rss = y_df.dot(y_df.T) - beta.T.dot(dat_df.T).dot(y_df)
    s2 = rss / (y_df.size - dat_df.columns.size - 1)
    var_beta = s2 * inv(dat_df.T.dot(dat_df))
    # print(var_beta)

    # R-Sqaured GoF Measure
    # R^2 = RSS / SYY
    # (Y'*Y - Beta'X'Y) / (Y'Y - n*Y_hat^2)

    # R-Sqaured Adjusted
    # RSS / (n - p - 1) / (SYY / (n-1))


def main():
    print("803 REGRESSION APPLICATION")
    print("Instructions:\n1. Specify the input file (should be a csv with headers)\n")
    print("2. Give the outcome variable (by column name)\n")
    print("3. Input the maximum number of variables that may be removed in the model\n")
    linreg("/home/elkip/Datasets/Wine.csv", "Alcohol", 0)


if __name__ == "__main__":
    main()
