import csv
import numpy as np
import pandas as pd


# INPUT:
# data_file - Input dataset location (see instructions for format)
# out_var - Outcome variable
# num_pred - Number of predictors allowed to vary
# OUTPUT:
# - Parameter Estimates for predictors
# - Standard errors for all predictors
# - R-squared goodness of fit measure
def linreg(data_file, out_var, pred):
    float_formatter = "{:.4f}".format
    np.set_printoptions(formatter={'float_kind': float_formatter})
    pd.options.display.float_format = float_formatter
    # Load the data and extract headers
    with open(data_file) as csv_file:
        try:
            data = csv.reader(csv_file, delimiter=',')
            dat_df = pd.DataFrame(data)
            # Remove next 2 lines out if the first row is not labels
            dat_df.rename(columns=dat_df.loc[0].str.strip(), inplace=True)
            x_df = dat_df.drop(0)
            # Extract the predictor column
            x_df = x_df.apply(pd.to_numeric)
            if out_var in x_df.columns:
                y_df = x_df[out_var]
                x_df = x_df.drop(columns=out_var)
                # If using a subset of columns
                if pred.strip() != '':
                    sub = [s.strip() for s in pred.split(",")]
                    x_df = x_df[sub]
            else:
                print("The outcome variable you are searching for is not present.")
                exit(1)
            # Add a column of 1's and convert to numbers
            x_df.insert(0, "Intercept", np.ones((y_df.size, 1)))
        except FileNotFoundError as f:
            print("The requested file does not exist, check you filepath and try again")
            print(str(f))
            exit(1)
        except KeyError as k:
            print("The column list passed is not valid")
            print(str(k))
            exit(1)
        except Exception as e:
            print("The data is formatted incorrectly")
            print(type(e), str(e))
            exit(1)

    # Estimation of Predictors
    # ^beta = (X'X)^-1 * X' Y
    var = np.linalg.inv(x_df.T.dot(x_df))
    var2 = var.dot(x_df.T)
    beta = var2.dot(y_df)

    # Standard error of predictors
    # RSS = Y'Y - β'X'Y
    # s^2 = RSS/(n−p−1)
    # var(^β) = s^2 * (X'X)^-1
    rss = y_df.dot(y_df.T) - beta.T.dot(x_df.T).dot(y_df)
    s2 = rss / (y_df.size - x_df.columns.size - 1)
    var_beta = np.diagonal(s2 * np.linalg.inv(x_df.T.dot(x_df)))
    sd_beta = var_beta ** (1 / 2)

    linreg_df = pd.DataFrame(list(zip(beta, var_beta, sd_beta)), columns=["Est Beta", "Var of Beta", "SD of Beta"],
                             index=x_df.columns)
    print(linreg_df)

    # R-Squared GoF Measure
    # R^2 = 1 - RSS / SYY
    # 1 - (Y'*Y - Beta' X'Y) / (Y'Y - n*Y_hat^2)
    syy = (y_df.T.dot(y_df) - y_df.size * y_df.mean() ** 2)
    r_sq = 1 - (rss / syy)
    print(f'r-squared: {r_sq:.3f}')

    # R-Squared Adjusted
    # 1 - RSS / (n - p - 1) / (SYY / (n-1))
    r_sq_adj = 1 - (rss / (y_df.size - x_df.columns.size - 1)) / (syy / (y_df.size - 1))
    print(f'r-squared adjusted: {r_sq_adj:.3f}')


def main():
    print("803 REGRESSION APPLICATION\nPlease read the documentation!")
    file_loc = input("1.Specify the path of input file:\n")
    out_var = input("2. Give the outcome variable by column name:\n")
    max_drop = input("3. Specify the predictors to use by column name seperated by a comma, "
                     "or leave blank to use every column:\n")
    # linreg("/home/elkip/Datasets/Wine.csv", "Alcohol", "Hue, Ash" )
    linreg(file_loc, out_var, max_drop)


if __name__ == "__main__":
    main()
