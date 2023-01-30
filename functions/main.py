from varplots import *
import argparse
import os 
import pandas as pd

#

if __name__ == "__main__":
    # Get imput data information 
    parser = argparse.ArgumentParser()
    parser.add_argument("--data", help='link to the datafolder')
    parser.add_argument("--path", help='link to the graphicsfolder')
    parser.add_argument("--N_col", help='number of columnns for graphics')
    parser.add_argument("--figsize", help='list of the size x,y of the graphics')
    parser.add_argument("--category_col", help='list of categorical variables', type=str)

    args = parser.parse_args()


    #Default configuration 
    args.N_col = 3
    args.figsize = [12,15] 
    cat_col = [item for item in args.category_col.split(' ')]
    data = pd.read_csv(filepath_or_buffer = args.data)


    plot_data_variable().plot_variables(data = data,
                            category_col = cat_col)
    

