import numpy as np
import matplotlib.pyplot as plt 
import os 
import seaborn as sns 
import pandas as pd 


class plot_data_variable():
    """ Class to plots density / bar plots """
    def __init__(self, 
                objective: str = 'NS') -> None:
        """ objective : survival : S or non survival : NS""" 
        self.objective = objective 
    

    def plot_variables(self, 
                        data: pd.DataFrame,
                        path: str = r"C:\Users\mehdy\Desktop\Work\KAGGLE\Mobile-price-classiffication\graphics",
                        N_col: int = 3,
                        figsize: list = [10,15],
                        category_col: list = None,
                        y:str = None): 
        """" Plot density plot for continuous variables and barplot for categorical variables
        data : dataframe
        N_col : Number of columns for subplots 
        figsize : list of x,y size for the entire plot
        path : the path where to save plots 
        y : if you want hue for plots """

        self.data = data
        self.y = y
        self.path = path

        self.data[category_col] = self.data[category_col].astype('category')

        # checking if the directory demo_folder2 
        # exist or not.
        if not os.path.isdir(self.path):
            # if the demo_folder2 directory is 
            # not present then create it.
            os.makedirs(self.path)
            #save plot in the new folder 
        N_features = len(self.data.columns)
        # calculate of number of rows dependant of number of features 
        if N_features%N_col == 0:
            n_rows = np.int64(N_features/N_col)
        else:
            n_rows = np.int64(N_features/N_col) + 1

        # plot recursivly according to the type of variables
        fig, ax = plt.subplots(nrows=n_rows, ncols=N_col, figsize = (figsize[0],figsize[1]))
        fig.tight_layout(pad=5.0)
        ind_row = 0
        ind_col = 0
        for i in self.data.columns:
            if N_col == ind_col:
                ind_col = 0 
                ind_row += 1 
            if i in self.data.select_dtypes('category').columns.tolist():
                g = sns.countplot(data= self.data, x=str(i), ax=ax[ind_row][ind_col], hue=y)
                ind_col += 1  
            elif i in self.data.select_dtypes(exclude='category').columns.tolist():
                g = sns.kdeplot(data = self.data, x=str(i), hue=y, ax=ax[ind_row][ind_col], multiple="stack")
                ind_col +=1
        
        
        title = "Density plots and barplots of variables.png"
        plt.suptitle(str(title))
        plt.savefig(os.path.join(self.path, title))

