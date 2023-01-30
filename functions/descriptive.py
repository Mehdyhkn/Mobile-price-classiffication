import numpy as np
import seaborn as sns 
import matplotlib.pyplot as plt 
import pandas as pd 

# class to plot missing values, convert types of variables 

class Descriptive_analysis():
  """ 
    Class to compute all types of descriptives analysis steps 
  """
  def __init__(self,
              objective:str = 'survival') -> None:
    self.objective = objective

  def missing_values(self,
                  X:pd.DataFrame):
    """ Compute missing values df """

    self.X = X

    #compute df of missing values sorted 
    self.miss = pd.DataFrame(X.isnull().sum())
    self.miss.columns = ['Nans']
    self.miss = self.miss.sort_values( by= ['Nans'], ascending= False)
    self.miss = self.miss[self.miss.Nans != 0]
    self.miss.reset_index(inplace=True)
    self.miss = self.miss.rename(columns = {'index': 'Variables'})
    if len(self.miss) == 0:
        print('There is no missing values')
    else :
        return self.miss
  

  def plot_missing_values(self, X:pd.DataFrame,
                              fig_size:list=[20,7],
                              size_police: int= 10,
                              threshold:float =None):
    """" Plot missing values bar"""
    self.X = X 
    self.fig_size = fig_size
    self.size_police = size_police
    self.threshold = threshold

    if self.threshold == None:
      self.threshold = np.round(X.shape[0]/2, 2)

    #compute df for missing values
    self.miss = self.missing_values(self.X)

    #ploting features

    plt.figure(figsize=(self.fig_size[0], self.fig_size[1]))
    g = sns.barplot(x="Variables", y="Nans", data=self.miss[self.miss.Nans > self.threshold])
    total = len(self.X)
    for p in g.patches:
      percentage = '{:.1f}%'.format(100 * p.get_height()/total)
      x = p.get_x() + p.get_width() / 2 - 0.05
      y = p.get_y() + p.get_height()
      g.annotate(percentage, (x, y), size = 10)
    plt.title('Variables with more than 50% missing values')
    plt.show()


  def define_dtypes(self, X:pd.DataFrame, 
                          category_columns:list=None,
                          str_columns:list=None,
                          date_columns:list=None,
                          format_date:str=None,
                          num_columns:str=None):
    """ Convert variables """
    self.X = X
    self.category_columns = category_columns
    self.str_columns = str_columns
    self.date_columns = date_columns
    self.format_date = format_date
    self.num_columns = num_columns

    self.X[self.category_columns] = self.X[self.category_columns].astype('category')
    self.X[self.str_columns] = self.X[self.str_columns].astype('str')
    # date convertion
    for date_c in self.date_columns:
      self.X[date_c] = pd.to_datetime(self.X[date_c], format=self.format_date , errors='coerce')
    if self.num_columns != None : 
      self.X[self.num_columns] = self.X[self.num_columns].astype('float')
    else : 
      list_other_variables = list(set(self.X.columns)- set(self.category_columns))
      list_other_variables = list(set(list_other_variables)- set(self.str_columns))
      list_other_variables = list(set(list_other_variables)- set(self.date_columns))
      self.X[list_other_variables] = self.X[list_other_variables].astype('float')
    
    return self.X