import numpy as np
import pandas as pd
from sklearn.ensemble import RandomForestClassifier
from sklearn.linear_model import LogisticRegression, LinearRegression, Ridge, Lasso, ElasticNet
from sklearn.tree import DecisionTreeClassifier
from sklearn.svm import SVC
from xgboost import XGBClassifier
from lightgbm import LGBMClassifier
from sklearn.base import BaseEstimator, ClassifierMixin, clone
from sklearn.pipeline import Pipeline
from sklearn.utils.validation import check_X_y, check_array, check_is_fitted
from sklearn.utils.multiclass import unique_labels




class RandomForest():
    """ Random forest classifier model."""

    def __init__(self ,
                 criterion='gini') -> None:
        super().__init__()

        self.criterion = criterion
        self.name = 'RandomForest' 
        self.model = RandomForestClassifier(random_state=0, 
                                            criterion=self.criterion,
                                            class_weight='balanced')

    def get_hyperparameter_space(self):
        hyp = {'model__max_depth': range(2, 4, 1),
                'model__n_estimators': [100, 200, 500]}

        return hyp


class LinRegression():
    """ LinearRegression classifier model."""

    def __init__(self ,
                 criterion='gini') -> None:
        super().__init__()
        
        self.criterion = criterion
        self.name = 'RandomForest' 
        self.model = LinearRegression(random_state=0, 
                                            criterion=self.criterion,
                                            class_weight='balanced')

    def get_hyperparameter_space(self):
        hyp = {'model__max_depth': range(2, 4, 1),
                'model__n_estimators': [100, 200, 500]}

        return hyp


class Elasticnet():
    """ LinearRegression with regularization classifier model. alpha = 0 is linear regression without regularization"""

    def __init__(self ) -> None:
        super().__init__()
 
        self.name = 'ElasticNet' 
        self.model = ElasticNet(random_state=0)

    def get_hyperparameter_space(self):
        hyp = {'model__alpha': [np.arange(0,5,0.5)],
                'model__l1_ratio': [np.arange(0,1,0.05)]}

        return hyp




class XGB():
    """ Xgboost classifier model."""

    def __init__(self,
                 n_estimators=50,
                 learning_rate=0.1,
                 max_depth=3,
                 reg_lambda=0.1,
                 colsample_bytree=1) -> None:
        super().__init__()

        self.n_estimators = n_estimators
        self.learning_rate = learning_rate
        self.max_depth = max_depth
        self.reg_lambda = reg_lambda
        self.colsample_bytree = colsample_bytree
        self.name = 'XGB' 
        self.model = XGBClassifier(random_state=0,
                                   n_estimators=self.n_estimators,
                                   learning_rate=self.learning_rate,
                                   max_depth=self.max_depth,
                                   reg_lambda=self.reg_lambda,
                                   colsample_bytree=self.colsample_bytree)

    def get_hyperparameter_space(self):
        hyp = {
                #'model__n_estimators': [50],
                'model__learning_rate': [0.1, 0.3],
                'model__max_depth': range(2, 4, 1),
                'model__reg_lambda': np.logspace(-4, 1, num=3)
                #'model__colsample_bytree': [0.5, 0.8, 1]
            }

        return hyp


class LightGBM():
    """ Xgboost classifier model."""

    def __init__(self,
                 n_estimators=50,
                 learning_rate=0.1,
                 max_depth=3,
                 reg_lambda=0.1,
                 reg_alpha=0.001) -> None:
        super().__init__()

        self.n_estimators = n_estimators
        self.learning_rate = learning_rate
        self.max_depth = max_depth
        self.reg_lambda = reg_lambda
        self.reg_alpha = reg_alpha
        self.name = 'LightGBM' 
        self.model = LGBMClassifier(random_state=0,
                                    n_estimators=self.n_estimators,
                                    learning_rate=self.learning_rate,
                                    max_depth=self.max_depth,
                                    reg_lambda=self.reg_lambda,
                                    reg_alpha=self.reg_alpha,
                                    class_weight='balanced')

    def get_hyperparameter_space(self):
        hyp = {
                #'model__n_estimators': [10, 20, 50],
                'model__learning_rate': [0.01, 0.1, 1],
                'model__max_depth': range(2, 4, 1),
                #'subsample': [0.5, 0.8],
                #'model__reg_lambda': np.logspace(-3, 2, num=3),
                #'model__reg_alpha': np.logspace(-4, 2, num=3)
            }

        return hyp


class LogisticReg():
    """ Logistic regression classifier model."""

    def __init__(self,
                 C=1.0) -> None:
        super().__init__()

        self.C = C
        self.name = 'LogisticRegression' 
        self.model = LogisticRegression(random_state=0,
                                        class_weight='balanced',
                                        solver='liblinear',
                                        penalty='l2',
                                        C=self.C)

    def get_hyperparameter_space(self):
        #hyp = {'model__C': np.logspace(-3, 1, num=3)}
        hyp = {'model__C': [0.001, 0.01, 0.1, 1]}

        return hyp


class DecisionTree():
    """ Decision tree classifier model."""

    def __init__(self,
                 max_depth=3) -> None:
        super().__init__()

        self.max_depth = max_depth
        self.name = 'DecisionTree' 
        self.model = DecisionTreeClassifier(random_state=0,
                                            class_weight='balanced',
                                            max_depth=self.max_depth)

    def get_hyperparameter_space(self):
        hyp = {'model__max_depth': range(2, 5, 1)}

        return hyp


class LinearSVM():
    """LinearSVM classifier model."""

    def __init__(self,
                 C=1.0,
                 max_iter=1000) -> None:
        super().__init__()

        self.C = C
        self.max_iter = max_iter
        self.name = 'LinearSVM' 
        self.model = SVC(random_state=0,
                         kernel='linear',
                         C=self.C,
                         max_iter=self.max_iter,
                         class_weight='balanced',
                         probability=True)

    def get_hyperparameter_space(self):
        hyp = {'model__C': [0.001, 0.01, 0.1, 1]}

        return hyp


