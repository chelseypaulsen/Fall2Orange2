# -*- coding: utf-8 -*-
"""
Created on Thu Oct 11 13:06:24 2018

@author: johnb
"""

import pandas as pd
import os
from sklearn import tree
from sklearn.model_selection import train_test_split, RandomizedSearchCV, GridSearchCV
from sklearn.preprocessing import LabelEncoder
import graphviz
from time import time

os.chdir('C:\\Users\\johnb\\OneDrive\\Documents\\MSA\\Fall 2\\Data Mining\\HW2')
os.environ["PATH"] += os.pathsep + 'C:\\Users\\johnb\\Anaconda3\\Library\\bin\\graphviz\\'
file = 'FFTSales.xls'

sales = pd.read_excel(file)
features = sales.drop('y', axis=1)
# Select only the numeric variables (sklearn doesn't take strings)
numeric_features = features.select_dtypes(exclude='object')
# Below uses thier label encoding to turn the categorical variables into
# numeric labels
le = LabelEncoder()
for col in features.columns:
    if features[col].dtype == 'O':
        features[col] = le.fit_transform(features[col])
        
boo = {'yes':1, 'no':0}

target = sales['y'].map(boo)
target.mean()

X_train, X_test, y_train, y_test = train_test_split(features, target,
                                                    test_size=0.1,
                                                    random_state=42)




# Fit the classifier
clf = tree.DecisionTreeClassifier()

clf = clf.fit(X_train, y_train)

dot_data = tree.export_graphviz(clf, out_file=None, feature_names=features.columns)

graph = graphviz.Source(dot_data)
graph.render('tree')

train_score = clf.score(X_train, y_train)
test_score = clf.score(X_test, y_test)

print(train_score, test_score)

start = time()
params = {'criterion':['gini','entropy'],
          'max_depth':range(2,8),
          'min_samples_leaf':[5,15,25,35,45,55],
          'splitter':['best','random']}

random_search = GridSearchCV(clf, param_grid=params,
                                   cv=5)

random_search.fit(X_train, y_train)
print(time() - start)
cv_res = pd.DataFrame(random_search.cv_results_).sort_values(by=['rank_test_score'])

print(cv_res[['rank_test_score','mean_test_score']].head(10))

cv_score = random_search.score(X_test, y_test)
print(cv_score)


