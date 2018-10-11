# -*- coding: utf-8 -*-
"""
Created on Thu Oct 11 13:06:24 2018

@author: johnb
"""

import pandas as pd
import os
from sklearn import tree
from sklearn.model_selection import train_test_split
import graphviz

os.chdir('C:\\Users\\johnb\\OneDrive\\Documents\\MSA\\Fall 2\\Data Mining\\HW2')
os.environ["PATH"] += os.pathsep + 'C:\\Users\\johnb\\Anaconda3\\Library\\bin\\graphviz\\'
file = 'FFTSales.xls'

sales = pd.read_excel(file)
features = sales.drop('y', axis=1)
numeric_features = features.select_dtypes(exclude='object')

boo = {'yes':1, 'no':0}

target = sales['y'].map(boo)

X_train, X_test, y_train, y_test = train_test_split(numeric_features, target,
                                                    test_size=0.3,
                                                    random_state=42)




# Fit the classifier
clf = tree.DecisionTreeClassifier(max_depth=4)

clf = clf.fit(X_train, y_train)

dot_data = tree.export_graphviz(clf, out_file=None)

graph = graphviz.Source(dot_data)
graph.render('tree')

train_score = clf.score(X_train, y_train)
test_score = clf.score(X_test, y_test)

print(train_score, test_score)