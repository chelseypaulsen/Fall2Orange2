# -*- coding: utf-8 -*-
"""
Created on Thu Oct 11 13:06:24 2018

@author: johnb
"""

import pandas as pd
import os
from sklearn import tree
from sklearn.model_selection import train_test_split
#import graphviz

os.chdir('C:\\Users\\johnb\\OneDrive\\Documents\\MSA\\Fall 2\\Data Mining\\HW2')

file = 'FFTSales.xls'

sales = pd.read_excel(file)
features = sales.drop('y', axis=1)
target = sales['y']

X_train, X_test, y_train, y_test = train_test_split(features, target,
                                                    test_size=0.3,
                                                    random_state=42)

boo = {'yes':1, 'no':0}

targetbool = target.map(boo)
# Fit the classifier
clf = tree.DecisionTreeClassifier()

clf = clf.fit(features, targetbool)

dot_data = tree.export(clf, out_file=None, feature_names=features.columns.values,
                       class_names=target.values, filled=True,
                       rounded=True, special_characters=True)

graph = graphviz.Source(dot_data)
graph.render('tree')