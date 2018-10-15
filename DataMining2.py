
import pandas as pd
import os
from sklearn import tree
from sklearn.model_selection import train_test_split, RandomizedSearchCV, GridSearchCV
from sklearn.preprocessing import LabelEncoder
#import graphviz
from time import time
from scipy.stats import randint as sp_randint
from sklearn.metrics import classification_report,confusion_matrix


#os.chdir('C:\\Users\\johnb\\OneDrive\\Documents\\MSA\\Fall 2\\Data Mining\\HW2')
#os.environ["PATH"] += os.pathsep + 'C:\\Users\\johnb\\Anaconda3\\Library\\bin\\graphviz\\'
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

#graph = graphviz.Source(dot_data)
#graph.render('tree')



##################Randomized CV##################


#start = time()
#params = {'criterion':['gini', 'entropy'],
          #'max_depth':sp_randint(13,15),
          #'max_features':sp_randint(14, 15),
          #'min_samples_leaf':sp_randint(80, 120),
          #'min_samples_split':sp_randint(2,20)}

#random_search = RandomizedSearchCV(clf, param_distributions=params,
                                   #cv=5, n_iter = 200)

#random_search.fit(X_train, y_train)
#print(time() - start)
#cv_res = pd.DataFrame(random_search.cv_results_).sort_values(by=['rank_test_score'])

#print (cv_res.columns.values)

##print(cv_res[['rank_test_score','mean_test_score']].head(10))

#print(cv_res[['param_max_depth', 'param_criterion', 'param_max_features']].head(10))

#print(cv_res[['param_min_samples_leaf','param_min_samples_split',]].head(10))

#cv_score = random_search.score(X_test, y_test)
#pred = random_search.predict(X_test)
#print(cv_score)

#print(confusion_matrix(y_test,pred))
#print(classification_report(y_test,pred))


###########Scoring Our Best Model##################

bestdtc = tree.DecisionTreeClassifier(max_depth=14, criterion = 'entropy', max_features=14, min_samples_leaf= 96, min_samples_split=18)


fit = bestdtc.fit(X_train, y_train)
train_score = bestdtc.score(X_train, y_train)
test_score = bestdtc.score(X_test, y_test)

pred = bestdtc.predict(X_test)
print(train_score, test_score)

print(confusion_matrix(y_test,pred))
print(classification_report(y_test,pred))