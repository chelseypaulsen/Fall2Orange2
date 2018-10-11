# -*- coding: utf-8 -*-
"""
Created on Thu Oct 11 13:06:24 2018

@author: johnb
"""

import pandas as pd
import os

os.chdir('C:\Users\johnb\OneDrive\Documents\MSA\Fall 2\Data Mining\HW2')

file = 'FFTSales.xls'

sales = pd.read_excel(file)

