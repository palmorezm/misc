# -*- coding: utf-8 -*-


# Data Visual
# Hire Rate Example
# From The_Little_Grey_Book_of_Recruiting_Benchmarks_2016.pdf

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns

df = pd.DataFrame({'Size':['<100', '101-10,000', '10,001+'], 'Applicants':[94,98,129]})

sns.barplot(x = 'Size', y = 'Applicants', data = df)