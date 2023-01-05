
print("pyprep.py was successfully run")

import matplotlib.pyplot as plt 
import pandas as pd
import numpy as np

x = [1, 2, 2, 2, 2, 6, 8, 8, 8, 9, 9, 10]
bins = 10
plt.hist(x, bins)
plt.show()

# d2021 = pd.read_excel("2021.xlsx")
# d2022 = pd.read_excel("2022.xlsx")
