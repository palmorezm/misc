import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

# Data source (published publicly)
vitals = pd.read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTcFzJXYjKvyhUI3my6VTaqRfWG0-pHldCRvf3nndHPIVMh-C1BqzGvB8P9p-GZZ63fbXdS4i0O8a5C/pub?output=csv")
print(vitals)

nas = vitals[vitals.isnull().any(axis=1)]
print (nas)

vitals[vitals.isna()]

plt.plot(x = Weight, y = Height, data=vitals)

