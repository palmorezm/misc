
import pandas as pd 

df = pd.read_csv("https://raw.githubusercontent.com/palmorezm/msds/main/Business%20Analytics%20and%20Data%20Mining/HW2/classification-output-data.csv")
df.head()

def precision(df):
    tp = sum(df['class'] == 1 & df['scored.class'] == 1)
    fp = sum(df['class'] == 0 & df['scored.class'] == 1)
    return((tp/(tp + fp)))

precision(df)