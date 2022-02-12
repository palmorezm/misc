
import pandas as pd
import numpy as np 

features = [0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 0, 1, 0, 1, 1, 1, 1]
labels = [0, 1, 1, 1, 1, 0, 0, 1, 1, 0, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 1, 0]

def likelihood_estimate(features, labels):
  f0 = 0.0
  f1 = 0.0
  y = 0.0
  
# Update accordingly:
  for i in labels:
    if i == 1:
      y = y + 1

  for i in labels:
    if ((labels[i] == 1) & (features[i] == 0)):
      f0 = f0 + 1
    elif ((labels[i] == 1) & (features[i] == 1)):
      f1 = f1 + 1

  priory1 = y / len(labels)
  pf0y1 = f0 / len(features)
  pf1y1 = f1 / len(features)

  prob0 = pf0y1 / priory1 
  prob1 = pf1y1 / priory1 
  
  return [prob0, prob1]

print(likelihood_estimate(features, labels))

def likelihood_estimate2(features, labels):
  f0 = 0.0
  f1 = 0.0
  y = 0.0
  
  if labels == 1: 
    features == 0.sum()


  priory1 = y / len(labels)
  pf0y1 = f0 / len(features)
  pf1y1 = f1 / len(features)

  prob0 = pf0y1 / priory1 
  prob1 = pf1y1 / priory1 
  
  return [prob0, prob1]

print(likelihood_estimate2(features, labels))

features = [0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 0, 1, 0, 1, 1, 1, 1]
labels = [0, 1, 1, 1, 1, 0, 0, 1, 1, 0, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 1, 0]
print(features[features == 0].sum())

def sumzeros(features):
    return (features == 0).sum()

sumzeros(features)

features = [0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 0, 1, 0, 1, 1, 1, 1]
len(features == '0')