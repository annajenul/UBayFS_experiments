#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Dec 10 17:37:40 2021

@author: anna
"""
import numpy as np
import pandas as pd
from sklearn.datasets import make_classification

dataset = make_classification(n_samples=512,
                              n_features=256,
                              n_informative=16,
                              n_redundant=6,
                              n_repeated=0,
                              n_classes=2,
                              class_sep=1,
                              shuffle=False,
                              random_state=1)

data = dataset[0]
labels = dataset[1]

# =============================================================================
# Generate group structure
# =============================================================================
np.random.seed(42)
bo = np.random.permutation(8) # random order of blocks
bv = np.concatenate([np.repeat(bo[0],4), 
                     np.repeat(bo[1],4), 
                     np.repeat(bo[2],4),
                     np.repeat(bo[3],4),
                     np.repeat(bo[4],3),
                     np.repeat(bo[5],3),
                     np.repeat(bo[0],28),
                     np.repeat(bo[1],28),
                     np.repeat(bo[2],28),
                     np.repeat(bo[3],28),
                     np.repeat(bo[4],29),
                     np.repeat(bo[5],29),
                     np.repeat(bo[6],32),
                     np.repeat(bo[7],32)])


bv = bv + np.arange(0,1, (1/256))
fo = np.argsort(bv)

data = data[:,fo]

# List of important features (1) and unimportant features (0)
idx = np.repeat(0,256)
for i in range(4):
    idx[(32*bo[i]):(32*bo[i]+4)] = 1


# List of redundant features (1) and not-redundant features (0)
redundant_idx = np.repeat(0,256)
for i in np.arange(4,6,1):
    redundant_idx[(32*bo[i]):(32*bo[i]+3)] = 1

# Indices of important groups (+1 because dataset is used in R)
important_groups = bo[0:4]+1


# =============================================================================
# Save files
# =============================================================================
pd.DataFrame(data).to_csv("data.csv", index=False)
pd.DataFrame(labels).to_csv("labels.csv", index=False)
pd.DataFrame(idx).to_csv("idx.csv", index=False)
pd.DataFrame(redundant_idx).to_csv("redundant_idx.csv", index=False)
pd.DataFrame(important_groups).to_csv("important_groups.csv", index=False)