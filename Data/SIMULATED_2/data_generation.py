#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Dec 10 17:47:42 2021

@author: anna
"""
import numpy as np
import pandas as pd
from sklearn.datasets import make_classification

dataset = make_classification(n_samples=64,
                              n_features=32,
                              n_informative=16,
                              n_redundant=16,
                              n_repeated=0,
                              n_classes=2,
                              class_sep=1,
                              shuffle=False,
                              random_state=1)

data = dataset[0]
labels = dataset[1]

# List of important features (1) and unimportant features (0)
idx = np.repeat(0,32)
idx[:16] = 1

# List of redundant features (1) and not-redundant features (0)
redundant_idx = np.repeat(0,32)
redundant_idx[16:] = 1

# =============================================================================
# Save data
# =============================================================================
pd.DataFrame(data).to_csv("data.csv", index=False)
pd.DataFrame(labels).to_csv("labels.csv", index=False)
pd.DataFrame(idx).to_csv("idx.csv", index=False)
pd.DataFrame(redundant_idx).to_csv("redundant_idx.csv", index=False)