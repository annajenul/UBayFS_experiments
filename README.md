# UBayFS_experiments

This repository contains code supporting the experiments for the manuscript entitled "A User-Guided Bayesian Framework for Ensemble Feature Selection in Life Science Applications (UBayFS)"

Experiments are performed on R v4.1.1 / R Studio v1.4 using the following software packages:
- DirichletReg v0.7-0 (Dirichlet distribution)
- hyper2 v2.0-1 (Hyperdirichlet distribution)
- GA v3.2.1 (optimization: Genetic Algorithm)
- mRMRe v2.1.0 (elementary model: mRMR)
- Rdimtools v1.0.8 (elementary model: Fisher score)
- rpart v4.1-15 (elementary model: decision tree)
- caret v6.0-88 (elementary model: RFE and predictive model: SVM)
- GSelection v0.1.0 (elementary model: HSIC)
- glmnet v4.1-2 (elementary model: Lasso and predictive model: GLM)

For experimental comparison with other state-of-the-art feature selectors (FS), the following software packages were used:
- random forest: R package randomForest v4.6-14
- RENT: Python package, see https://github.com/NMBU-Data-Science/RENT
- Sparse Group Lasso: R package SGL v1.3

For an implementation of the stability criterion by Nogueira et al., see https://github.com/nogueirs/JMLR2018.
