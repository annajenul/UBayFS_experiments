# This R-script contains code to run the competing methods random forest, Laplacian score, mRMR and sparse group lasso.

library(caret)
library(glmnet)
library(e1071)
library(mRMRe)
library(randomForest)
library(Rdimtools)
library(SGL)


#################
# random forest
#################

random_forest <- function(data, target, pos,  K=100, num=10, stability_runs = 10, seed = 1){
  # data ... dataset
  # target ... response vector
  # pos ... positive label
  # K ... number of trees
  # num ... number of features to select
  # stability_runs ... number of random forest runs
  # seed ... random seed for reproducible results

  set.seed(seed)
  Z = data.frame(matrix(0, nrow = stability_runs, ncol=ncol(data))) # a dataframe that stores the selected features
                                                                    # after each run. The i-th row of Z contains
                                                                    # information about which features are selecten in
                                                                    # the i-th run
  colnames(Z) = colnames(data)
  f1_glm = c()          							# F1 scores
  f1_svm = c()
  f1_knn = c()
  f1_rpart = c()


  # store train and test data for each stability run - needed for predicition models later
  train_data_list = list()
  train_target_list = list()
  test_data_list = list()
  test_target_list = list()


  for (stab in 1:stability_runs) { # loop through different runs
    # in each run, split data into train and test set
    train_index = createDataPartition(target, p = 0.75, list = FALSE)
    test_index = setdiff(1:length(target), train_index)
    train_data = data[train_index,]
    train_labels = target[train_index]
    test_data = data[test_index,]
    test_labels = target[test_index]

    mean_train = apply(train_data, 2, mean)
    std_train = apply(train_data, 2, sd)

    # standardization
    train_data_sc = as.matrix(sweep(sweep(train_data, 2, mean_train), 2, std_train, "/"))
    test_data_sc = as.matrix(sweep(sweep(test_data, 2, mean_train), 2, std_train, "/"))

    train_data_list[[stab]] = train_data_sc
    train_target_list[[stab]] = train_labels
    test_data_list[[stab]] = test_data_sc
    test_target_list[[stab]] = test_labels

    # remove features that contain NA values after standardization
    rem_cols_train = which(apply(train_data_sc, 2, function(x){return(any(is.na(x)))}))
    rem_cols_test = which(apply(test_data_sc, 2, function(x){return(any(is.na(x)))}))
    rem_cols = unique(c(rem_cols_train, rem_cols_test))
    if(length(rem_cols) > 0){
      train_data_sc = train_data_sc[,-rem_cols]
      test_data_sc = test_data_sc[,-rem_cols]
    }

    # random forest
    rf_data = cbind(train_labels, train_data_sc)
    colnames(rf_data) <- make.names(colnames(rf_data))
    fit = randomForest::randomForest(as.factor(as.character(train_labels))~.,
                                     data = rf_data, ntree = K, importance = TRUE, proximity = TRUE)
    feature_importances = order(randomForest::importance(fit, type=1), decreasing = TRUE)[1:num]
    Z[stab, feature_importances] = 1

  }


  for (stab in 1:stability_runs) {
    features = (Z[stab,] == 1)
    results = metrics(train_data = train_data_list[[stab]], test_data = test_data_list[[stab]],
                     train_labels = train_target_list[[stab]], test_labels = test_target_list[[stab]],
                     features = features, f1_glm = f1_glm, f1_knn = f1_knn, f1_svm = f1_svm, f1_rpart = f1_rpart, pos=pos)

    f1_glm = results[["f1_glm"]]
    f1_svm = results[["f1_svm"]]
    f1_knn = results[["f1_knn"]]
    f1_rpart = results[["f1_rpart"]]

  }
  return(list("avg_f1_glm" = mean(f1_glm, na.rm = TRUE),
              "avg_f1_svm" = mean(f1_svm, na.rm = TRUE),
              "avg_f1_knn" = mean(f1_knn, na.rm = TRUE),
              "avg_f1_rpart" = mean(f1_rpart, na.rm = TRUE),
              "Z" = Z))
}



#################
# Laplacian Score
#################

laplace <- function(data, target, pos, num=10, stability_runs = 10, seed=1){
  # data ... dataset
  # target ... response vector
  # pos ... positive label
  # num ... number of features to select
  # stability_runs ... number of random forest runs
  # seed ... random seed for reproducible results

  set.seed(seed)
  Z = data.frame(matrix(0, nrow = stability_runs, ncol=ncol(data)))
  colnames(Z) = colnames(data)
  f1_glm = c()          							# F1 scores
  f1_svm = c()
  f1_knn = c()
  f1_rpart = c()

  # store train and test data for each stability run - needed for predicition models later
  train_data_list = list()
  train_target_list = list()
  test_data_list = list()
  test_target_list = list()

  for (stab in 1:stability_runs) {

    train_index = createDataPartition(target, p = 0.75, list = FALSE)
    test_index = setdiff(1:length(target), train_index)
    train_data = data[train_index,]
    train_labels = target[train_index]
    test_data = data[test_index,]
    test_labels = target[test_index]

    mean_train = apply(train_data, 2, mean)
    std_train = apply(train_data, 2, sd)

    train_data_sc = as.matrix(sweep(sweep(train_data, 2, mean_train), 2, std_train, "/"))
    test_data_sc = as.matrix(sweep(sweep(test_data, 2, mean_train), 2, std_train, "/"))

    train_data_list[[stab]] = train_data_sc
    train_target_list[[stab]] = train_labels
    test_data_list[[stab]] = test_data_sc
    test_target_list[[stab]] = test_labels

    rem_cols_train = which(apply(train_data_sc, 2, function(x){return(any(is.na(x)))}))
    rem_cols_test = which(apply(test_data_sc, 2, function(x){return(any(is.na(x)))}))
    rem_cols = unique(c(rem_cols_train, rem_cols_test))
    if(length(rem_cols) > 0){
      train_data_sc = train_data_sc[,-rem_cols]
      test_data_sc = test_data_sc[,-rem_cols]
    }
    # Laplacian score for features
    lsc = do.lscore(train_data_sc, ndim = num)$featidx
    Z[stab, lsc] = 1

  }

  for (stab in 1:stability_runs) {
    features = (Z[stab,] == 1)
    results = metrics(train_data = train_data_list[[stab]], test_data = test_data_list[[stab]],
                      train_labels = train_target_list[[stab]], test_labels = test_target_list[[stab]],
                      features = features, f1_glm = f1_glm, f1_knn = f1_knn, f1_svm = f1_svm, f1_rpart = f1_rpart, pos=pos)


    f1_glm = results[["f1_glm"]]
    f1_svm = results[["f1_svm"]]
    f1_knn = results[["f1_knn"]]
    f1_rpart = results[["f1_rpart"]]

  }
  return(list("avg_f1_glm" = mean(f1_glm, na.rm = TRUE),
              "avg_f1_svm" = mean(f1_svm, na.rm = TRUE),
              "avg_f1_knn" = mean(f1_knn, na.rm = TRUE),
              "avg_f1_rpart" = mean(f1_rpart, na.rm = TRUE),
              "Z" = Z))
}




###########
# mRMR
###########

mRMR <- function(data, target, pos, num=10, stability_runs = 10, seed=1){
  # data ... dataset
  # target ... response vector
  # pos ... positive label
  # num ... number of features to select
  # stability_runs ... number of random forest runs
  # seed ... random seed for reproducible results

  set.seed(seed)
  Z = data.frame(matrix(0, nrow = stability_runs, ncol=ncol(data)))
  colnames(Z) = colnames(data)
  f1_glm = c()          							# F1 scores
  f1_svm = c()
  f1_knn = c()
  f1_rpart = c()


  # store train and test data for each stability run - needed for predicition models later
  train_data_list = list()
  train_target_list = list()
  test_data_list = list()
  test_target_list = list()

  for (stab in 1:stability_runs) {
    train_index = createDataPartition(target, p = 0.75, list = FALSE)
    test_index = setdiff(1:length(target), train_index)
    train_data = data[train_index,]
    train_labels = target[train_index]
    test_data = data[test_index,]
    test_labels = target[test_index]

    mean_train = apply(train_data, 2, mean)
    std_train = apply(train_data, 2, sd)

    train_data_sc = as.matrix(sweep(sweep(train_data, 2, mean_train), 2, std_train, "/"))
    test_data_sc = as.matrix(sweep(sweep(test_data, 2, mean_train), 2, std_train, "/"))

    train_data_list[[stab]] = train_data_sc
    train_target_list[[stab]] = train_labels
    test_data_list[[stab]] = test_data_sc
    test_target_list[[stab]] = test_labels

    rem_cols_train = which(apply(train_data_sc, 2, function(x){return(any(is.na(x)))}))
    rem_cols_test = which(apply(test_data_sc, 2, function(x){return(any(is.na(x)))}))
    rem_cols = unique(c(rem_cols_train, rem_cols_test))
    if(length(rem_cols) > 0){
      train_data_sc = train_data_sc[,-rem_cols]
      test_data_sc = test_data_sc[,-rem_cols]
    }

    # mRMR
    mr_dat = data.frame(train_data_sc, "class" = train_labels)
    mr_dat$class = factor(mr_dat$class, ordered = TRUE)
    mrmr = unlist(mRMR.classic(data=mRMR.data(mr_dat), target_indices = ncol(mr_dat), feature_count = num)@filters)
    Z[stab, mrmr] = 1

  }

  for (stab in 1:stability_runs) {
    features = (Z[stab,] == 1)
    results = metrics(train_data = train_data_list[[stab]], test_data = test_data_list[[stab]],
                      train_labels = train_target_list[[stab]], test_labels = test_target_list[[stab]],
                      features = features, f1_glm = f1_glm, f1_knn = f1_knn, f1_svm = f1_svm, f1_rpart = f1_rpart, pos=pos)


    f1_glm = results[["f1_glm"]]
    f1_svm = results[["f1_svm"]]
    f1_knn = results[["f1_knn"]]
    f1_rpart = results[["f1_rpart"]]

  }
  return(list("avg_f1_glm" = mean(f1_glm, na.rm = TRUE),
              "avg_f1_svm" = mean(f1_svm, na.rm = TRUE),
              "avg_f1_knn" = mean(f1_knn, na.rm = TRUE),
              "avg_f1_rpart" = mean(f1_rpart, na.rm = TRUE),
              "Z" = Z))
}




####################
# sparse group lasso
####################

sparse_group_lasso <- function(data, target, pos, group, stability_runs = 10, lambda=0.1, seed=1){
  # data ... dataset
  # target ... response vector
  # pos ... positive label
  # group ... vector of feature-group assignment
  # num ... number of features to select
  # stability_runs ... number of random forest runs
  # lambda ... regularization parameter value
  # seed ... random seed for reproducible results

  set.seed(seed)
  Z = data.frame(matrix(0, nrow = stability_runs, ncol=ncol(data)))
  colnames(Z) = colnames(data)
  f1_glm = c()          							# F1 scores
  f1_svm = c()
  f1_knn = c()
  f1_rpart = c()
  avg_feature_length = c() # track the average feature length


  # original target levels
  # transform factors to numeric values, which are required for the SGL function
  target_to_sparse_glasso <- function(target){
    t = rep(0, length(target))
    t[target == pos] <- 1
    t[target != pos] <- 0
    return(t)
  }

  # store train and test data for each stability run - needed for predicition models later
  train_data_list = list()
  train_target_list = list()
  test_data_list = list()
  test_target_list = list()


  for (stab in 1:stability_runs) {
    g = group # copy the group - if some features are removed already after standardization, they
              # also need to be removed from the group vector

    train_index = createDataPartition(target, p = 0.75, list = FALSE)
    test_index = setdiff(1:length(target), train_index)
    train_data = data[train_index,]
    train_labels = target[train_index]
    test_data = data[test_index,]
    test_labels = target[test_index]

    mean_train = apply(train_data, 2, mean)
    std_train = apply(train_data, 2, sd)

    train_data_sc = as.matrix(sweep(sweep(train_data, 2, mean_train), 2, std_train, "/"))
    test_data_sc = as.matrix(sweep(sweep(test_data, 2, mean_train), 2, std_train, "/"))

    train_data_list[[stab]] = train_data_sc
    train_target_list[[stab]] = train_labels
    test_data_list[[stab]] = test_data_sc
    test_target_list[[stab]] = test_labels

    rem_cols_train = which(apply(train_data_sc, 2, function(x){return(any(is.na(x)))}))
    rem_cols_test = which(apply(test_data_sc, 2, function(x){return(any(is.na(x)))}))
    rem_cols = unique(c(rem_cols_train, rem_cols_test))
    if(length(rem_cols) > 0){
      train_data_sc = train_data_sc[,-rem_cols]
      test_data_sc = test_data_sc[,-rem_cols]
      g = group[-rem_cols] # remove columns with NA's also from the group vector

    }
    #sparse group lasso
    sgdata <- list(x = train_data_sc, y = target_to_sparse_glasso(train_labels))
    gl_model <- SGL(data = sgdata, index = g, type = "logit", nlam = 1,  lambdas = lambda)
    gl = which(gl_model$beta!=0)
    Z[stab, gl] = 1
    avg_feature_length = c(avg_feature_length, length(gl))
  }
  for (stab in 1:stability_runs) {
    features = (Z[stab,] == 1)
    results = metrics(train_data = train_data_list[[stab]], test_data = test_data_list[[stab]],
                      train_labels = train_target_list[[stab]], test_labels = test_target_list[[stab]],
                      features = features, f1_glm = f1_glm, f1_knn = f1_knn, f1_svm = f1_svm, f1_rpart = f1_rpart, pos=pos)


    f1_glm = results[["f1_glm"]]
    f1_svm = results[["f1_svm"]]
    f1_knn = results[["f1_knn"]]
    f1_rpart = results[["f1_rpart"]]

  }
  return(list("avg_f1_glm" = mean(f1_glm, na.rm = TRUE),
              "avg_f1_svm" = mean(f1_svm, na.rm = TRUE),
              "avg_f1_knn" = mean(f1_knn, na.rm = TRUE),
              "avg_f1_rpart" = mean(f1_rpart, na.rm = TRUE),
              "Z" = Z,
              "avg_feature_length" = mean(avg_feature_length)))
}



##############################
# sparse group lasso bisection
##############################

bisection_sparse <- function(a, b, n_feats, pos, group, val_a=NULL, val_b=NULL, seed=1){
  # a ... upper limit
  # b ... lower limit
  # n_feats ... number of features to select (bisection optimized w.r.t. this number)
  # pos ... positive label
  # group ... vector of feature-group assignment
  # val_a ... average number of features for value a
  # val_b ... average number of features for value b
  # seed ... random seed for reproducibe results


  m = exp((log(a) + log(b))/2)
  if(is.null(val_a)){
    val_a = sparse_group_lasso(data, labels, pos=pos, group = group, stability_runs = 5, lambda = a, seed=seed)$avg_feature_length
  }
  val_m <- sparse_group_lasso(data, labels, pos=pos, group = group, stability_runs = 5, lambda = m, seed=seed)$avg_feature_length
  if(is.null(val_b)){
    val_b = sparse_group_lasso(data, labels, pos=pos, group = group, stability_runs = 5, lambda = b, seed=seed)$avg_feature_length
  }

  if(abs(a - b) < 1e-2){
    print("Stop with a=b")
    return(b)
  }
  else if(val_a >= n_feats){
    print("Stop with val_a >= n_feats")
    return(a)
  }
  else if(val_b <= n_feats){
    print("Stop with val_b <= n_feats")
    return(b)
  }
  else if(val_m > n_feats){
    return(bisection_sparse(a = a, b = m, pos=pos, group=group, seed=seed, n_feats = n_feats, val_a = val_a, val_b = val_m))
  }
  else if(val_m < n_feats){
    return(bisection_sparse(a = m, b = b, pos=pos, group=group, seed=seed, n_feats = n_feats, val_a = val_m, val_b = val_b))
  }
  else{
    return(m)
  }
}



