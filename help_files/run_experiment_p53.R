library(caret)

library(glmnet)


run_experiment_p53 = function(data,
                          labels,
                          params,
                          seed = 1, 
                          pos = "active",
                          cor_level = 0.5){



set.seed(seed)

# initialize variables to track
feature_sets = c()                                                    # selected feature sets
times = c()                                                           # runtimes
f1_glm = c()          							# F1 scores
f1_svm = c()
f1_knn = c()
f1_rpart = c()                                                           # F1 scores
no_violated_const = c()
no_optimal_sets = c()           # number of optimal feature sets
correlation = c()
all_models = list()


original_block_matrix = params$block_constraints$block_matrix

# store train and test data for each stability run - needed for predicition models later
train_data_list = list()
train_target_list = list()
test_data_list = list()
test_target_list = list()

#for(stab in 1:params$I){                                      # I independent runs
 
 stab = 1
 converge = 0
 while(stab <= params$I){

 tryCatch({
 cat("stab:", stab, "\n")

 # perform train-test-split
  train_index = caret::createDataPartition(labels,
                                           p = params$tt_split, list = FALSE)
  test_index = setdiff(1:nrow(data), train_index)
  train_data = data[train_index,]
  train_labels = labels[train_index]
  test_data = data[test_index,]
  test_labels = labels[test_index]

  # scale data
  mean_train = apply(train_data, 2, mean)
  std_train = apply(train_data, 2, sd)
  train_data = as.matrix(sweep(
    sweep(train_data,2, mean_train),
    2, std_train, "/"))
  test_data = as.matrix(sweep(
    sweep(test_data,2, mean_train),
    2, std_train, "/"))

  rem_cols_train = which(apply(train_data, 2, function(x){return(any(is.na(x)))}))
  rem_cols_test = which(apply(test_data, 2, function(x){return(any(is.na(x)))}))

  rem_cols =unique( c(rem_cols_train, rem_cols_test))
  if(length(rem_cols) > 0){
    train_data = train_data[,-rem_cols]
    test_data = test_data[,-rem_cols]
    params$block_constraints$block_matrix = original_block_matrix[, -rem_cols]

  }else{params$block_constraints$block_matrix = original_block_matrix}


  cat("block_constraint matrix before: ", dim(params$block_constraints$block_matrix), "\n")

  cat("block_constraint matrix after: ", dim(params$block_constraints$block_matrix), "\n")

  cat("Dim train data: ", dim(train_data), "\n")

  cat("Dim test data: ", dim(test_data), "\n")

  cat(" length remove columns", length(rem_cols),"\n")

  # perform feature selection using UBayFS
  start = proc.time()                         # track


  train_data_list[[stab]] = train_data
  train_target_list[[stab]] = train_labels
  test_data_list[[stab]] = test_data
  test_target_list[[stab]] = test_labels

  mod = build.UBaymodel(                          # build elementary models
    data = train_data,
    target = train_labels,
    M = params$M,
    tt_split = params$tt_split,
    nr_features = params$max_size,
    method = params$method,
    constraints = buildConstraints(
      c("max_size"),                                                         # constraint types
      list(20),                                                                     # features
      ncol(train_data),                                                    # number of features
      rho = 1),
    block_constraints = params$block_constraints,
    weights = rep(1, ncol(train_data)),
    optim_method = params$optim_method,
    popsize = params$popsize,
    maxiter = params$maxiter
  )
  
  if(!is.null(params$decorr) & params$decorr){
      mod <- UBayFS::setConstraints(mod, 
      buildDecorrConstraints(mod$data, level = cor_level, method = "spearman"), append = TRUE)
    }
  
  mod = UBayFS::train(mod) # optimization
  
  
  times = c(times, (proc.time() - start)[3])   # end runtime tracking
  
  all_models[[stab]] = mod
  correlation = c(correlation, mod$output$metrics[nrow(mod$output$metrics),1])
  no_optimal_sets = c(no_optimal_sets, nrow(mod$output$feature_set))
  for(l in 1:nrow(mod$output$feature_set)){
    features = as.vector(mod$output$feature_set[l,] == 1)
    feature_sets = rbind(feature_sets, features)
  }
   # number of violated constraints
      new_vals = sum(mod$constraint.params$constraints$A %*% features > mod$constraint.params$constraints$b)
      if(!is.null(mod$constraint.params$block_constraints)){
        new_vals = new_vals + sum(mod$constraint.params$block_constraints$A %*%
                                    (mod$constraint.params$block_constraints$block_matrix %*% features >=1) > mod$constraint.params$block_constraints$b)
      }
      no_violated_const = c(no_violated_const, new_vals)


    stab = stab + 1}, error = function(e) {
	stab = stab
	converge <<- converge  + 1
})

   if (converge == 30){
 	warning("Did not converge within 30 runs!")
	return(NA)
	}
   

  }
  
  for (stab in 1:params$I) {


    features = feature_sets[stab,]
    if(sum(features) < 2){      				# glmnet does not work on 1 feature
      f1_glm = c(f1_glm, NA)
      f1_svm = c(f1_svm, NA)
      f1_knn = c(f1_knn, NA)
      f1_rpart = c(f1_rpart, NA)
    }
    else{

      results = metrics(train_data = train_data_list[[stab]], test_data = test_data_list[[stab]],
                       train_labels = train_target_list[[stab]], test_labels = test_target_list[[stab]],
                       features = features, f1_glm = f1_glm, f1_knn = f1_knn, f1_svm = f1_svm, f1_rpart = f1_rpart, pos=pos)

      f1_glm = results[["f1_glm"]]
      f1_svm = results[["f1_svm"]]
      f1_knn = results[["f1_knn"]]
      f1_rpart = results[["f1_rpart"]]
    }

  }



# stability see https://github.com/nogueirs/JMLR2018
stability = getStability(feature_sets, alpha=0.5)

# returns data frame with means / variances over all metrics,
# as well as the selected feature sets
return(list(
    summary = data.frame(
      avg_times = mean(times),
      var_times = var(times),
      q25_times = quantile(times, 0.25),
      q75_times = quantile(times, 0.75),
      median_times = quantile(times, 0.5),
      avg_f1_glm = mean(f1_glm),
      var_f1_glm = var(f1_glm),
      q25_f1_glm = quantile(f1_glm, 0.25),
      q75_f1_glm = quantile(f1_glm, 0.75),
      median_f1_glm = quantile(f1_glm, 0.5),
      avg_f1_svm = mean(f1_svm),
      var_f1_svm = var(f1_svm),
      q25_f1_svm = quantile(f1_svm, 0.25),
      q75_f1_svm = quantile(f1_svm, 0.75),
      median_f1_svm = quantile(f1_svm, 0.5),
      avg_f1_knn = mean(f1_knn),
      var_f1_knn = var(f1_knn),
      q25_f1_knn = quantile(f1_knn, 0.25),
      q75_f1_knn = quantile(f1_knn, 0.75),
      median_f1_knn = quantile(f1_knn, 0.5),
      avg_f1_rpart = mean(f1_rpart),
      var_f1_rpart = var(f1_rpart),
      q25_f1_rpart = quantile(f1_rpart, 0.25),
      q75_f1_rpart = quantile(f1_rpart, 0.75),
      median_f1_rpart = quantile(f1_rpart, 0.5),
      avg_stability = stability$stability,
      var_stability = stability$variance,
      q25_stability = stability$lower,
      q75_stability = stability$upper,
      avg_correlation = mean(correlation),
      var_correlation = var(correlation),
      q_25_correlation = quantile(correlation, 0.25),
      q_75_correlation = quantile(correlation, 0.75),
      median_correlation = quantile(correlation, 0.5),
      no_violated_const = mean(no_violated_const),
      no_optimal_sets = mean(no_optimal_sets)),
    feature_sets = feature_sets,
    models = all_models)
  )
}


