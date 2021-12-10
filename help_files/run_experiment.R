### run UBayFS experiment

library(glmnet)
require(caret)

run_experiment = function(data,
                          labels,
                          params,
                          maxiter = 30,
                          seed = 1,
			  pos="1",
			  cor_level=0.5){

  # data ... a feature matrix
  # labels ... vector of labels (factor with 2 levels)

  # params ... list of model parameters:
  ## positive ... name of the positive class to calculate F1 score
  ## I ... number of runs I
  ## max_size ... number of features to select
  ## M ... number of splits to train elementary FS
  ## tt_split ... train-test-ratio in each run
  ## method ... vector of elementary FS to use
  ## constraints (A, b, rho) ... constraints
  ## block_constraints (A, b, rho) ... block-wise constraints
  ## weights ... prior weights alpha from user
  ## optim_method ... method for optimization (GA or MH)
  ## popsize ... initial sample size for optimization
  ## maxiter ... number of iterations for optimization
  # seed ... constant seed = 1 for all experiments
  
  set.seed(seed)
  if(is.null(params$lambda)){params$lambda=1}


  # initialize variables to track
  feature_sets = c() 							# selected feature sets
  times = c()       							# runtimes
  f1_glm = c()          							# F1 scores
  f1_svm = c()
  f1_knn = c()
  f1_rpart = c()
  rmse_LM = c()
  R2_LM = c()
  rmse_tree = c()
  R2_tree = c()
  no_violated_const = c()         # number of violated constraints
  no_optimal_sets = c()           # number of optimal feature sets
  correlation = c()
  all_models = list()

  # store train and test data for each stability run - needed for predicition models later
  train_data_list = list()
  train_target_list = list()
  test_data_list = list()
  test_target_list = list()

  # compute I Ubay models and store parameters
  # for(stab in 1:params$I){    					# I independent runs
  stab = 1
  iter = 0
  
  while(stab <= params$I && iter < maxiter){

    tryCatch({
      cat("stab:", stab, "\n")

      # perform train-test-split
      train_index = caret::createDataPartition(labels,
                                               p = params$tt_split, list = FALSE)
      test_index = setdiff(1:nrow(data), train_index)
      train_data = data[train_index,]
      train_labels <- labels[train_index]
      test_data = data[test_index,]
      test_labels <- labels[test_index]

      # scale data
      mean_train = apply(train_data, 2, mean)
      std_train = apply(train_data, 2, sd)
      std_train[std_train == 0] = 1

      train_data <- as.matrix(sweep(
        sweep(train_data,2, mean_train),
        2, std_train, "/"))
      test_data <- as.matrix(sweep(
        sweep(test_data,2, mean_train),
        2, std_train, "/"))

      train_data_list[[stab]] = train_data
      train_target_list[[stab]] = train_labels
      test_data_list[[stab]] = test_data
      test_target_list[[stab]] = test_labels

      # perform feature selection using UBayFS
      start = proc.time()                         # track time
      mod = build.UBaymodel(                      # build elementary models
        data = train_data,
        target = train_labels,
        M = params$M,
        tt_split = params$tt_split,
        nr_features = params$max_size,
        method = params$method,
        constraints = params$constraint,
        block_constraints = params$block_constraints,
        weights = params$weights,
        optim_method = params$optim_method,
        popsize = params$popsize,
        maxiter = params$maxiter,
        prior_model = params$prior_model,
  	lambda = params$lambda

      )
      if(!is.null(params$decorr) & params$decorr){
        mod <- UBayFS::setConstraints(mod, 
        UBayFS::buildDecorrConstraints(mod$data, level=cor_level, method="spearman"), append = TRUE)
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

      stab = stab + 1},
      error = function(e) {
        print(e)
        iter <<- iter + 1
      })

  }

  if (iter == maxiter){
    warning("Did not converge!")
    return(NA)
  }

  # prediction for each Ubay feature selection
  for (stab in 1:params$I) {


    features = feature_sets[stab,]
    if(sum(features) < 2){      				# glmnet does not work on 1 feature
      f1_glm = c(f1_glm, NA)
      f1_svm = c(f1_svm, NA)
      f1_knn = c(f1_knn, NA)
      f1_rpart = c(f1_rpart, NA)
      rmse_tree = c(rmse_tree, NA)
      rmse_LM = c(rmse_LM, NA)
      R2_LM = c(R2_LM, NA)
      R2_tree = c(R2_tree, NA)
    }
    else{
      if (is.factor(train_target_list[[1]])){
        results = metrics(train_data = train_data_list[[stab]], test_data = test_data_list[[stab]],
                          train_labels = train_target_list[[stab]], test_labels = test_target_list[[stab]],
                          features = features, f1_glm = f1_glm, f1_knn = f1_knn, f1_svm = f1_svm, f1_rpart = f1_rpart, pos=pos)

        f1_glm = results[["f1_glm"]]
        f1_svm = results[["f1_svm"]]
        f1_knn = results[["f1_knn"]]
        f1_rpart = results[["f1_rpart"]]
      }
      else{
        results = metrics(train_data = train_data_list[[stab]], test_data = test_data_list[[stab]],
                          train_labels = train_target_list[[stab]], test_labels = test_target_list[[stab]],
                          features = features, rmse_LM = rmse_LM, rmse_tree = rmse_tree, R2_LM = R2_LM, R2_tree = R2_tree)

        rmse_LM = results[["rmse_LM"]]
        R2_LM = results[["R2_LM"]]
        rmse_tree = results[["rmse_tree"]]
        R2_tree = results[["R2_tree"]]
      }
    }

  }


  # use of the stability measure by Nogueira et al. - https://github.com/nogueirs/JMLR2018
  stability = getStability(feature_sets, alpha=0.5)

  # returns data frame with means / variances over all metrics,
  # as well as the selected feature sets
  if (is.factor(train_target_list[[1]])){
    return(list(
      summary = data.frame(
        avg_times = mean(times, na.rm=TRUE),
        var_times = var(times, na.rm=TRUE),
        q25_times = quantile(times, 0.25, na.rm=TRUE),
        q75_times = quantile(times, 0.75, na.rm=TRUE),
        median_times = quantile(times, 0.5, na.rm=TRUE),
        avg_f1_glm = mean(f1_glm, na.rm=TRUE),
        var_f1_glm = var(f1_glm, na.rm=TRUE),
        q25_f1_glm = quantile(f1_glm, 0.25, na.rm=TRUE),
        q75_f1_glm = quantile(f1_glm, 0.75, na.rm=TRUE),
        median_f1_glm = quantile(f1_glm, 0.5, na.rm=TRUE),
        avg_f1_svm = mean(f1_svm, na.rm=TRUE),
        var_f1_svm = var(f1_svm, na.rm=TRUE),
        q25_f1_svm = quantile(f1_svm, 0.25, na.rm=TRUE),
        q75_f1_svm = quantile(f1_svm, 0.75, na.rm=TRUE),
        median_f1_svm = quantile(f1_svm, 0.5, na.rm=TRUE),
        avg_f1_knn = mean(f1_knn, na.rm=TRUE),
        var_f1_knn = var(f1_knn, na.rm=TRUE),
        q25_f1_knn = quantile(f1_knn, 0.25, na.rm=TRUE),
        q75_f1_knn = quantile(f1_knn, 0.75, na.rm=TRUE),
        median_f1_knn = quantile(f1_knn, 0.5, na.rm=TRUE),
        avg_f1_rpart = mean(f1_rpart, na.rm=TRUE),
        var_f1_rpart = var(f1_rpart, na.rm=TRUE),
        q25_f1_rpart = quantile(f1_rpart, 0.25, na.rm=TRUE),
        q75_f1_rpart = quantile(f1_rpart, 0.75, na.rm=TRUE),
        median_f1_rpart = quantile(f1_rpart, 0.5, na.rm=TRUE),
        avg_stability = stability$stability,
        var_stability = stability$variance,
        q25_stability = stability$lower,
        q75_stability = stability$upper,
        avg_correlation = mean(correlation, na.rm=TRUE),
        var_correlation = var(correlation, na.rm=TRUE),
        q_25_correlation = quantile(correlation, 0.25, na.rm=TRUE),
        q_75_correlation = quantile(correlation, 0.75, na.rm=TRUE),
        median_correlation = quantile(correlation, 0.5, na.rm=TRUE),
        no_violated_const = mean(no_violated_const, na.rm=TRUE),
        no_optimal_sets = mean(no_optimal_sets, na.rm=TRUE)),
      feature_sets = feature_sets,
      models = all_models)
    )
  }
  else{
    return(list(
      summary = data.frame(
        avg_times = mean(times),
        var_times = var(times),
        q25_times = quantile(times, 0.25),
        q75_times = quantile(times, 0.75),
        median_times = quantile(times, 0.5),
        avg_R2_LM = mean(R2_LM),
        var_R2_LM = var(R2_LM),
        q25_R2_LM = quantile(R2_LM, 0.25),
        q75_R2_LM = quantile(R2_LM, 0.75),
        median_R2_LM = quantile(R2_LM, 0.5),
        avg_R2_tree = mean(R2_tree),
        var_R2_tree = var(R2_tree),
        q25_R2_tree = quantile(R2_tree, 0.25),
        q75_R2_tree = quantile(R2_tree, 0.75),
        median_R2_tree = quantile(R2_tree, 0.5),
        avg_rmse_LM = mean(rmse_LM),
        var_rmse_LM = var(rmse_LM),
        q25_rmse_LM = quantile(rmse_LM, 0.25),
        q75_rmse_LM = quantile(rmse_LM, 0.75),
        median_rmse_LM = quantile(rmse_LM, 0.5),
        vg_rmse_tree = mean(rmse_tree),
        var_rmse_tree = var(rmse_tree),
        q25_rmse_tree = quantile(rmse_tree, 0.25),
        q75_rmse_tree = quantile(rmse_tree, 0.75),
        median_rmse_tree = quantile(rmse_tree, 0.5),
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
}
