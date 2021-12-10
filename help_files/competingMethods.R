
# This script contains a function which computes the competing methods, used in the paper (random forest, Laplacian
# score, mRMR and sparse group lasso for datasets with block structure).
# To come close to the user defined number of features also in sparse group lasso,
# the bisection method is used prior to the sparse group lasso.


competingMethods = function(name, data, labels, K=100, stability_runs=10, seed=1){

  if(name == "BCW"){
    ########## BCW #############
    rf = random_forest(data, labels, pos="B", K = K, num = 5, stability_runs = stability_runs, seed = seed) #random forest
    lap = laplace(data, labels,pos="B", num = 5, stability_runs = stability_runs, seed = seed) # Laplacian score
    mrmr = mRMR(data, labels, pos="B", num = 5, stability_runs = stability_runs, seed = seed) # mRMR
    lambda = bisection_sparse(a = 10, b= 0.01, pos = "B", group = group_lasso_groups, n_feats = 5, seed=seed) # bisection
    grpl = sparse_group_lasso(data, labels, pos="B", group =  group_lasso_groups , # sparse group lasso
                       lambda = lambda, stability_runs =stability_runs, seed=seed)
  }
  else if(name == "HD"){
    rf = random_forest(data, labels, pos="1", K = K, num = 5, stability_runs = stability_runs, seed = seed)
    lap = laplace(data, labels, pos="1", num = 5, stability_runs = stability_runs, seed = seed)
    mrmr = mRMR(data, labels, pos="1", num = 5, stability_runs = stability_runs, seed = seed)

  }
  else if(name == "MPE"){
    rf = random_forest(data, labels, pos="1", K = K, num = 5, stability_runs = stability_runs, seed = seed)
    lap = laplace(data, labels, pos="1",  num = 5, stability_runs = stability_runs, seed = seed)
    mrmr = mRMR(data, labels, pos="1", num = 5, stability_runs = stability_runs, seed = seed)
  }
  else if(name == "COL"){
    rf = random_forest(data, labels, pos="1", K = K, num = 5, stability_runs = stability_runs, seed = seed)
    lap = laplace(data, labels, pos="1", num = 5, stability_runs = stability_runs, seed = seed)
    mrmr = mRMR(data, labels, pos="1", num = 5, stability_runs = stability_runs, seed = seed)
    lambda = bisection_sparse(a = 10, b= 0.01, pos = "1", group = group_lasso_groups, n_feats = 5, seed=seed)
    grpl = sparse_group_lasso(data, labels, pos="1", group =  group_lasso_groups ,
                       lambda = lambda, stability_runs =stability_runs, seed=seed)
  }
  else if(name == "LSVT"){
    rf = random_forest(data, labels, pos="1", K = K, num = 10, stability_runs = stability_runs, seed = seed)
    lap = laplace(data, labels, pos="1", num = 10, stability_runs = stability_runs, seed = seed)
    mrmr = mRMR(data, labels, pos="1", num = 10, stability_runs = stability_runs, seed = seed)
    lambda = bisection_sparse(a = 10, b= 0.01, pos = "1", group = group_lasso_groups, n_feats = 10, seed=seed)
    grpl = sparse_group_lasso(data, labels, pos="1", group =  group_lasso_groups ,
                       lambda = lambda, stability_runs =stability_runs, seed=seed)
  }
  else if(name == "p53"){
    rf = random_forest(data, labels, pos="active", K = K, num = 20, stability_runs = stability_runs, seed = seed)
    lap = laplace(data, labels, pos="active", num = 20, stability_runs = stability_runs, seed = seed)
    mrmr = mRMR(data, labels, pos="active", num = 20, stability_runs = stability_runs, seed = seed)
    lambda = bisection_sparse(a = 10, b= 0.01, pos = "active", group = group_lasso_groups, n_feats = 20, seed=seed)
    grpl = sparse_group_lasso(data, labels, pos="active", group =  group_lasso_groups ,
                        lambda = lambda, stability_runs =stability_runs, seed=seed)
  }

  else if(name == "SCADI"){
    rf = random_forest(data, labels, pos="1", K = K, num = 10, stability_runs = stability_runs, seed = seed)
    lap = laplace(data, labels, pos="1",  num = 10, stability_runs = stability_runs, seed = seed)
    mrmr = mRMR(data, labels, pos="1", num = 10, stability_runs = stability_runs, seed = seed)
  }

  else if(name == "prostate"){
    rf = random_forest(data, labels, pos="1", K = K, num = 20, stability_runs = stability_runs, seed = seed)
    lap = laplace(data, labels, pos="1",  num = 20, stability_runs = stability_runs, seed = seed)
    mrmr = mRMR(data, labels, pos="1", num = 20, stability_runs = stability_runs, seed = seed)
  }

  else if(name == "leukemia"){
    rf = random_forest(data, labels, pos="2", K = K, num = 20, stability_runs = stability_runs, seed = seed)
    lap = laplace(data, labels, pos="2",  num = 20, stability_runs = stability_runs, seed = seed)
    mrmr = mRMR(data, labels, pos="2", num = 20, stability_runs = stability_runs, seed = seed)
  }

  else{
    print("Name not found!")
  }
    cat("---------------------RESULTS----------------------------------", "\n")
    cat("The average glm f1-score of random forest is: ",rf$avg_f1_glm, "\n")
    cat("The average svm f1-score of random forest is: ",rf$avg_f1_svm, "\n")
    cat("The average knn f1-score of random forest is: ",rf$avg_f1_knn, "\n")
    cat("The average rpart f1-score of random forest is: ",rf$avg_f1_rpart, "\n")
    cat("Random forest has a stability of", getStability(rf$Z)$stability, " with a variance of ", getStability(rf$Z)$variance, "\n")
    cat("--------------------------------------------------------------", "\n")

    cat("The average f1-score_glm of laplace score is: ",lap$avg_f1_glm, "\n")
    cat("The average f1-score_svm of laplace score is: ",lap$avg_f1_svm, "\n")
    cat("The average f1-score_knn of laplace score is: ",lap$avg_f1_knn, "\n")
    cat("The average f1-score_rpart of laplace score is: ",lap$avg_f1_rpart, "\n")
    cat("Laplace score has a stability of", getStability(lap$Z)$stability, " with a variance of ", getStability(lap$Z)$variance, "\n")
    cat("--------------------------------------------------------------", "\n")

    cat("The average f1-score_glm of mrmr score is: ",mrmr$avg_f1_glm, "\n")
    cat("The average f1-score_svm of mrmr score is: ",mrmr$avg_f1_svm, "\n")
    cat("The average f1-score_knn of mrmr score is: ",mrmr$avg_f1_knn, "\n")
    cat("The average f1-score_rpart of mrmr score is: ",mrmr$avg_f1_rpart, "\n")
    cat("MRMR score has a stability of", getStability(mrmr$Z)$stability, " with a variance of ", getStability(mrmr$Z)$variance, "\n")
    cat("--------------------------------------------------------------", "\n")

    if(name %in% c("BCW", "COL", "LSVT", "p53")){
      cat("Lambda value selected with bisection: ", lambda, "\n")
      cat("The average f1-score_glm for GL is: ",grpl$avg_f1_glm, "\n")
      cat("The average f1-score_svm for GL is: ",grpl$avg_f1_svm, "\n")
      cat("The average f1-score_knn for GL is: ",grpl$avg_f1_knn, "\n")
      cat("The average f1-score_rpart for GL is: ",grpl$avg_f1_rpart, "\n")
      cat("GL score has a stability of", getStability(grpl$Z)$stability, " with a variance of ", getStability(grpl$Z)$variance, "\n")
      cat("--------------------------------------------------------------", "\n")
    }

  }






