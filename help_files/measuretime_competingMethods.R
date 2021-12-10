
# This script contains a function which computes the competing methods, used in the paper (random forest, Laplacian
# score, mRMR and sparse group lasso for datasets with block structure).
# To come close to the user defined number of features also in sparse group lasso,
# the bisection method is used prior to the sparse group lasso.

# source the file define_competingMethods.R
time_competingMethods = function(name, data, labels, K=100, stability_runs=10, seed=1){

  if(name == "BCW"){
    ########## BCW #############
    cat("RF time: ", system.time(random_forest(data, labels, pos="B", K = K, num = 5, stability_runs = stability_runs, seed = seed))) #random forest
    #lap = laplace(data, labels,pos="B", num = 5, stability_runs = stability_runs, seed = seed) # Laplacian score
    #mrmr = mRMR(data, labels, pos="B", num = 5, stability_runs = stability_runs, seed = seed) # mRMR
    print("lambda compute")
    lambda = bisection_sparse(a = 10, b= 0.01, pos = "B", group = group_lasso_groups, n_feats = 5, seed=seed) # bisection
    cat("group lasso: ", system.time(sparse_group_lasso(data, labels, pos="B", group =  group_lasso_groups , # sparse group lasso
                       lambda = lambda, stability_runs =stability_runs, seed=seed)))
  }
  else if(name == "HD"){
    cat("RF time: ", system.time(random_forest(data, labels, pos="1", K = K, num = 5, stability_runs = stability_runs, seed = seed)))
    #lap = laplace(data, labels, pos="1", num = 5, stability_runs = stability_runs, seed = seed)
    #mrmr = mRMR(data, labels, pos="1", num = 5, stability_runs = stability_runs, seed = seed)

  }
  else if(name == "MPE"){
    cat("RF time: ", system.time(random_forest(data, labels, pos="1", K = K, num = 5, stability_runs = stability_runs, seed = seed)))
    #lap = laplace(data, labels, pos="1",  num = 5, stability_runs = stability_runs, seed = seed)
    #mrmr = mRMR(data, labels, pos="1", num = 5, stability_runs = stability_runs, seed = seed)
  }
  else if(name == "COL"){
    cat("RF time: ", system.time(random_forest(data, labels, pos="1", K = K, num = 5, stability_runs = stability_runs, seed = seed)))
    #lap = laplace(data, labels, pos="1", num = 5, stability_runs = stability_runs, seed = seed)
    #mrmr = mRMR(data, labels, pos="1", num = 5, stability_runs = stability_runs, seed = seed)
    lambda = bisection_sparse(a = 10, b= 0.01, pos = "1", group = group_lasso_groups, n_feats = 5, seed=seed)
    cat("group lasso time: ",  system.time(sparse_group_lasso(data, labels, pos="1", group =  group_lasso_groups ,
                       lambda = lambda, stability_runs =stability_runs, seed=seed)))
  }
  else if(name == "LSVT"){
    cat("RF time: ", system.time(random_forest(data, labels, pos="1", K = K, num = 10, stability_runs = stability_runs, seed = seed)))
    #lap = laplace(data, labels, pos="1", num = 10, stability_runs = stability_runs, seed = seed)
    #mrmr = mRMR(data, labels, pos="1", num = 10, stability_runs = stability_runs, seed = seed)
    lambda = bisection_sparse(a = 10, b= 0.01, pos = "1", group = group_lasso_groups, n_feats = 10, seed=seed)
    cat("GL time: ", system.time(sparse_group_lasso(data, labels, pos="1", group =  group_lasso_groups ,
                       lambda = lambda, stability_runs =stability_runs, seed=seed)))
  }
  else if(name == "p53"){
    cat("RF time: ", system.time(random_forest(data, labels, pos="active", K = K, num = 20, stability_runs = stability_runs, seed = seed)))
    #lap = laplace(data, labels, pos="active", num = 20, stability_runs = stability_runs, seed = seed)
    #mrmr = mRMR(data, labels, pos="active", num = 20, stability_runs = stability_runs, seed = seed)
    lambda = bisection_sparse(a = 10, b= 0.01, pos = "active", group = group_lasso_groups, n_feats = 20, seed=seed)
    cat("GL time: ", system.time(sparse_group_lasso(data, labels, pos="active", group =  group_lasso_groups ,
                        lambda = lambda, stability_runs =stability_runs, seed=seed)))
  }

  
  else if(name == "prostate"){
    cat("RF time: ", system.time(random_forest(data, labels, pos="1", K = K, num = 20, stability_runs = stability_runs, seed = seed)))
    #lap = laplace(data, labels, pos="1",  num = 20, stability_runs = stability_runs, seed = seed)
    #mrmr = mRMR(data, labels, pos="1", num = 20, stability_runs = stability_runs, seed = seed)
  }

  else if(name == "leukemia"){
    cat("RF time: ", system.time(random_forest(data, labels, pos="2", K = K, num = 20, stability_runs = stability_runs, seed = seed)))
    #lap = laplace(data, labels, pos="2",  num = 20, stability_runs = stability_runs, seed = seed)
    #mrmr = mRMR(data, labels, pos="2", num = 20, stability_runs = stability_runs, seed = seed)
  }

  }
