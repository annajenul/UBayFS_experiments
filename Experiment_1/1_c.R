source("general_settings.R")


data = read.csv("Data/SIMULATED_1/data.csv")
labels = factor(read.csv("Data/SIMULATED_1/labels.csv")[,1])
important_features = read.csv("Data/SIMULATED_1/idx.csv")[,1]
redundant_features = read.csv("Data/SIMULATED_1/redundant_idx.csv")[,1]
important_groups = read.csv("Data/SIMULATED_1/important_groups.csv")[,1]



block_list = list()
count = 1
for (b in 1:8) {
  block_list[[b]] = c(count : (count + 31))
  count = count + 32
}

blocks = c()
for (i in 1:8) {
  blocks = c(blocks, rep(i, 32))
}

build_block_weights = function(blocks, weights){
  return(weights[blocks])
}


params_list = list()

for(rho in c(0,1,10,100,Inf)){
  for (OPB in c(TRUE, FALSE)) {

    if(rho==0){constraints = buildConstraints(constraint_types = c("max_size"),
                                          constraint_vars = 16,
                                          num_elements = ncol(data),
                                          rho = c(1),
                                          block_list = NULL);
		block_constraints = NULL; a = "opb"}
    else{
    ifelse(OPB, {constraints = buildConstraints(constraint_types = c("max_size", rep("cannot_link",8)),
                                               constraint_vars = append(16, block_list),
                                               num_elements = ncol(data),
                                               rho = c(1,rep(rho,8)),
                                               block_list = NULL);
		 constraints$b = c(16, rep(2, length(constraints$b)-1));
                 block_constraints = NULL;
                 a ="opb"},
           {constraints = buildConstraints(constraint_types = c("max_size"),
                                          constraint_vars = 16,
                                          num_elements = ncol(data),
                                          rho = c(1),
                                          block_list = NULL);
            block_constraints = buildConstraints(constraint_types = c("max_size"),
                                                constraint_vars = 2,
                                                num_elements = 8,
                                                rho = rho,
                                                block_list = block_list);
            a="bms"})
	}

    num_feats = length(which(important_features==1))
    # S0
    params_list[[paste0("m", rho, a)]] =
      list(M = 100 ,   								# number elementary FS runs
           tt_split = 0.75,        			# ratio of train-test-partition per run
           method = c("mrmr"),          #which elementary feature selector to use
           constraints = constraints, 									# rho
           block_constraints = block_constraints,
           weights = 0.01,
           optim_method = "GA", #optimization method GA or MH
           popsize = 100,
           maxiter = 100,
           max_size = num_feats,								# number of features for Hamming prior / ensemble
           I = 10,              			# number of runs per setup
           positive = "1",        						# positive class to compute F1 score
           decorr = FALSE,
	   prior_model = "dirichlet"
      )

    params_list[[paste0("h", rho, a)]] = params_list[[paste0("m", rho, a)]]
    params_list[[paste0("h", rho, a)]]$method = "hsic"
    params_list[[paste0("h", rho, a)]]$M = 5

    params_list[[paste0("l", rho, a)]] = params_list[[paste0("m", rho, a)]]
    params_list[[paste0("l", rho, a)]]$method = "lasso"

    params_list[[paste0("r", rho, a)]] = params_list[[paste0("m", rho, a)]]
    params_list[[paste0("r", rho, a)]]$method = "rfe"
    params_list[[paste0("r", rho, a)]]$M = 5

    params_list[[paste0("f", rho, a)]] = params_list[[paste0("m", rho, a)]]
    params_list[[paste0("f", rho, a)]]$method = "fisher"

    params_list[[paste0("L", rho, a)]] = params_list[[paste0("m", rho, a)]]
    params_list[[paste0("L", rho, a)]]$method = "laplace"

    params_list[[paste0("t", rho, a)]] = params_list[[paste0("m", rho, a)]]
    params_list[[paste0("t", rho, a)]]$method = "dtree"


  }
}


res = NULL													# initialize result list
for(i in 1:length(params_list)){							# iterate over setups
  print(names(params_list)[i])
  res[[ names(params_list)[i] ]] = run_experiment(
    data,
    labels,
    params_list[[i]])
}


res = feature_f1Score(res = res, true_indices = which(important_features==1), red_indices = which(redundant_features==1))




