source("general_settings.R")


data = read.csv("Data/SIMULATED_2/data.csv")
labels = factor(read.csv("Data/SIMULATED_2/labels.csv")[,1])
important_features = read.csv("Data/SIMULATED_2/idx.csv")[,1]
redundant_features = read.csv("Data/SIMULATED_2/redundant_idx.csv")[,1]



params_list = list()

for(pm in c("dirichlet", "wong", "hankin")){
  for (decorr in c(TRUE, FALSE)) {
    if(pm == "wong"){w = "w"}
    if(pm == "dirichlet"){w = "d"}
    if(pm == "hankin"){w="h"}
    ifelse(decorr, {a ="y"}, {a="n"})

    num_feats = length(which(important_features==1))
    # S0
    params_list[[paste0("m", w, a)]] =
      list(M = 100,   								# number elementary FS runs
           tt_split = 0.75,        			# ratio of train-test-partition per run
           method = c("mrmr"),          #which elementary feature selector to use
           constraints = buildConstraints(
             c("max_size"), 							# constraint types
             list(num_feats), 									# features
             ncol(data), 							# number of features
             rho = 1), 									# rho
           block_constraints = NULL,
           weights = 0.01,
           optim_method = "GA", #optimization method GA or MH
           popsize = 100,
           maxiter = 100,
           max_size = num_feats,								# number of features for Hamming prior / ensemble
           I = 10,              			# number of runs per setup
           positive = "1",        						# positive class to compute F1 score
           decorr = decorr,
           prior_model = pm
      )

    params_list[[paste0("h", w, a)]] = params_list[[paste0("m", w, a)]]
    params_list[[paste0("h", w, a)]]$method = "hsic"
    params_list[[paste0("h", w, a)]]$M = 5
    #
    params_list[[paste0("l", w, a)]] = params_list[[paste0("m", w, a)]]
    params_list[[paste0("l", w, a)]]$method = "lasso"
    #
    params_list[[paste0("r", w, a)]] = params_list[[paste0("m", w, a)]]
    params_list[[paste0("r", w, a)]]$method = "rfe"
    params_list[[paste0("r", w, a)]]$M = 5
    #
    params_list[[paste0("f", w, a)]] = params_list[[paste0("m", w, a)]]
    params_list[[paste0("f", w, a)]]$method = "fisher"
    #
    params_list[[paste0("L", w, a)]] = params_list[[paste0("m", w, a)]]
    params_list[[paste0("L", w, a)]]$method = "laplace"
    #
    params_list[[paste0("t", w, a)]] = params_list[[paste0("m", w, a)]]
    params_list[[paste0("t", w, a)]]$method = "dtree"


  }
}


res = NULL													# initialize result list
for(i in 1:length(params_list)){							# iterate over setups
  print(names(params_list)[i])
  res[[ names(params_list)[i] ]] = run_experiment(
    data,
    labels,
    params_list[[i]], pos="1", cor_level = 0.4)
}


res = feature_f1Score(res = res, true_indices = which(important_features==1), red_indices = which(redundant_features==1))


