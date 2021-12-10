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

for(w in c(0.01, 10, 100, 1000)){
  for (agreement in c(TRUE, FALSE)) {

    ifelse(agreement, {bl_weights = rep(0.01,8); bl_weights[important_groups] = w; a ="a"}, {bl_weights = rep(0.01,8); bl_weights[-important_groups] = w; a="d"})

    num_feats = length(which(important_features==1))
    # S0
    params_list[[paste0("m", w, a)]] =
      list(M = 100 ,   								# number elementary FS runs
           tt_split = 0.75,        			# ratio of train-test-partition per run
           method = c("mrmr"),          #which elementary feature selector to use
           constraints = buildConstraints(
             c("max_size"), 							# constraint types
             list(num_feats), 									# features
             ncol(data), 							# number of features
             rho = 1), 									# rho
           block_constraints = NULL,
           weights = build_block_weights(                               # prior weights per block
              blocks,
              bl_weights),
           optim_method = "GA", #optimization method GA or MH
           popsize = 100,
           maxiter = 100,
           max_size = num_feats,								# number of features for Hamming prior / ensemble
           I = 10,              			# number of runs per setup
           positive = "1",        						# positive class to compute F1 score
           decorr = FALSE,
	   prior_model = "dirichlet"
      )

    params_list[[paste0("h", w, a)]] = params_list[[paste0("m", w, a)]]
    params_list[[paste0("h", w, a)]]$method = "hsic"
    params_list[[paste0("h", w, a)]]$M = 5

    params_list[[paste0("l", w, a)]] = params_list[[paste0("m", w, a)]]
    params_list[[paste0("l", w, a)]]$method = "lasso"

    params_list[[paste0("r", w, a)]] = params_list[[paste0("m", w, a)]]
    params_list[[paste0("r", w, a)]]$method = "rfe"
    params_list[[paste0("r", w, a)]]$M = 5

    params_list[[paste0("f", w, a)]] = params_list[[paste0("m", w, a)]]
    params_list[[paste0("f", w, a)]]$method = "fisher"

    params_list[[paste0("L", w, a)]] = params_list[[paste0("m", w, a)]]
    params_list[[paste0("L", w, a)]]$method = "laplace"

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
    params_list[[i]])
}


res = feature_f1Score(res = res, true_indices = which(important_features==1), red_indices = which(redundant_features==1))


