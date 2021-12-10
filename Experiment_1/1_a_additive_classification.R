source("general_settings.R")

# Data generation
generate_data <- function(sample_size=1000, feature_size=256, classification=TRUE, additive=TRUE){
  set.seed(42)
  x = mvrnorm(n = sample_size, mu = rep(0, feature_size), Sigma = diag(rep(1,feature_size)))
  eps = rnorm(sample_size, 0, 1)

  ifelse(additive, {y = -2 * sin(2*x[,1]) + x[,2]^2 + x[,3] + exp(-x[,4]) + eps}, {y = x[,1] * exp(2 * x[,2]) + x[,3]^2 + eps})

  if(classification){y = factor(round(1 / (1 + exp(-y))))}

  return(list(data=as.data.frame(x), labels=y))

}

gd = generate_data(1000,1000,classification = TRUE, additive = TRUE)
data = gd[["data"]]
labels = gd[["labels"]]

params_list = list()

for(M in c(1,5,10,50,100,200)){

  # S0
  params_list[[paste0("m", M)]] =
    list(M = M,   								# number elementary FS runs
         tt_split = 0.75,        			# ratio of train-test-partition per run
         method = c("mrmr"),          #which elementary feature selector to use
         constraints = buildConstraints(
           c("max_size"), 							# constraint types
           list(4), 									# features
           ncol(data), 							# number of features
           rho = 1), 									# rho
         block_constraints = NULL,
         weights = rep(0.01, ncol(data)),
         optim_method = "GA", #optimization method GA or MH
         popsize = 100,
         maxiter = 100,
         max_size = 4,								# number of features for Hamming prior / ensemble
         I = 10,              			# number of runs per setup
         positive = "1",        						# positive class to compute F1 score
         decorr = FALSE,
         prior_model = "dirichlet"
    )

  if(M<=10){
  params_list[[paste0("h", M)]] = params_list[[paste0("m", M)]]
  params_list[[paste0("h", M)]]$method = "hsic"
  
  params_list[[paste0("r", M)]] = params_list[[paste0("m", M)]]
  params_list[[paste0("r", M)]]$method = "rfe" 
 }
  params_list[[paste0("l", M)]] = params_list[[paste0("m", M)]]
  params_list[[paste0("l", M)]]$method = "lasso"

  params_list[[paste0("f", M)]] = params_list[[paste0("m", M)]]
  params_list[[paste0("f", M)]]$method = "fisher"

  params_list[[paste0("L", M)]] = params_list[[paste0("m", M)]]
  params_list[[paste0("L", M)]]$method = "laplace"

  params_list[[paste0("t", M)]] = params_list[[paste0("m", M)]]
  params_list[[paste0("t", M)]]$method = "dtree"

}


res = NULL													# initialize result list
for(i in 1:length(params_list)){							# iterate over setups
  res[[ names(params_list)[i] ]] = run_experiment(
    data,
    labels,
    params_list[[i]])
}


res = feature_f1Score(res = res, true_indices = c(1,2,3,4))

