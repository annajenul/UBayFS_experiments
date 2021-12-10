feature_f1Score <- function(res, true_indices, red_indices=NULL){
  indices = rep(0, ncol(res[[1]]$feature_sets))
  indices[true_indices] = 1

  if(!is.null(red_indices)){
    redundant_indices = rep(0, ncol(res[[1]]$feature_sets))
    redundant_indices[red_indices] = 1
  }


  feature_sets_f1 = list()
  redundant_feature_sets_f1 = list()

  for (r in 1:length(res)) {
    if(is.list(res[[r]])){
      nr_feature_sets = nrow(res[[r]]$feature_sets)
      f1_scores_featuresets = apply(res[[r]]$feature_sets * 1, 1, function(x){return(caret::confusionMatrix(
        data = factor(x, levels = c(0,1)),
        reference = factor(indices, levels = c(0,1)), positive = "1")$byClass["F1"])})
      res[[r]]$important_features_f1 = f1_scores_featuresets
      # feature_sets_f1[[names(res)[r]]] = f1_scores_featuresets

      if(!is.null(red_indices)){
        f1_scores_featuresets = apply(res[[r]]$feature_sets * 1, 1, function(x){return(caret::confusionMatrix(
          data = factor(x, levels = c(0,1)),
          reference = factor(redundant_indices, levels = c(0,1)), positive = "1")$byClass["F1"])})
        redundant_feature_sets_f1[[names(res)[r]]] = f1_scores_featuresets
        res[[r]]$redundant_features_f1 = f1_scores_featuresets
      }


    }
  }

  return(res)

}

