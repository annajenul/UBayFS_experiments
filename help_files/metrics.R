classification_metrics <- function(train_data, test_data, train_labels, test_labels, features,
                                   f1_glm, f1_svm, f1_knn, f1_rpart, pos){

  if (length(features) == 0 || all(features == FALSE)) {
    print("no feature selected")
    f1_glm = c(f1_glm, 0)
    f1_svm = c(f1_svm, 0)
    f1_knn = c(f1_knn, 0)
    f1_rpart = c(f1_rpart, 0)
  }
  else{
    tr_data <- cbind(train_labels, as.data.frame(train_data[,features]))
    te_data <- cbind(test_labels, as.data.frame(test_data[,features]))
    if (dim(tr_data)[2] == 2){
	tr_data$copy = tr_data[,2]; 
	te_data$copy = te_data[,2]
	colnames(tr_data) = c("train_labels", "f1", "f2");
	colnames(te_data) = c("test_labels", "f1", "f2")
}

    # GLM

    glm_model = caret::train(train_labels~., data = tr_data, method = "glmnet", lambda=0) 
    f1_score_glm <- confusionMatrix(predict(glm_model, newdata = te_data), test_labels, positive=pos)$byClass["F1"]
    if(is.na(f1_score_glm)){f1_glm = c(f1_glm, 0)} # predictions can be NA when F1 score is not defined due to an undefined precision
    # when no sample is predicted as true (true positive + false positive = 0)
    else{f1_glm = c(f1_glm, f1_score_glm)}

    # SVM
    svm_model = caret::train(train_labels~., data = tr_data, method = "svmLinear")
    f1_score_svm = confusionMatrix(predict(svm_model, newdata = te_data), test_labels, positive=pos)$byClass["F1"]
    if(is.na(f1_score_svm)){f1_svm = c(f1_svm, 0)}
    else{f1_svm = c(f1_svm, f1_score_svm)}

    # KNN
    knn_model = caret::train(train_labels~., data = tr_data, method = "knn")
    f1_score_knn = confusionMatrix(predict(knn_model, newdata = te_data), test_labels, positive=pos)$byClass["F1"]
    if(is.na(f1_score_knn)){f1_knn = c(f1_knn, 0)}
    else{f1_knn = c(f1_knn, f1_score_knn)}


    # random forest
    colnames(tr_data) <- make.names(colnames(tr_data))
    colnames(te_data) <- make.names(colnames(te_data))
    rpart_model = caret::train(train_labels~., data = tr_data, method = "rf", ntree=100)
    f1_score_rpart = confusionMatrix(predict(rpart_model, newdata = te_data), test_labels, positive=pos)$byClass["F1"]
    if(is.na(f1_score_rpart)){f1_rpart = c(f1_rpart, 0)}
    else{f1_rpart = c(f1_rpart, f1_score_rpart)}
  }

  return(list(f1_glm = f1_glm, f1_svm = f1_svm, f1_knn = f1_knn, f1_rpart = f1_rpart))
}



regression_metrics <- function(train_data, test_data, train_labels, test_labels, features, rmse_LM, R2_LM, rmse_tree, R2_tree){

  if (length(features) == 0 || all(features == FALSE)) {
    print("no feature selected")
    rmse_LM = c(rmse_LM, 0)
    rmse_R2 = c(rmse_R2, 0)
    R2_LM = c(R2_LM, 0)
    R2_tree = c(R2_tree, 0)
  }
  else{

    tr_data <- cbind(train_labels, as.data.frame(train_data[,features]))
    te_data <- cbind(test_labels, as.data.frame(test_data[,features]))

    # GLM
    #lm_model = caret::train(train_labels~., data = tr_data, method = "lm")
    lm_model = lm(train_labels~., data = tr_data)
    pred = predict(lm_model, newdata = te_data)
    rmse_score <- caret::RMSE(pred = pred, obs = test_labels)
    R2_score <- caret::R2(pred = pred, obs = test_labels, formula = "traditional")

    if (is.na(rmse_score)){
      rmse_LM = c(rmse_LM, 0)
    }else{rmse_LM = c(rmse_LM, rmse_score)}

    if (is.na(R2_score)){
      R2_LM = c(R2_LM, 0)
    }else{R2_LM = c(R2_LM, R2_score)}


    # random forest
    colnames(tr_data) <- make.names(colnames(tr_data))
    colnames(te_data) <- make.names(colnames(te_data))
    rpart_model = caret::train(train_labels~., data = tr_data, method = "rf", ntree=100)
    pred = predict(rpart_model, newdata = te_data)
    rmse_score <- caret::RMSE(pred = pred, obs = test_labels)
    R2_score <- caret::R2(pred = pred, obs = test_labels)

    if (is.na(rmse_score)){
      rmse_tree = c(rmse_tree, 0)
    }else{rmse_tree = c(rmse_tree, rmse_score)}

    if (is.na(R2_score)){
      R2_tree = c(R2_tree, 0)
    }else{R2_tree = c(R2_tree, R2_score)}

  }

  return(list(rmse_LM = rmse_LM, rmse_tree = rmse_tree, R2_LM = R2_LM, R2_tree = R2_tree))
}


metrics <- function(train_data, test_data, train_labels, test_labels, features,
                    f1_glm=NULL, f1_svm=NULL, f1_knn=NULL, f1_rpart=NULL, rmse_LM=NULL, R2_LM=NULL, rmse_tree=NULL, R2_tree=NULL, pos="1"){


  if(is.factor(train_labels)){
    return(classification_metrics(train_data, test_data, train_labels, test_labels, features,
                                  f1_glm, f1_svm, f1_knn, f1_rpart, pos=pos))
  }
  else{
    return(regression_metrics(train_data, test_data, train_labels, test_labels, features,
                              rmse_LM, R2_LM, rmse_tree, R2_tree))
  }
}
