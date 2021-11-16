#' --------------------------------------------
#  1. define ml_detect ------------------------
#' function to apply ML approach to single case
#' --------------------------------------------

ml_detect <- function(data = aus08, eligible = aus08$eligible, votes_a = aus08$SPÖ, 
                      votes_b = aus08$ÖVP, turnout_emp = aus08$turnout, shareA_emp = aus08$share_spo, 
                      shareB_emp = aus08$share_ovp, fraud_incA = seq(0.01, 0.50, 0.01), 
                      fraud_extA = seq(0.01, 0.1, 0.01),
                      fraud_types = c("bbs", "stealing", "switching"),
                      n_elections = 1, models = c("kNN", "regul_reg", "randomForest", "gradBoost"), 
                      ml_task = c("binary", "cat", "cont"), seed=12345, parallel = T) {
  
  # data = data of empirical case 
  # eligible = vector of eligible voters across entities of empirical case
  # votes_a = vector of raw votes (winner)
  # votes_b = vector of raw votes (looser)
  # turnout_emp = empirical turnout vector
  # shareA_emp = empirical shareA vector (winner's vote shares)
  # shareB_emp = empirical shareB vector (looser's vote shares)
  # fraud_incA/B = range of fraud_inc parameters to train on 
  # fraud_extA/B = range of fraud_ext parameters to train on 
  # fraud_types = fraud types to train on 
  # n_elections = number of artifical elections to create for each individual scenario
  # models = ML approaches to use for training
  # ml_task = which ML tasks should be performed?
  # seed = seed value to use
  # parallel = should parallel computing be used?
  
  
  ### so basically, I use the gen_data function here within this function 
  ### the input is just a single empirical dataset, out of which I extract the relevant
  ### parameters like turnout_mean, n_entities etc. 
  
  ### then behind the scenes, many artificial datasets are generated and frauded
  ### and models are trained. 
  ### output should summarize what was used for training and also the test errors
  ### that were yielded in the lab
  
  ### then trained models are applied to empirical dataset
  
  ### output matrix should then give the predictions (binary, mechanism, degree)
  ### for all used methods 
  
  set.seed(seed)
  
  if (class(data) != "data.frame") 
    stop("provided dataset is not a data.frame object")
  
  if (parallel) {
    cl <- makeCluster(parallel::detectCores(), type = "SOCK")
    registerDoSNOW(cl)
  }
  
  final_models <- list()
  
  #' ------------------------------------
  #  (i) construct artifical data -------
  #' ------------------------------------
  
    # define fraud scenarios that models are trained on 
    fraud_scenarios <- as.data.frame(expand.grid(fraud_incA, fraud_extA, fraud_types))
    colnames(fraud_scenarios) <- c("fraud_incA", "fraud_extA", "type")
    
    # optimize for turnout, shareA
    opt_vecs <- gen_data(n_entities = nrow(data), 
                         eligible = eligible,
                         turnout_emp = turnout_emp, 
                         shareA_emp = shareA_emp,
                         optimize_only = T
    )
    
    # generate n_elections artificial cases under each scenario in sim_scenarios
    Sys.time()
    for (scenario in 1:nrow(fraud_scenarios)) {
      
      output <- gen_data(n_entities = nrow(data), 
                         eligible = eligible,
                         fraud_type = fraud_scenarios[scenario, "type"],
                         fraud_incA = fraud_scenarios[scenario, "fraud_incA"],
                         fraud_extA = fraud_scenarios[scenario, "fraud_extA"],
                         fraud_incB = 0,
                         fraud_extB = 0,
                         fraud_expo = 1.5,
                         agg_factor = 1, 
                         n_elections = n_elections,
                         data_type = "num_char", 
                         shareA_emp = shareA_emp,
                         turnout = opt_vecs$turnout, 
                         shareA = opt_vecs$shareA
      )
      
      ifelse(scenario == 1,
             sim_elections <- output, 
             sim_elections <- rbind(sim_elections, output)
      )
      
      if (scenario %% 100 == 0) 
        print(str_c("generating synthetic data: fraud scenario ", scenario, " out of ", nrow(fraud_scenarios), " simulated, ", Sys.time()))
      
    } # end for scenario in 1:nrow(scenarios)
    
    # add clean scenarios
    # set up in a way that 50% of scenarios are clean, rest is frauded to different degrees
    clean_elections <- gen_data(n_entities = nrow(data), 
                                eligible = eligible,
                                fraud_type = "clean",
                                agg_factor = 1, 
                                n_elections = nrow(sim_elections),
                                data_type = "num_char", 
                                shareA_emp = shareA_emp,
                                turnout = opt_vecs$turnout, 
                                shareA = opt_vecs$shareA
    )
    
    sim_elections <- rbind(sim_elections, clean_elections)
    
  #' ----------------------------
  #  (ii) train ML models -------
  #' ----------------------------
  
    # split data into training set (used for cross-validation) and final test set (untouched)
    # stratify over y-proportions (mechanisms), 80/20 split
    train_id <- createDataPartition(sim_elections$fraud_type,
                                    times = 1,
                                    p = 0.8,
                                    list = FALSE)
    train <- sim_elections[train_id,]
    test <- sim_elections[-train_id,]
    
    # construct empty objects to store results
    test_errors <- as.data.frame(matrix(NA, nrow = 5, ncol = 3))
    rownames(test_errors) <- c("kNN", "ridge", "lasso", "randomForest", "gradBoost")
    colnames(test_errors) <- c("binary", "categorical", "RMSE")
    
    # construct blueprint confusion matrices
    confusion_binary <- list()
    confusion_cat <- list()
    
    # predictions on empirical case from trained models
    predictions <- as.data.frame(matrix(NA, nrow=5, ncol=9))
    rownames(predictions) <- c("kNN", "ridge", "lasso", "randomForest", "gradBoost")
    colnames(predictions) <- c("binary", "p(fraud|X)", "categorical", "p(bbs|X)", "p(stealing|X)", 
                               "p(switching|X)", "p(clean|X)", "perc_frauded", "sd(yhat-y)")
    X_emp <- gen_features(votes_a, votes_b, turnout_emp, shareA_emp, shareB_emp, eligible)
    
    # define variables
    X_train <- model.matrix(fraud ~., train[,-c(2:8)])[,-1]
    X_test <- model.matrix(fraud ~., test[,-c(2:8)])[,-1]
    
    
    y_train_binary <- as.factor(train$fraud)
    y_test_binary <- as.factor(test$fraud)
    y_train_cat <- as.factor(train$fraud_type)
    y_test_cat <- as.factor(test$fraud_type)
    y_train_cont <- train$perc_frauded
    y_test_cont <- test$perc_frauded
    
    # define trControl settings that are agnostic to the specific method
    tr_settings <- trainControl(method = "cv", 
                                number = 5, 
                                repeats=NA, 
                                search="grid")
  
  
    #' ------------------------
    # kNN
    if(is.element("kNN", models)) {
      #' ------------------------
      
      if (is.element("binary", ml_task)) {
        
        # binary 
        binary_knn <- train(
          x=X_train, y=y_train_binary, method="knn", metric="Accuracy", 
          trControl = tr_settings, 
          tuneGrid = expand.grid(k=3:20)
        )
        
        preds_knnP <- binary_knn %>% predict(X_test, type="prob")
        preds_knn <- apply(preds_knnP, 1, which.max)-1
        
        test_errors["kNN", "binary"] <- length(which(y_test_binary != preds_knn)) / length(y_test_binary)
        confusion_binary[["knn"]] <- confusionMatrix(data = as.factor(preds_knn), reference = y_test_binary)
        
        predictions["kNN", "p(fraud|X)"] <- (binary_knn %>% predict(X_emp, type="prob"))[,2]
        predictions["kNN", "binary"] <- ifelse(predictions["kNN", "p(fraud|X)"] > 0.5, "fraud", "clean")
        
        final_models[["kNN"]][["binary"]] <- binary_knn
        
        print("ML training: kNN, binary done. ")
      }
      
      if (is.element("cat", ml_task)) {
        
        # categorical 
        cat_knn <- train(
          x=X_train, y=y_train_cat, method="knn", metric="Accuracy", 
          trControl = tr_settings, 
          tuneGrid = expand.grid(k=3:20)
        )
        
        preds_knnP <- cat_knn %>% predict(X_test, type="prob")
        preds_knn <- colnames(preds_knnP)[apply(preds_knnP, 1, which.max)]
        
        test_errors["kNN", "categorical"] <- length(which(y_test_cat != preds_knn)) / length(y_test_cat)
        confusion_cat[["knn"]] <- confusionMatrix(data = factor(preds_knn, levels=levels(y_test_cat)), reference = y_test_cat)
        
        predictions["kNN", "categorical"] <- names(which.max((cat_knn %>% predict(X_emp, type="prob"))))
        predictions["kNN", 4:7] <- cat_knn %>% predict(X_emp, type="prob")
        
        final_models[["kNN"]][["categorical"]] <- cat_knn
        
        print("ML training: kNN, categorical done. ")
      }
      
      if (is.element("cont", ml_task)) {
      
        # continuous 
        cont_knn <- train(
          x=X_train, y=y_train_cont, method="knn", metric="RMSE", 
          trControl = tr_settings, 
          tuneGrid = expand.grid(k=3:20)
        )
        
        preds_knn <- cont_knn %>% predict(X_test)
        test_errors["kNN", "RMSE"] <-  RMSE(preds_knn, y_test_cont)
        
        predictions["kNN", "perc_frauded"] <- cont_knn %>% predict(X_emp)
        predictions["kNN", "sd(yhat-y)"] <- sd(preds_knn - y_test_cont)
        
        final_models[["kNN"]][["continuous"]] <- cont_knn
        
        print("ML training: kNN, continuous done. ")
      }
        
    } # end kNN
    
    
    #' ------------------------
    # ridge/lasso regression 
    if(is.element("regul_reg", models)) {
    #' ------------------------
    
      if (is.element("binary", ml_task)) {  
      
        # binary
        binary_regul <- train(
          x=X_train, y=y_train_binary, method="glmnet", metric="Accuracy",
          trControl = tr_settings, 
          tuneGrid = expand.grid(alpha=c(0,1), lambda=10^seq(3, -3, length=100)) 
        )
        binary_ridge <- update(binary_regul, 
                               param=list(alpha=0, lambda=binary_regul$results$lambda[which(binary_regul$results$Accuracy[binary_regul$results$alpha==0] == max(binary_regul$results$Accuracy[binary_regul$results$alpha==0]))[1]]))
        binary_lasso <- update(binary_regul, 
                               param=list(alpha=1, lambda=binary_regul$results$lambda[which(binary_regul$results$Accuracy[binary_regul$results$alpha==1] == max(binary_regul$results$Accuracy[binary_regul$results$alpha==1]))[1]]))
        
        preds_ridgeP <- binary_ridge %>% predict(X_test, type="prob")
        preds_ridge <- apply(preds_ridgeP, 1, which.max)-1
        
        preds_lassoP <- binary_lasso %>% predict(X_test, type="prob")
        preds_lasso <- apply(preds_lassoP, 1, which.max)-1
        
        test_errors["ridge", "binary"] <- length(which(y_test_binary != preds_ridge)) / length(y_test_binary)
        test_errors["lasso", "binary"] <- length(which(y_test_binary != preds_lasso)) / length(y_test_binary)
        
        confusion_binary[["ridge"]] <- confusionMatrix(data = as.factor(preds_ridge), reference = y_test_binary)
        confusion_binary[["lasso"]] <- confusionMatrix(data = as.factor(preds_lasso), reference = y_test_binary)
        
        predictions["ridge", "p(fraud|X)"] <- (binary_ridge %>% predict(X_emp, type="prob"))[,2]
        predictions["ridge", "binary"] <- ifelse(predictions["ridge", "p(fraud|X)"] > 0.5, "fraud", "clean")
        
        predictions["lasso", "p(fraud|X)"] <- (binary_lasso %>% predict(X_emp, type="prob"))[,2]
        predictions["lasso", "binary"] <- ifelse(predictions["lasso", "p(fraud|X)"] > 0.5, "fraud", "clean")
        
        final_models[["ridge"]][["binary"]] <- binary_ridge
        final_models[["lasso"]][["binary"]] <- binary_lasso
        
        print("ML training: regularized regression, binary done.")
      }  
      
      if (is.element("cat", ml_task)) {
      
        # categorical
        cat_regul <- train(
          x=X_train, y=y_train_cat, method="glmnet", metric="Accuracy",
          trControl = tr_settings, 
          tuneGrid=expand.grid(alpha=c(0,1), lambda=10^seq(3, -3, length=100)) 
        )
        cat_ridge <- update(cat_regul, 
                            param=list(alpha=0, lambda=cat_regul$results$lambda[which(cat_regul$results$Accuracy[cat_regul$results$alpha==0] == max(cat_regul$results$Accuracy[cat_regul$results$alpha==0]))[1]]))
        cat_lasso <- update(cat_regul, 
                            param=list(alpha=1, lambda=cat_regul$results$lambda[which(cat_regul$results$Accuracy[cat_regul$results$alpha==1] == max(cat_regul$results$Accuracy[cat_regul$results$alpha==1]))[1]]))
        
        preds_ridgeP <- cat_ridge %>% predict(X_test, type="prob")
        preds_ridge <- colnames(preds_ridgeP)[apply(preds_ridgeP, 1, which.max)]
        
        preds_lassoP <- cat_lasso %>% predict(X_test, type="prob")
        preds_lasso <- colnames(preds_lassoP)[apply(preds_lassoP, 1, which.max)]
        
        test_errors["ridge", "categorical"] <- length(which(y_test_cat != preds_ridge)) / length(y_test_cat)
        test_errors["lasso", "categorical"] <- length(which(y_test_cat != preds_lasso)) / length(y_test_cat)
        
        confusion_cat[["ridge"]] <- confusionMatrix(data = as.factor(preds_ridge), reference = y_test_cat)
        confusion_cat[["lasso"]] <- confusionMatrix(data = as.factor(preds_lasso), reference = y_test_cat)
        
        predictions["ridge", "categorical"] <- names(which.max((cat_ridge %>% predict(X_emp, type="prob"))))
        predictions["ridge", 4:7] <- cat_ridge %>% predict(X_emp, type="prob")
        
        predictions["lasso", "categorical"] <- names(which.max((cat_lasso %>% predict(X_emp, type="prob"))))
        predictions["lasso", 4:7] <- cat_lasso %>% predict(X_emp, type="prob")
        
        final_models[["ridge"]][["categorical"]] <- cat_ridge
        final_models[["lasso"]][["categorical"]] <- cat_lasso
        
        print("ML training: regularized regression, categorical done.")
      }  
      
      if (is.element("cont", ml_task)) {
      
        # continuous
        cont_regul <- train(
          x=X_train, y=y_train_cont, method="glmnet", metric="RMSE",
          trControl = tr_settings, 
          tuneGrid=expand.grid(alpha=c(0,1), lambda=10^seq(3, -3, length=100)) 
        )
        cont_ridge <- update(cont_regul, 
                             param=list(alpha=0, lambda=cont_regul$results$lambda[which(cont_regul$results$RMSE[cont_regul$results$alpha==0] == min(cont_regul$results$RMSE[cont_regul$results$alpha==0]))[1]]))
        cont_lasso <- update(cont_regul, 
                             param=list(alpha=1, lambda=cont_regul$results$lambda[which(cont_regul$results$RMSE[cont_regul$results$alpha==1] == min(cont_regul$results$RMSE[cont_regul$results$alpha==1]))[1]]))
        
        preds_ridge <- cont_ridge %>% predict(X_test)
        preds_lasso <- cont_lasso %>% predict(X_test)
        
        test_errors["ridge", "RMSE"] <- RMSE(preds_ridge, y_test_cont)
        test_errors["lasso", "RMSE"] <- RMSE(preds_lasso, y_test_cont)
        
        predictions["ridge", "perc_frauded"] <- cont_ridge %>% predict(X_emp)
        predictions["ridge", "sd(yhat-y)"] <- sd(preds_ridge - y_test_cont)
        
        predictions["lasso", "perc_frauded"] <- cont_lasso %>% predict(X_emp)
        predictions["lasso", "sd(yhat-y)"] <- sd(preds_lasso - y_test_cont)
        
        final_models[["ridge"]][["continuous"]] <- cont_ridge
        final_models[["lasso"]][["continuous"]] <- cont_lasso
        
        print("ML training: regularized regression, continuous done.")
      }
    } # end regul_reg
    
    
    #' ------------------------
    # random Forest
    if(is.element("randomForest", models)) {
    #' ------------------------
    
      if (is.element("binary", ml_task)) {  
      
        # binary 
        binary_rf <- train(
          x=X_train, y=y_train_binary, method="rf", metric="Accuracy", 
          trControl = tr_settings, 
          tuneGrid = expand.grid(mtry=c(3:20))
        )
        
        preds_rfP <- binary_rf %>% predict(X_test, type="prob")
        preds_rf <- apply(preds_rfP, 1, which.max)-1
        
        test_errors["randomForest", "binary"] <- length(which(y_test_binary != preds_rf)) / length(y_test_binary)
        confusion_binary[["randomForest"]] <- confusionMatrix(data = as.factor(preds_rf), reference = y_test_binary)
        
        predictions["randomForest", "p(fraud|X)"] <- (binary_rf %>% predict(X_emp, type="prob"))[,2]
        predictions["randomForest", "binary"] <- ifelse(predictions["randomForest", "p(fraud|X)"] > 0.5, "fraud", "clean")
        
        final_models[["randomForest"]][["binary"]] <- binary_rf
        
        print("ML training: randomForest, binary done.")
      } 
        
      if (is.element("cat", ml_task)) {
        
        # categorical 
        cat_rf <- train(
          x=X_train, y=y_train_cat, method="rf", metric="Accuracy", 
          trControl = tr_settings, 
          tuneGrid = expand.grid(mtry=c(3:20))
        )
        
        preds_rfP <- cat_rf %>% predict(X_test, type="prob")
        preds_rf <- colnames(preds_rfP)[apply(preds_rfP, 1, which.max)]
        
        test_errors["randomForest", "categorical"] <- length(which(y_test_cat != preds_rf)) / length(y_test_cat)
        confusion_cat[["randomForest"]] <- confusionMatrix(data = as.factor(preds_rf), reference = y_test_cat)
        
        predictions["randomForest", "categorical"] <- names(which.max((cat_rf %>% predict(X_emp, type="prob"))))
        predictions["randomForest", 4:7] <- cat_rf %>% predict(X_emp, type="prob")
        
        final_models[["randomForest"]][["categorical"]] <- cat_rf
        
        print("ML training: randomForest, categorical done.")
      }
      
      if (is.element("cont", ml_task)) {
        
        # continuous 
        cont_rf <- train(
          x=X_train, y=y_train_cont, method="rf", metric="RMSE", 
          trControl = tr_settings, 
          tuneGrid = expand.grid(mtry=c(3:20))
        )
        
        preds_rf <- cont_rf %>% predict(X_test)
        test_errors["randomForest", "RMSE"] <- RMSE(preds_rf, y_test_cont)
        
        predictions["randomForest", "perc_frauded"] <- cont_rf %>% predict(X_emp)
        predictions["randomForest", "sd(yhat-y)"] <- sd(preds_rf - y_test_cont)
        
        final_models[["randomForest"]][["continuous"]] <- cont_rf
        
        print("ML training: randomForest, continuous done.")
      }
      
    } # end randomForest
    
    
    #' ------------------------------------
    # gradient boosting
    if(is.element("gradBoost", models)) {
    #' ------------------------------------
    
      if (is.element("binary", ml_task)) {  
        
        # binary 
        binary_boost <- train(
          x=X_train, y=y_train_binary, method="xgbTree", metric="Accuracy", 
          trControl = tr_settings, 
          tuneGrid = expand.grid(eta = c(0.05, 0.075, 0.1),
                                 nrounds = c(50, 75, 100),
                                 max_depth = 6:8,
                                 min_child_weight = 2.2,
                                 colsample_bytree = 0.4,
                                 gamma = 0,
                                 subsample = 1)
        )
        preds_boostP <- binary_boost %>% predict(X_test, type="prob")
        preds_boost <-  apply(preds_boostP, 1, which.max)-1
        
        test_errors["gradBoost", "binary"] <- length(which(y_test_binary != preds_boost)) / length(y_test_binary)
        confusion_binary[["gradBoost"]] <- confusionMatrix(data = as.factor(preds_boost), reference = y_test_binary)
        
        predictions["gradBoost", "p(fraud|X)"] <- (binary_boost %>% predict(X_emp, type="prob"))[,2]
        predictions["gradBoost", "binary"] <- ifelse(predictions["gradBoost", "p(fraud|X)"] > 0.5, "fraud", "clean")
        
        final_models[["gradBoost"]][["binary"]] <- binary_boost
        
        print("ML training: gradBoost, binary done.")
      }
      
      if (is.element("cat", ml_task)) {  
      
        # categorical 
        cat_boost <- train(
          x=X_train, y=y_train_cat, method="xgbTree", metric="Accuracy", 
          trControl = tr_settings, 
          tuneGrid = expand.grid(eta = c(0.05, 0.075, 0.1),
                                 nrounds = c(50, 75, 100),
                                 max_depth = 6:8,
                                 min_child_weight = 2.2,
                                 colsample_bytree = 0.4,
                                 gamma = 0,
                                 subsample = 1)
        )
        preds_boostP <- cat_boost %>% predict(X_test, type="prob")
        preds_boost <- colnames(preds_boostP)[apply(preds_boostP, 1, which.max)]
        
        test_errors["gradBoost", "categorical"] <- length(which(y_test_cat != preds_boost)) / length(y_test_cat)
        confusion_cat[["gradBoost"]] <- confusionMatrix(data = as.factor(preds_boost), reference = y_test_cat)
        
        predictions["gradBoost", "categorical"] <- names(which.max((cat_boost %>% predict(X_emp, type="prob"))))
        predictions["gradBoost", 4:7] <- cat_boost %>% predict(X_emp, type="prob")
        
        final_models[["gradBoost"]][["categorical"]] <- cat_boost
        
        print("ML training: gradBoost, categorical done.")
      }  
      
      if (is.element("cont", ml_task)) {  
      
        # continuous 
        cont_boost <- train(
          x=X_train, y=y_train_cont, method="xgbTree", metric="RMSE", 
          trControl = tr_settings, 
          tuneGrid = expand.grid(eta = c(0.05, 0.075, 0.1),
                                 nrounds = c(50, 75, 100),
                                 max_depth = 6:8,
                                 min_child_weight = 2.2,
                                 colsample_bytree = 0.4,
                                 gamma = 0,
                                 subsample = 1)
        )
        
        preds_boost <- cont_boost %>% predict(X_test)
        test_errors["gradBoost", "RMSE"] <- RMSE(preds_boost, y_test_cont)
        
        predictions["gradBoost", "perc_frauded"] <- cont_boost %>% predict(X_emp)
        predictions["gradBoost", "sd(yhat-y)"] <- sd(preds_boost - y_test_cont)
        
        final_models[["gradBoost"]][["continuous"]] <- cont_boost
        
        print("ML training: gradBoost, continuous done.")
      }
    } # end gradBoost
    
    # end parallelization 
    if(parallel)
      stopCluster(cl) 
  
    
  #' ---------------------------
  #  (iii) output objects -------
  #' ---------------------------
  
    if(is.element("regul_reg", models)) {
      models <- models[-which(models=="regul_reg")]
      models <- c(models, "ridge", "lasso")
    }  
    
    predictions <- predictions[models,]
    
    out_list <- list("performance in the laboratory setting, test errors" = test_errors, 
                     "predictions for case" = predictions, 
                     "final models" = final_models, 
                     "simulated elections" = sim_elections)
    return(out_list)
  
}

