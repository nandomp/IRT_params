
###############################################
################# LIBRARIES ###################
###############################################
options( java.parameters = "-Xmx6g" )
.lib<- c("ggplot2","caret","plyr","dplyr","RWeka", "sampling","gbm", "rpart", "e1071","randomForest","foreach","MASS","mlbench",
         "rrlda","C50", "MASS","RSNNS", "class", "kernlab","party", "sda", "rrcov", "robustbase", "earth", "mda", 
         "ada", "caTools", "adabag","ipred", "bst", "randomForest", "RRF", "pls", "KODAMA")

.inst <- .lib %in% installed.packages()
if (length(.lib[!.inst])>0) install.packages(.lib[!.inst], repos=c("http://rstudio.org/_packages", "http://cran.rstudio.com")) 
lapply(.lib, require, character.only=TRUE)

#remove.packages( installed.packages( priority = "NA" )[,1] )
#install.packages("ada", repos=c("http://rstudio.org/_packages", "http://cran.rstudio.com")) 
modelF <- NA
testF <- NA
trainF <- NA
###############################################
############# OPERATING OPTIONS ###############
###############################################
set.seed(998)
args <- commandArgs(trailingOnly = TRUE)

OptimimalParam = TRUE
binaryClass = FALSE
weArePlaying = TRUE

ControlExperiment = FALSE

#Temp <- "_incrementalNoise_/"
Temp <- "_Toy_/"

if (weArePlaying){
  ds <- Temp
  
  }else{
    if (binaryClass) {
      ds<-"_Binary_/"
    }else{
      ds<-"_Multiclass_/"
    }
    
}

datasets <- list.files(ds, pattern = "*csv$")


if (ControlExperiment){
  methods <- c("gt90%_A","gt90%_B","gt90%_C",
               "gt80%_A","gt80%_B","gt80%_C",
               "gt70%_A","gt70%_B","gt70%_C",
               "gt60%_A","gt60%_B","gt60%_C",
               "gt50%_A","gt50%_B","gt50%_C",
               "gt40%_A","gt40%_B","gt40%_C",
               "gt30%_A","gt30%_B","gt30%_C",
               "gt20%_A","gt20%_B","gt20%_C",
               "gt10%_A","gt10%_B","gt10%_C",
               "RandomClass_A", "RandomClass_B", "RandomClass_C",
               "MajorityClass", "MinorityClass", 
               "OptimalClass","PessimalClass")
}else{
  
#methods<-c("simpls_ncomp3")
  methods<-c("c5.0", "c5.0_winnow", "J48", "J48Unp", "LMT", "LMT_CV", "LMT_AIC", "rpart", "ctree_c0.01",
             "ctree_c0.05", "ctree_c0.99", "JRip", "JRip_Unp","PART", "sda_L0.0", "sda_L0.5", "sda_L1.0",
             "fda_prune2", "fda_prune9", "fda_prune17", "mda_subc2", "mda_subc3", "mda_subc4",
             "W_NB", "NB", "NB_laplace", "rbf", "mlp_1", "mlp_3", "mlp_5", "mlp_7", "mlp_9", "avNNet_decay1e04",
             "avNNet_decay01", "avNNet_decay0", "pcaNNet", "lvq_1", "lvq_3", "lvq_5", "SMV", "svmRadialCost_C0.01",
             "svmRadialCost_C0.1", "svmRadialCost_C1", "svmRadialCost_C2", "svmLinear_C0.01", "svmLineart_C0.1",
             "svmLinear_C1", "svmLinear_C2", "svmLinear_C4", "svmLinear_C8", "svmPoly_d_1_s_0.001", "svmPoly_d_1_s_0.01",
             "svmPoly_d_1_s_0.1", "svmPoly_d_2_s_0.001", "svmPoly_d_2_s_0.01", "svmPoly_d_2_s_0.1", "svmPoly_d_3_s_0.001",
             "svmPoly_d_3_s_0.01", "svmPoly_d_3_s_0.1", "gbm_1_50", "gbm_1_100", "gbm_1_150", "gbm_2_50", "gbm_2_100", "gbm_2_150",
             "gbm_3_50", "gbm_3_100", "gbm_3_150", "treeBag", "bagFDA_prune2", "bagFDA_prune4", "bagFDA_prune8",
             "bagFDA_prune16", "rf_mtry2", "rf_mtry4", "rf_mtry8",
             "rf_mtry16", "rf_mtry32", "rf_mtry64", "rf_mtry128", "rrf_mtry2", "rrf_mtry4", "rrf_mtry8", "rrf_mtry16",
             "rrf_mtry32", "rrf_mtry64", "rrf_mtry128", "cforest_mtry2", "cforest_mtry4", "cforest_mtry8", "cforest_mtry16",
             "cforest_mtry32", "cforest_mtry64", "cforest_mtry128", "parRF_mtry2","parRF_mtry4", "parRF_mtry8", "parRF_mtry16",
             "parRF_mtry32", "parRF_mtry64", "parRF_mtry128", "knn_k1", "knn_k2", "knn_k3", "knn_k5", "knn_k7", "knn_k9",
             "Ibk_k1", "Ibk_k2", "Ibk_k3", "Ibk_k5", "Ibk_k7", "Ibk_k9", "pls_ncomp1", "pls_ncomp2",
             "simpls_ncomp1", "simpls_ncomp2", "gcvEarth_d1", "gcvEarth_d2", "gcvEarth_d3",
             "RandomClass_A", "RandomClass_B", "RandomClass_C","MajorityClass", "MinorityClass", "OptimalClass","PessimalClass")
}



# METHODS THAT FAIL: "RLDA", "pls_ncomp3", "simpls_ncomp3",
#"ada_iter_50_depth_1", "ada_iter_50_depth_2", "ada_iter_50_depth_3", 
#"ada_iter_100_depth_1", "ada_iter_100_depth_2", "ada_iter_100_depth_3", "ada_iter_150_depth_1", "ada_iter_150_depth_2", 
#"ada_iter_150_depth_3"
#"blackboost_maxdepth1", "blackboost_maxdepth2", "blackboost_maxdepth3", "pls_ncomp5", "pls_ncomp10", "simpls_ncomp5", "simpls_ncomp10",
# "qda", "QdaCov","logitBoost_i11", "logitBoost_i21", "logitBoost_i31",

nfolds<-5

ListDS_Results <- list()

start.time <- Sys.time()

for (id in 1:length(datasets))
{
  selected <- id
  #datos <- read.arff(paste(ds,datasets[selected],sep=""))
  datos <- read.csv(paste(ds,datasets[selected],sep=""))
  
  colnames(datos)[ncol(datos)]<-"Class"  #Changed by Adolfo: 04/04/2016
  datos$Class <- as.factor(datos$Class)
  
  posParamEstudio<-length(datos[1,])
  nomParamEstudio<-names(datos)[posParamEstudio]
  
  tam<-length(datos[,1])
  matrixDS<- matrix(NA, nrow = tam,ncol=length(methods))
  colnames(matrixDS) <- methods
  
  #w<-datos[sample(nrow(datos)),]
  w<-datos
  
  folds<-createFolds(w[[nomParamEstudio]], k = nfolds)
  
  print(paste("DS:  ",datasets[selected]))
  for (ik in 1:nfolds)
  {
    
    indiceStrat<-folds[[ik]]
    train<-w[-indiceStrat,]
    test<-w[indiceStrat,]
    ob<-paste(nomParamEstudio,"~.",sep="")
    
    for (im in 1:length(methods))
    {
      ERROR = FALSE
      msel<-im
      print(paste("Dataset: ",datasets[selected],", Method: ", methods[msel],", fold: ",ik))
      
      
      ########################
      ###  Decision Trees  ###
      ######################## 
      
      if (methods[msel]=="c5.0") {
        ERROR <-  tryCatch(model <- C5.0(Class ~., data = train), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        }
        
        
      }
     
       # winnow = TRUE -> pre-selection of attributes that will be used to contruct the decision tree/ruleset
      if (methods[msel]=="c5.0_winnow"){ 
        ERROR <-  tryCatch(model <- C5.0(Class ~., data = train, control = C5.0Control(winnow = TRUE)), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        }
      
        
      }
      
      if (methods[msel]=="J48"){
        ERROR <-  tryCatch(model <- J48(Class ~., data = train, control = Weka_control(U = FALSE,A=TRUE)), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})        
        }

      }
      
      if (methods[msel]=="J48_Unp"){
        ERROR <-  tryCatch(model <- J48(Class ~., data = train, control = Weka_control(U = TRUE,A=TRUE)), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        }
        
      }
      
      # Classifier for building 'logistic model trees', which are classification trees with logistic regression functions at the leaves. The algorithm can deal with binary and multi-class target variables, numeric and nominal attributes and missing values.
      if (methods[msel]=="LMT"){
        ERROR <-  tryCatch(model <- LMT(Class ~., data = train), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})       
        }
        
      }
      
      # Use cross-validation for boosting at all nodes (i.e., disable heuristic)
      if (methods[msel]=="LMT_CV"){
        ERROR <-  tryCatch(model <- LMT(Class ~., data = train, control = Weka_control(C = TRUE)), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        }
      }
      
      # The AIC is used to choose the best iteration.
      if (methods[msel]=="LMT_AIC"){
        ERROR <-  tryCatch(model <- LMT(Class ~., data = train, control = Weka_control(A = TRUE)), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        }
      }
      
      #Recursive Partitioning and Regression Trees 
      if (methods[msel]=="rpart") {
        ERROR <-  tryCatch(model <- rpart(Class ~., data = train, method = "class"), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
          if (is.matrix(preds)){
            preds <-  as.factor(as.vector(apply(preds,1,function (x) {colnames(preds)[which.max(x)]})))
          }
        }
          # printcp(Fit2) # display the results
          # plotcp(Fit2) # visualize cross-validation results
          # summary(Fit2) # detailed summary of splits
          # testPred <- predict(Fit2, test)
          # testPred <- as.factor(as.vector(apply(testPred,1,function (x) {colnames(testPred)[which.max(x)]})))
      }
      
        # conditional inference trees by recursively making binary splittings on the variables with the highest association to the class (measured by a statistical test). The threshold in the association measure is given by the parameter mincriterion
      if (methods[msel] == "ctree_c0.01"){
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "ctree", tuneGrid=data.frame(mincriterion=0.01), trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        }
      }
      
      # mincriterion = 0.5
      if (methods[msel] == "ctree_c0.5"){
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "ctree", tuneGrid=data.frame(mincriterion=0.5), trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        }
      }
      
      # mincriterion = 0.99
      if (methods[msel] == "ctree_c0.99"){
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "ctree", tuneGrid=data.frame(mincriterion=0.99), trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        }
      }
      
      ##########################
      ### Rule-based methods ###
      ##########################
      
      # RWeka algorithm; JRip implements a propositional rule learner
      if (methods[msel]=="jRip"){ 
         ERROR <-  tryCatch(model <-JRip(Class ~., data = train), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        }
      }
      
      # RWeka algorithm; JRip implements a propositional rule learner (unpruned)
      if (methods[msel]=="JRip_Unp"){
         ERROR <-  tryCatch(model <-JRip(Class ~., data = train, control = Weka_control(E = TRUE)), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        }
      }
      
      # RWeka algorithm: PART generates PART decision lists using the approach of Frank and Witten 
      if (methods[msel] == "PART"){
         ERROR <-  tryCatch(model <- PART(Class ~., data = train), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        }
      }
      
      #############################
      ### Discriminant analysis ###
      #############################
      
      if (methods[msel] == "RLDA") {
        ERROR <-  tryCatch(model <- rrlda(train[,-ncol(train)], train[,ncol(train)]), error = function(e) {return(TRUE)})
        
        if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
          
        }else{
          preds<- tryCatch(predict(model, test[,1:(ncol(test)-1)]), error = function(e) {print(e);return(rep(FALSE, nrow(test)))})
          if (is.list(preds)){
            preds <-  preds$class
          }
        }
        
      }
      
      
      # It performs LDA or diagonal discriminant analysis (DDA) with variable selection using CAT (Correlation-Adjusted T) scores. The best classifier (LDA or DDA) is selected.
      if (methods[msel] == "sda_L0.0") {
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "sda", tuneGrid=data.frame(diagonal = FALSE, lambda = 0.0), trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        }
      }
      # lambda = 0.5
      if (methods[msel] == "sda_L0.5") {
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "sda", tuneGrid=data.frame(diagonal = FALSE, lambda = 0.5), trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        }
      }
      
      # lambda = 1
      if (methods[msel] == "sda_L1.0") {
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "sda", tuneGrid=data.frame(diagonal = FALSE, lambda = 1.0), trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        }
      }
      
      # quadratic discriminant analysis
      # if (methods[msel] == "qda") {
      #    ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "qda", trControl = trainControl(method="none"))
      #   preds<-predict(model, newdata = test)
      # }
      # 
      # # Robust QDA
      # if (methods[msel] == "QdaCov") {
      #    ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "qda", trControl = trainControl(method="none"))
      #   preds<-predict(model, newdata = test)
      # }
      
      # flexible discriminant analysis. prune = 2
      if (methods[msel] == "fda_prune2") {
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "fda", tuneGrid=data.frame(nprune = 2, degree = 1), trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        }
      }
      
      # flexible discriminant analysis. prune = 9
      if (methods[msel] == "fda_prune9") {
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "fda", tuneGrid=data.frame(nprune = 9, degree = 1), trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        }
      }
      
      # flexible discriminant analysis. prune = 17
      if (methods[msel] == "fda_prune17") {
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "fda", tuneGrid=data.frame(nprune = 17, degree = 1), trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        }
      }
      
      # mixture discriminant analysis. subclasses = 2
      if (methods[msel] == "mda_subc2") {
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "mda", tuneGrid=data.frame(subclasses = 2), trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        }
      }
      
      # subclasses = 3
      if (methods[msel] == "mda_subc3") {
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "mda", tuneGrid=data.frame(subclasses = 3), trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        }
      }
      
      # subclasses = 4
      if (methods[msel] == "mda_subc4") {
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "mda", tuneGrid=data.frame(subclasses = 4), trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        }
      }
      
      
      ################
      ### Bayesian ###
      ################
      
      if (methods[msel] == "W_NB")
      {
        NB <- make_Weka_classifier("weka/classifiers/bayes/NaiveBayes")
         ERROR <-  tryCatch(model <- NB(Class ~., data = train), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        }
      }
      
      if (methods[msel] == "NB") {
         ERROR <-  tryCatch(model <- naiveBayes(train[,-ncol(train)], train[,ncol(train)]), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, test[,1:ncol(test)-1]), error = function(e) {return(rep(FALSE, nrow(test)))})
       
        }
      }
        
      if (methods[msel] == "NB_laplace") {
         ERROR <-  tryCatch(model <- naiveBayes(train[,-ncol(train)], train[,ncol(train)], laplace = 3), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, test[,1:ncol(test)-1]), error = function(e) {return(rep(FALSE, nrow(test)))})
       
        }
      }
    
      

      #######################
      ### Neural Networks ###
      #######################
      
      # Radial Basis Function Network with negative threshold
      if (methods[msel] == "rbf") {
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "rbfDDA", tuneGrid=data.frame(negativeThreshold=0.001), trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})

        }
      }
      
      # Multilayer perceptron wiht 1 unit in the hidden layer
      if (methods[msel] == "mlp_1") {
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "mlp", tuneGrid=data.frame(size=1), trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        
        }
      }
      
      # Multilayer perceptron wiht 3 unit in the hidden layer
      if (methods[msel] == "mlp_3") {
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "mlp", tuneGrid=data.frame(size=3), trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        
        }
      }
      
      # Multilayer perceptron wiht 5 unit in the hidden layer
      if (methods[msel] == "mlp_5") {
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "mlp", tuneGrid=data.frame(size=5), trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        
        }
      }
      
      # Multilayer perceptron wiht 7 unit in the hidden layer
      if (methods[msel] == "mlp_7") {
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "mlp", tuneGrid=data.frame(size=7), trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        
        }
      }
      
      # Multilayer perceptron wiht 9 unit in the hidden layer
      if (methods[msel] == "mlp_9") {
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "mlp", tuneGrid=data.frame(size=9), trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        
        }
        }
      
      # committee of 5 MLPs (the number of MLPs is given by parameter repeat) trained with different random weight initializations and bag=false. decay = 1e-04
      if (methods[msel] == "avNNet_decay1e04") {
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "avNNet", tuneGrid=data.frame(size=5, decay = 1e-04, bag = FALSE ), trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        
        }
      }
      
      # committee of 5 MLPs (the number of MLPs is given by parameter repeat) trained with different random weight initializations and bag=false. decay = 0.1
      if (methods[msel] == "avNNet_decay01") {
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "avNNet", tuneGrid=data.frame(size=5, decay = 0.1, bag = FALSE ), trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        
        }
      }
      
      # committee of 5 MLPs (the number of MLPs is given by parameter repeat) trained with different random weight initializations and bag=false. decay = 0
      if (methods[msel] == "avNNet_decay0") {
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "avNNet", tuneGrid=data.frame(size=5, decay = 0, bag = FALSE ), trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        
        }
      }
      
      # trains the MLP using caret and the nnet package, but running principal component analysis (PCA) previously on the data set
      if (methods[msel] == "pcaNNet") {
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "pcaNNet", tuneGrid=data.frame(size=5, decay = 1e-04), trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        
        }
      }
      
      # learning vector quantization implemented using the function lvq in the class package, with codebook of size 50, and k=1 nearest neighbors.
      if (methods[msel] == "lvq_1") {
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "lvq", tuneGrid=data.frame(size=50, k = 1), trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        
        }
      }
      
      # k=3 nearest neighbors.
      if (methods[msel] == "lvq_3") {
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "lvq", tuneGrid=data.frame(size=50, k = 3), trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        
        }
      }
      
      # k=5 nearest neighbors.
      if (methods[msel] == "lvq_5") {
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "lvq", tuneGrid=data.frame(size=50, k = 5), trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        
        }
      }
      
      ###############################
      ### Support Vector Machines ###
      ###############################
      
      # SMO implements John C. Platt's sequential minimal optimization algorithm for training a support vector classifier using polynomial or RBF kernels. Multi-class problems are solved using pairwise classification.
      if (methods[msel] == "SMV") {
         ERROR <-  tryCatch(model <- SMO(Class ~., data = train), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        
        }
      }
      
      # SVM with Gaussian kernel, the spread of the Gaussian kernel is calculated automatically, cost C = 0.01,
      if (methods[msel] == "svmRadialCost_C0.01") {
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "svmRadialCost", tuneGrid=data.frame(C=0.01), trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        
        }
      }
      # cost C = 0.1
      if (methods[msel] == "svmRadialCost_C0.1") {
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "svmRadialCost", tuneGrid=data.frame(C=0.1), trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        
        }
      }
      
      # cost C = 1
      if (methods[msel] == "svmRadialCost_C1") {
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "svmRadialCost", tuneGrid=data.frame(C=1), trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        
        }
      } 
      
      # cost C = 2
      if (methods[msel] == "svmRadialCost_C2") {
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "svmRadialCost", tuneGrid=data.frame(C=2), trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
          }
        }
      
      # uses the function ksvm (kernlab package) with linear kernel tuning C =0.01
      if (methods[msel] == "svmLinear_C0.01") {
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "svmLinear", tuneGrid=data.frame(C=0.01), trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        
        }
      }
      # cost C = 0.1
      if (methods[msel] == "svmLineart_C0.1") {
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "svmLinear", tuneGrid=data.frame(C=0.1), trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        
        }
      }
      
      # cost C = 1
      if (methods[msel] == "svmLinear_C1") {
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "svmLinear", tuneGrid=data.frame(C=1), trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        
        }
      }
      
      # cost C = 2
      if (methods[msel] == "svmLinear_C2") {
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "svmLinear", tuneGrid=data.frame(C=2), trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        
        }
      }
      
      # cost C = 4
      if (methods[msel] == "svmLinear_C4") {
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "svmLinear", tuneGrid=data.frame(C=4), trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        
        }
      }
      
      # cost C = 8
      if (methods[msel] == "svmLinear_C8") {
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "svmLinear", tuneGrid=data.frame(C=8), trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        
        }
      }
      
      # linear, quadratic and cubic kernels. Degree = 1. Scale = 0.001
      if (methods[msel] == "svmPoly_d_1_s_0.001") {
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "svmPoly", tuneGrid=data.frame(degree=1, scale= 0.001, C = 1), trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        
        }
      }
      
      # linear, quadratic and cubic kernels. Degree = 1. Scale = 0.01
      if (methods[msel] == "svmPoly_d_1_s_0.01") {
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "svmPoly", tuneGrid=data.frame(degree=1, scale= 0.01, C = 1), trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        
      }
      }
      # linear, quadratic and cubic kernels. Degree = 1. Scale = 0.1
      if (methods[msel] == "svmPoly_d_1_s_0.1") {
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "svmPoly", tuneGrid=data.frame(degree=2, scale= 0.1, C = 1), trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        
        }
      }
      
      # linear, quadratic and cubic kernels. Degree = 2. Scale = 0.001
      if (methods[msel] == "svmPoly_d_2_s_0.001") {
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "svmPoly", tuneGrid=data.frame(degree=2, scale= 0.001, C = 1), trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        
        }
      }
      
      # linear, quadratic and cubic kernels. Degree = 2. Scale = 0.01
      if (methods[msel] == "svmPoly_d_2_s_0.01") {
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "svmPoly", tuneGrid=data.frame(degree=2, scale= 0.01, C = 1), trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        
        }
      }
      
      # linear, quadratic and cubic kernels. Degree = 2. Scale = 0.1
      if (methods[msel] == "svmPoly_d_2_s_0.1") {
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "svmPoly", tuneGrid=data.frame(degree=1, scale= 0.1, C = 1), trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        
        }
      }
      
      # linear, quadratic and cubic kernels. Degree = 3. Scale = 0.001
      if (methods[msel] == "svmPoly_d_3_s_0.001") {
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "svmPoly", tuneGrid=data.frame(degree=3, scale= 0.001, C = 1), trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        
        }
      }
      
      # linear, quadratic and cubic kernels. Degree = 3. Scale = 0.01
      if (methods[msel] == "svmPoly_d_3_s_0.01") {
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "svmPoly", tuneGrid=data.frame(degree=3, scale= 0.01, C = 1), trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        
        }
      }
      
      # linear, quadratic and cubic kernels. Degree = 3. Scale = 0.1 #62
      if (methods[msel] == "svmPoly_d_3_s_0.1") {
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "svmPoly", tuneGrid=data.frame(degree=3, scale= 0.1, C = 1), trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        }
      }
      
      ##################
      ###  Boosting  ###
      ##################
      
      # adaboost.M1 method (Freund and Schapire, 1996) to create an adaboost ensemble of classification trees 
      # DOES NOT SUPPORT MULTICLASS PROBLEMS
      
      # if (methods[msel] == "ada_iter_50_depth_1") {
      #    ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "ada", tuneGrid=data.frame(iter = 50,maxdepth= 1, nu=0.1), trControl = trainControl(method="none"))
      #   preds <- predict(model, test)
      # }
      # if (methods[msel] == "ada_iter_50_depth_2") {
      #    ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "ada", tuneGrid=data.frame(iter = 50,maxdepth= 2, nu=0.1), trControl = trainControl(method="none"))
      #   preds <- predict(model, test)
      # }
      # if (methods[msel] == "ada_iter_50_depth_3") {
      #    ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "ada", tuneGrid=data.frame(iter = 50,maxdepth= 3, nu=0.1), trControl = trainControl(method="none"))
      #   preds <- predict(model, test)
      # }
      # 
      # if (methods[msel] == "ada_iter_100_depth_1") {
      #    ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "ada", tuneGrid=data.frame(iter = 100,maxdepth= 1, nu=0.1), trControl = trainControl(method="none"))
      #   preds <- predict(model, test)
      # }
      # if (methods[msel] == "ada_iter_100_depth_2") {
      #    ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "ada", tuneGrid=data.frame(iter = 100,maxdepth= 2, nu=0.1), trControl = trainControl(method="none"))
      #   preds <- predict(model, test)
      # }
      # if (methods[msel] == "ada_iter_100_depth_3") {
      #    ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "ada", tuneGrid=data.frame(iter = 100,maxdepth= 3, nu=0.1), trControl = trainControl(method="none"))
      #   preds <- predict(model, test)
      # }
      # 
      # if (methods[msel] == "ada_iter_150_depth_1") {
      #    ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "ada", tuneGrid=data.frame(iter = 150,maxdepth= 1, nu=0.1), trControl = trainControl(method="none"))
      #   preds <- predict(model, test)
      # }
      # if (methods[msel] == "ada_iter_150_depth_2") {
      #    ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "ada", tuneGrid=data.frame(iter = 150,maxdepth= 2, nu=0.1), trControl = trainControl(method="none"))
      #   preds <- predict(model, test)
      # }
      # if (methods[msel] == "ada_iter_150_depth_3") { #71
      #    ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "ada", tuneGrid=data.frame(iter = 150,maxdepth= 3, nu=0.1), trControl = trainControl(method="none"))
      #   preds <- predict(model, test)
      # }
      
      # uses additive logistic regressors (DecisionStump) base learners, the 100% of weight mass to base training on, without cross-validation, one run for internal cross-validation, threshold 1.79 on likelihood improvement, shrinkage parameter 1, and 10 iterations.
      # if (methods[msel] == "logitBoost_i11") { 
      #    ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "LogitBoost", tuneGrid=data.frame(nIter = 11), trControl = trainControl(method="none"))
      #   preds <- predict(model, test)
      # }
      # 
      # if (methods[msel] == "logitBoost_i21") { 
      #    ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "LogitBoost", tuneGrid=data.frame(nIter = 21), trControl = trainControl(method="none"))
      #   preds <- predict(model, test)
      # }
      # 
      # if (methods[msel] == "logitBoost_i31") { 
      #    ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "LogitBoost", tuneGrid=data.frame(nIter = 31), trControl = trainControl(method="none"))
      #   preds <- predict(model, test)
      # }
      
      if (methods[msel] == "gbm_1_50") { 
           ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "gbm", tuneGrid=data.frame(n.trees = 50, interaction.depth = 1, shrinkage = 0.1 , n.minobsinnode = 10), trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        }
      }
      
      if (methods[msel] == "gbm_1_100") { 
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "gbm", tuneGrid=data.frame(n.trees = 100, interaction.depth = 1, shrinkage = 0.1 , n.minobsinnode = 10), trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        }
      }
      if (methods[msel] == "gbm_1_150") { 
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "gbm", tuneGrid=data.frame(n.trees = 150, interaction.depth = 1, shrinkage = 0.1 , n.minobsinnode = 10), trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        }
      }
      
      if (methods[msel] == "gbm_2_50") { 
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "gbm", tuneGrid=data.frame(n.trees = 50, interaction.depth = 2, shrinkage = 0.1 , n.minobsinnode = 10), trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        }
      }
      
      if (methods[msel] == "gbm_2_100") { 
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "gbm", tuneGrid=data.frame(n.trees = 100, interaction.depth = 2, shrinkage = 0.1 , n.minobsinnode = 10), trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        }
        }
      if (methods[msel] == "gbm_2_150") { 
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "gbm", tuneGrid=data.frame(n.trees = 150, interaction.depth = 2, shrinkage = 0.1 , n.minobsinnode = 10), trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        }
      }
      
      
      if (methods[msel] == "gbm_3_50") { 
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "gbm", tuneGrid=data.frame(n.trees = 50, interaction.depth = 3, shrinkage = 0.1 , n.minobsinnode = 10), trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
      }
      }
      
      if (methods[msel] == "gbm_3_100") { 
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "gbm", tuneGrid=data.frame(n.trees = 100, interaction.depth = 3, shrinkage = 0.1 , n.minobsinnode = 10), trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        }
      }
      if (methods[msel] == "gbm_3_150") { 
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "gbm", tuneGrid=data.frame(n.trees = 150, interaction.depth = 3, shrinkage = 0.1 , n.minobsinnode = 10), trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
      }
      }
      
      # # Boosted Tree: Gradient boosting for optimizing arbitrary loss functions where regression trees are utilized as base-learners.  maxdepth = 1
      # if (methods[msel] == "blackboost_maxdepth1") { 
      #    ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "blackboost",tuneGrid=data.frame(mstop = 50, maxdepth= 1),  trControl = trainControl(method="none"))
      #   preds <- predict(model, test)
      # }
      # 
      # # Boosted Tree maxdepth = 2
      # if (methods[msel] == "blackboost_maxdepth2") { 
      #    ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "blackboost",tuneGrid=data.frame(mstop = 50, maxdepth= 2),  trControl = trainControl(method="none"))
      #   preds <- predict(model, test)
      # }
      # # Boosted Tree maxdepth = 3
      # if (methods[msel] == "blackboost_maxdepth3") { 
      #    ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "blackboost",tuneGrid=data.frame(mstop = 50, maxdepth= 3),  trControl = trainControl(method="none"))
      #   preds <- predict(model, test)
      # }
   
      
      
      ##################
      ###   Bagging  ###
      ##################
      
      if (methods[msel] == "treeBag") { 
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "treebag", trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        }
      }
      
      # Bagged Flexible Discriminant Analysis nprune =2
      if (methods[msel] == "bagFDA_prune2") { 
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "bagFDA",tuneGrid=data.frame(degree = 2, nprune= 2),  trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        }
      }
      # nprune = 4
      if (methods[msel] == "bagFDA_prune4") { 
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "bagFDA",tuneGrid=data.frame(degree = 2, nprune= 4),  trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        }
      }
      
      # nprune = 8
      if (methods[msel] == "bagFDA_prune8") { 
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "bagFDA",tuneGrid=data.frame(degree = 2, nprune= 8),  trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
      }
      }
      # nprune = 16  #82
      if (methods[msel] == "bagFDA_prune16") { 
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "bagFDA",tuneGrid=data.frame(degree = 2, nprune= 16),  trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
      }
      }
      
      ##################
      ###  Stacking  ###
      ##################
      
      ########################
      ###  Random Forests  ###
      ########################
      
      # creates a random forest with mtry = 2
      if (methods[msel] == "rf_mtry2") { 
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "rf",tuneGrid=data.frame(mtry = 2),  trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        }
      }
      # creates a random forest with mtry = 4
      if (methods[msel] == "rf_mtry4") { 
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "rf",tuneGrid=data.frame(mtry = 4),  trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        }
      }
      # creates a random forest with mtry = 8
      if (methods[msel] == "rf_mtry8") { 
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "rf",tuneGrid=data.frame(mtry = 8),  trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        }
      }
      # creates a random forest with mtry = 16
      if (methods[msel] == "rf_mtry16") { 
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "rf",tuneGrid=data.frame(mtry = 16),  trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        }
      }
      # creates a random forest with mtry = 32
      if (methods[msel] == "rf_mtry32") { 
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "rf",tuneGrid=data.frame(mtry = 32),  trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        }
      }
      # creates a random forest with mtry = 64
      if (methods[msel] == "rf_mtry64") { 
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "rf",tuneGrid=data.frame(mtry = 64),  trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        }
      }
      # creates a random forest with mtry = 128  
      if (methods[msel] == "rf_mtry128") { 
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "rf",tuneGrid=data.frame(mtry = 128),  trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        }
      }
      
      # creates a Regularised random forest with mtry = 2
      if (methods[msel] == "rrf_mtry2") { 
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "RRF",tuneGrid=data.frame(mtry = 2, coefReg=1, coefImp = 1),  trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        }
      }
      
      # creates a Regularised random forest with mtry = 4
      if (methods[msel] == "rrf_mtry4") { 
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "RRF",tuneGrid=data.frame(mtry = 4, coefReg=1, coefImp = 1),  trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
      }
      }
      # creates a Regularised random forest with mtry = 8
      if (methods[msel] == "rrf_mtry8") { 
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "RRF",tuneGrid=data.frame(mtry = 8, coefReg=1, coefImp = 1),  trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        }
      }
      
      # creates a Regularised random forest with mtry = 16
      if (methods[msel] == "rrf_mtry16") { 
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "RRF",tuneGrid=data.frame(mtry = 16, coefReg=1, coefImp = 1),  trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        }
      }
      
      # creates a Regularised random forest with mtry = 32
      if (methods[msel] == "rrf_mtry32") { 
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "RRF",tuneGrid=data.frame(mtry = 32, coefReg=1, coefImp = 1),  trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        }
      }
      
      # creates a Regularised random forest with mtry = 64
      if (methods[msel] == "rrf_mtry64") { 
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "RRF",tuneGrid=data.frame(mtry = 64, coefReg=1, coefImp = 1),  trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        }
      }
      
      # creates a Regularised random forest with mtry = 128
      if (methods[msel] == "rrf_mtry128") { 
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "RRF",tuneGrid=data.frame(mtry = 128, coefReg=1, coefImp = 1),  trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        }
      }
      
      
      # is a random forest and bagging ensemble of conditional inference trees (ctrees) aggregated by averaging observation weights extracted from each ctree. with mtry = 2
      if (methods[msel] == "cforest_mtry2") { 
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "cforest",tuneGrid=data.frame(mtry = 2),  trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        }
      }
      
      # mtry = 4
      if (methods[msel] == "cforest_mtry4") { 
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "cforest",tuneGrid=data.frame(mtry = 4),  trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        }
      }
      
      # mtry = 8
      if (methods[msel] == "cforest_mtry8") { 
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "cforest",tuneGrid=data.frame(mtry = 8),  trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        }
      }
      
      # mtry = 16
      if (methods[msel] == "cforest_mtry16") { 
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "cforest",tuneGrid=data.frame(mtry = 16),  trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
      }
      }
      # mtry = 32
      if (methods[msel] == "cforest_mtry32") { 
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "cforest",tuneGrid=data.frame(mtry = 32),  trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        }
      }
      
      # mtry = 64
      if (methods[msel] == "cforest_mtry64") { 
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "cforest",tuneGrid=data.frame(mtry = 64),  trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        }
      }
      
      # mtry = 128
      if (methods[msel] == "cforest_mtry128") { 
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "cforest",tuneGrid=data.frame(mtry = 128),  trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
      }
      }
      
      # is a random forest and bagging ensemble of conditional inference trees (ctrees) aggregated by averaging observation weights extracted from each ctree. with mtry = 2
      if (methods[msel] == "parRF_mtry2") { 
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "parRF",tuneGrid=data.frame(mtry = 2),  trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        }
      }
      
      # mtry = 4
      if (methods[msel] == "parRF_mtry4") { 
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "parRF",tuneGrid=data.frame(mtry = 4),  trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        }
      }
      
      # mtry = 8
      if (methods[msel] == "parRF_mtry8") { 
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "parRF",tuneGrid=data.frame(mtry = 8),  trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        }
      }
      
      # mtry = 16
      if (methods[msel] == "parRF_mtry16") { 
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "parRF",tuneGrid=data.frame(mtry = 16),  trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
      }
      }
      # mtry = 32
      if (methods[msel] == "parRF_mtry32") { 
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "parRF",tuneGrid=data.frame(mtry = 32),  trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        }
      }
      
      # mtry = 64
      if (methods[msel] == "parRF_mtry64") { 
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "parRF",tuneGrid=data.frame(mtry = 64),  trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
      }
      }
      # mtry = 128  #110
      if (methods[msel] == "parRF_mtry128") { 
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "parRF",tuneGrid=data.frame(mtry = 128),  trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
      }
      }
      
      ################################
      ### Nearest neighbor methods ###
      ################################
      
      ee <- function(){ return(TRUE)}
      
      if (methods[msel] == "knn_k1") { 
        ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "knn",tuneGrid=data.frame(k = 1),  trControl = trainControl(method="none")), error = function(e) {return(TRUE)})

        if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, test), error = function(e) {return(rep(FALSE, nrow(test)))})
        }
      }
      
      if (methods[msel] == "knn_k2") { 
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "knn",tuneGrid=data.frame(k = 2),  trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        }
      }
      
      if (methods[msel] == "knn_k3") { 
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "knn",tuneGrid=data.frame(k = 3),  trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        }
      }
      
      
      if (methods[msel] == "knn_k5") { 
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "knn",tuneGrid=data.frame(k = 5),  trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        }
      }
      
      
      if (methods[msel] == "knn_k7") { 
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "knn",tuneGrid=data.frame(k = 7),  trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        }
      }
      
      
      if (methods[msel] == "knn_k9") { 
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "knn",tuneGrid=data.frame(k = 9),  trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        }
      }
      
      if (methods[msel] == "Ibk_k1") { 
         ERROR <-  tryCatch(model <- IBk(Class ~., data = train,control = Weka_control(K=1)), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        }
      }
      
      if (methods[msel] == "Ibk_k2") { 
         ERROR <-  tryCatch(model <- IBk(Class ~., data = train,control = Weka_control(K=2)), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        }
        }
      
      
      if (methods[msel] == "Ibk_k3") { 
         ERROR <-  tryCatch(model <- IBk(Class ~., data = train,control = Weka_control(K=3)), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        }
      }
      
      if (methods[msel] == "Ibk_k5") { 
         ERROR <-  tryCatch(model <- IBk(Class ~., data = train,control = Weka_control(K=5)), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        }
      }
      
      if (methods[msel] == "Ibk_k7") { 
         ERROR <-  tryCatch(model <- IBk(Class ~., data = train,control = Weka_control(K=7)), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        }
      }
      
      if (methods[msel] == "Ibk_k9") { #122
         ERROR <-  tryCatch(model <- IBk(Class ~., data = train,control = Weka_control(K=9)), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        }
      }
      
      
      ################################################################
      ### Partial least squares and principal component regression ###
      ################################################################
      
      # uses the function mvr in the pls package to fit a PLSR (Martens, 1989) model tuning the number of components from 1 to 10.
      if (methods[msel] == "pls_ncomp1") { 
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "pls",tuneGrid=data.frame(ncomp = 1),  trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        }
      }
      
      if (methods[msel] == "pls_ncomp2") { 
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "pls",tuneGrid=data.frame(ncomp = 2),  trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        }
      }
      
      # if (methods[msel] == "pls_ncomp3") { 
      #    ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "pls",tuneGrid=data.frame(ncomp = 3),  trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
      #   
      #    if (is.logical(ERROR)){
      #     preds <- rep(FALSE, nrow(test))
      #   }else{
      #     preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
      #   }
      # }
      # 
      # if (methods[msel] == "pls_ncomp5") { 
      #    ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "pls",tuneGrid=data.frame(ncomp = 5),  trControl = trainControl(method="none"))
      #   preds <- predict(model, test)
      # }
      # 
      # if (methods[msel] == "pls_ncomp10") { 
      #    ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "pls",tuneGrid=data.frame(ncomp = 10),  trControl = trainControl(method="none"))
      #   preds <- predict(model, test)
      # }
      # 
      if (methods[msel] == "simpls_ncomp1") { 
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "simpls",tuneGrid=data.frame(ncomp = 1),  trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        
        } 
      }
      
      if (methods[msel] == "simpls_ncomp2") { 
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "simpls",tuneGrid=data.frame(ncomp = 2),  trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        
        } 
      }
      
      # if (methods[msel] == "simpls_ncomp3") { 
      #    ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "simpls",tuneGrid=data.frame(ncomp = 3),  trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
      #     testF <<- test
      #     trainF <<- train
      #     modelF <<- modelF
      #     print(ERROR)
      #    if (is.logical(ERROR)){
      #     preds <- rep(FALSE, nrow(test))
      #   }else{
      #     preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
      #     print(preds)
      #   }
      # }

      # if (methods[msel] == "simpls_ncomp5") { 
      #    ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "simpls",tuneGrid=data.frame(ncomp = 5),  trControl = trainControl(method="none"))
      #   preds <- predict(model, test)
      # }
      #   
      # if (methods[msel] == "simpls_ncomp10") { #132
      #    ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "simpls",tuneGrid=data.frame(ncomp = 10),  trControl = trainControl(method="none"))
      #   preds <- predict(model, test)
      # }
         
      ###########################################
      ### Logistic and multinomial regression ###
      ###########################################
      
      if (methods[msel] == "gcvEarth_d1") { 
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "gcvEarth",tuneGrid=data.frame(degree = 1),  trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        }
      }
      
      if (methods[msel] == "gcvEarth_d2") { 
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "gcvEarth",tuneGrid=data.frame(degree = 2),  trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        }
      }
      
      if (methods[msel] == "gcvEarth_d3") { #135
         ERROR <-  tryCatch(model <- train(Class ~ ., data = train, method = "gcvEarth",tuneGrid=data.frame(degree = 3),  trControl = trainControl(method="none")), error = function(e) {return(TRUE)})
        
         if (is.logical(ERROR)){
          preds <- rep(FALSE, nrow(test))
        }else{
          preds<- tryCatch(predict(model, newdata = test), error = function(e) {return(rep(FALSE, nrow(test)))})
        }
      }
      
      ####################
      ###  Base Lines  ###   
      ####################
      
      #Optimal Classifier
      
      #Optimal class
      if (methods[msel] == "OptimalClass") { 
        preds <- factor(test$Class, levels=unique(test$Class))
      }
      #DreadFul Class
      if (methods[msel] == "PessimalClass") { 
        preds <- rep(FALSE, nrow(test))
      }
      
      #Majority class
      if (methods[msel] == "MajorityClass") { 
        preds <- factor(rep(majority(datos$Class),nrow(test)), levels=unique(test$Class))
      }
      
      #Minority class
      if (methods[msel] == "MinorityClass") { 
        minC <- which.min(as.vector(table(datos$Class)))
        minC_class <- dimnames(table(datos$Class))[[1]][minC]       
        preds <- factor(rep(minC_class,nrow(test)), levels=unique(test$Class))
      }
      
      #random Class
      
      if (methods[msel] == "RandomClass_A") { 
        #preds <- sample(unique(datos$Class),nrow(test), replace=T)
        preds <- sample(test$Class) 
        
      }
      
      if (methods[msel] == "RandomClass_B") { 
        #preds <- sample(unique(datos$Class),nrow(test), replace=T)
        preds <- sample(test$Class) 
        
      }
      
      if (methods[msel] == "RandomClass_C") { 
        #preds <- sample(unique(datos$Class),nrow(test), replace=T)
        preds <- sample(test$Class) 
        
      }
      
      ## Control Experiment
      
      if (methods[msel] == "gt90%_A") { 
        FAILS <- 1
        N <- round(FAILS*(nrow(test)/10))
        preds <- as.character(test$Class)
        s <- sample(length(preds),N)
        j <- 1
        for(i in s){
          preds[i] <- "FAIL"
          j<-j+1
        }
        print("Salgo de gt90A")
      }
      
      if (methods[msel] == "gt90%_B") { 
        FAILS <- 1
        N <- round(FAILS*(nrow(test)/10))
        preds <- as.character(test$Class)
        s <- sample(length(preds),N)
        j <- 1
        for(i in s){
          preds[i] <- "FAIL"
          j<-j+1
        }
      }
      
      if (methods[msel] == "gt90%_C") { 
        FAILS <- 1
        N <- round(FAILS*(nrow(test)/10))
        preds <- as.character(test$Class)
        s <- sample(length(preds),N)
        j <- 1
        for(i in s){
          preds[i] <- "FAIL"
          j<-j+1
        }
      }
      
      ################
      
      if (methods[msel] == "gt80%_A") { 
        FAILS <- 2
        N <- round(FAILS*(nrow(test)/10))
        preds <- as.character(test$Class)
        s <- sample(length(preds),N)
        j <- 1
        for(i in s){
          preds[i] <- "FAIL"
          j<-j+1
        }
      }
      
      if (methods[msel] == "gt80%_B") { 
        FAILS <- 2
        N <- round(FAILS*(nrow(test)/10))
        preds <- as.character(test$Class)
        s <- sample(length(preds),N)
        j <- 1
        for(i in s){
          preds[i] <- "FAIL"
          j<-j+1
        }
      }
      
      if (methods[msel] == "gt80%_C") { 
        FAILS <- 2
        N <- round(FAILS*(nrow(test)/10))
        preds <- as.character(test$Class)
        s <- sample(length(preds),N)
        j <- 1
        for(i in s){
          preds[i] <- "FAIL"
          j<-j+1
        }
      }
      
      
      ############
      
      
      if (methods[msel] == "gt70%_A") { 
        FAILS <- 3
        N <- round(FAILS*(nrow(test)/10))
        preds <- as.character(test$Class)
        s <- sample(length(preds),N)
        j <- 1
        for(i in s){
          preds[i] <- "FAIL"
          j<-j+1
        }
      }
      
      if (methods[msel] == "gt70%_B") { 
        FAILS <- 3
        N <- round(FAILS*(nrow(test)/10))
        preds <- as.character(test$Class)
        s <- sample(length(preds),N)
        j <- 1
        for(i in s){
          preds[i] <- "FAIL"
          j<-j+1
        }
      }
      
      if (methods[msel] == "gt70%_C") { 
        FAILS <- 3
        N <- round(FAILS*(nrow(test)/10))
        preds <- as.character(test$Class)
        s <- sample(length(preds),N)
        j <- 1
        for(i in s){
          preds[i] <- "FAIL"
          j<-j+1
        }
      }
      
      
      ##############
      
      
      
      if (methods[msel] == "gt60%_A") { 
        FAILS <- 4
        N <- round(FAILS*(nrow(test)/10))
        preds <- as.character(test$Class)
        s <- sample(length(preds),N)
        j <- 1
        for(i in s){
          preds[i] <- "FAIL"
          j<-j+1
        }
      }
      
      if (methods[msel] == "gt60%_B") { 
        FAILS <- 4
        N <- round(FAILS*(nrow(test)/10))
        preds <- as.character(test$Class)
        s <- sample(length(preds),N)
        j <- 1
        for(i in s){
          preds[i] <- "FAIL"
          j<-j+1
        }
      }
      
      if (methods[msel] == "gt60%_C") { 
        FAILS <- 4
        N <- round(FAILS*(nrow(test)/10))
        preds <- as.character(test$Class)
        s <- sample(length(preds),N)
        j <- 1
        for(i in s){
          preds[i] <- "FAIL"
          j<-j+1
        }
      }
      
      ##################
      
      
      
      if (methods[msel] == "gt50%_A") { 
        FAILS <- 5
        N <- round(FAILS*(nrow(test)/10))
        preds <- as.character(test$Class)
        s <- sample(length(preds),N)
        j <- 1
        for(i in s){
          preds[i] <- "FAIL"
          j<-j+1
        }
      }
      
      if (methods[msel] == "gt50%_B") { 
        FAILS <- 5
        N <- round(FAILS*(nrow(test)/10))
        preds <- as.character(test$Class)
        s <- sample(length(preds),N)
        j <- 1
        for(i in s){
          preds[i] <- "FAIL"
          j<-j+1
        }
      }
      
      if (methods[msel] == "gt50%_C") { 
        FAILS <- 5
        N <- round(FAILS*(nrow(test)/10))
        preds <- as.character(test$Class)
        s <- sample(length(preds),N)
        j <- 1
        for(i in s){
          preds[i] <- "FAIL"
          j<-j+1
        }
      }
      
      ###################
      
      if (methods[msel] == "gt40%_A") { 
        FAILS <- 6
        N <- round(FAILS*(nrow(test)/10))
        preds <- as.character(test$Class)
        s <- sample(length(preds),N)
        j <- 1
        for(i in s){
          preds[i] <- "FAIL"
          j<-j+1
        }
      }
      
      if (methods[msel] == "gt40%_B") { 
        FAILS <- 6
        N <- round(FAILS*(nrow(test)/10))
        preds <- as.character(test$Class)
        s <- sample(length(preds),N)
        j <- 1
        for(i in s){
          preds[i] <- "FAIL"
          j<-j+1
        }
      }
      
      if (methods[msel] == "gt40%_C") { 
        FAILS <- 6
        N <- round(FAILS*(nrow(test)/10))
        preds <- as.character(test$Class)
        s <- sample(length(preds),N)
        j <- 1
        for(i in s){
          preds[i] <- "FAIL"
          j<-j+1
        }
      }
      
      ############
      
      if (methods[msel] == "gt30%_A") { 
        FAILS <- 7
        N <- round(FAILS*(nrow(test)/10))
        preds <- as.character(test$Class)
        s <- sample(length(preds),N)
        j <- 1
        for(i in s){
          preds[i] <- "FAIL"
          j<-j+1
        }
      }
      
      if (methods[msel] == "gt30%_B") { 
        FAILS <- 7
        N <- round(FAILS*(nrow(test)/10))
        preds <- as.character(test$Class)
        s <- sample(length(preds),N)
        j <- 1
        for(i in s){
          preds[i] <- "FAIL"
          j<-j+1
        }
      }
      
      if (methods[msel] == "gt30%_C") { 
        FAILS <- 7
        N <- round(FAILS*(nrow(test)/10))
        preds <- as.character(test$Class)
        s <- sample(length(preds),N)
        j <- 1
        for(i in s){
          preds[i] <- "FAIL"
          j<-j+1
        }
      }
      
      #############
      
      if (methods[msel] == "gt20%_A") { 
        FAILS <- 8
        N <- round(FAILS*(nrow(test)/10))
        preds <- as.character(test$Class)
        s <- sample(length(preds),N)
        j <- 1
        for(i in s){
          preds[i] <- "FAIL"
          j<-j+1
        }
      }
      
      if (methods[msel] == "gt20%_B") { 
        FAILS <- 8
        N <- round(FAILS*(nrow(test)/10))
        preds <- as.character(test$Class)
        s <- sample(length(preds),N)
        j <- 1
        for(i in s){
          preds[i] <- "FAIL"
          j<-j+1
        }
      }
      
      if (methods[msel] == "gt20%_C") { 
        FAILS <- 8
        N <- round(FAILS*(nrow(test)/10))
        preds <- as.character(test$Class)
        s <- sample(length(preds),N)
        j <- 1
        for(i in s){
          preds[i] <- "FAIL"
          j<-j+1
        }
      }
      
      ###################
      
      
      if (methods[msel] == "gt10%_A") { 
        FAILS <- 9
        N <- round(FAILS*(nrow(test)/10))
        preds <- as.character(test$Class)
        s <- sample(length(preds),N)
        j <- 1
        for(i in s){
          preds[i] <- "FAIL"
          j<-j+1
        }
      }
      
      if (methods[msel] == "gt10%_B") { 
        FAILS <- 9
        N <- round(FAILS*(nrow(test)/10))
        preds <- as.character(test$Class)
        s <- sample(length(preds),N)
        j <- 1
        for(i in s){
          preds[i] <- "FAIL"
          j<-j+1
        }
      }
      
      if (methods[msel] == "gt10%_C") { 
        FAILS <- 9
        N <- round(FAILS*(nrow(test)/10))
        preds <- as.character(test$Class)
        s <- sample(length(preds),N)
        j <- 1
        for(i in s){
          preds[i] <- "FAIL"
          j<-j+1
        }
      }
      
      

      
      for(test_ex in 1:length(preds)){
        matrixDS[indiceStrat[test_ex],msel] = as.character(preds[test_ex])==as.character(test[test_ex,nomParamEstudio])
        
      }
      
    }
  }
  
  
  #write.csv(matrixDS, file=paste(ds,"_",datasets[selected],"_Result.csv",sep=""))
  ListDS_Results[[datasets[selected]]]<- matrixDS
  
}
save(ListDS_Results, file=paste(ds,"ListAllResults.RData",sep=""))
save(methods, file = paste(ds,"Methods.RData",sep=""))
save(datasets,ds, file = paste(ds,"datasets.RData",sep=""))

end.time <- Sys.time()
time.taken <- end.time - start.time
print(paste("Time taken: ", time.taken))



print("Accuracy: ")
for(i in 1:length(ListDS_Results)){
  print(colMeans(ListDS_Results[[i]],na.rm = TRUE))
  
}



