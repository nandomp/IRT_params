# &#x1F535; Making sense of IRT in machine learning &#x1F535;
## (Expermiments & Code) [UPDATING...]



## Important code files:

- **MethodsResponse.R** Classifiers binary responses (TRUE,FALSE) for the instances in a dataset
- **IRT_model_fit_and_plot.R** IRT models (LTM and MIRT packages) and different plots.
- **3PLmodelsComparison.R** Comparative between four 3PL models (LTM, LTM RND, MIRT, MIRT RND). It generates the same plots as the previous file.
- **runExperiments.R** Run the experimets. Change the directory where your .csv are placed. 


## Set of classifiers

```R
  methods<-c("c5.0", "c5.0_winnow", "J48", "J48Unp", "LMT", "LMT_CV", "LMT_AIC", "rpart", "ctree_c0.01", 
  "ctree_c0.05", "ctree_c0.99", "JRip", "JRip_Unp","PART", "sda_L0.0", "sda_L0.5", "sda_L1.0", "fda_prune2", 
  "fda_prune9", "fda_prune17", "mda_subc2", "mda_subc3", "mda_subc4","W_NB", "NB", "NB_laplace", "rbf", 
  "mlp_1", "mlp_3", "mlp_5", "mlp_7", "mlp_9", "avNNet_decay1e04", "avNNet_decay01", "avNNet_decay0", "pcaNNet",
  "lvq_1", "lvq_3", "lvq_5", "SMV", "svmRadialCost_C0.01", "svmRadialCost_C0.1", "svmRadialCost_C1", 
  "svmRadialCost_C2", "svmLinear_C0.01", "svmLineart_C0.1", "svmLinear_C1", "svmLinear_C2", "svmLinear_C4", 
  "svmLinear_C8", "svmPoly_d_1_s_0.001", "svmPoly_d_1_s_0.01", "svmPoly_d_1_s_0.1", "svmPoly_d_2_s_0.001", 
  "svmPoly_d_2_s_0.01", "svmPoly_d_2_s_0.1", "svmPoly_d_3_s_0.001", "svmPoly_d_3_s_0.01", "svmPoly_d_3_s_0.1", 
  "gbm_1_50", "gbm_1_100", "gbm_1_150", "gbm_2_50", "gbm_2_100", "gbm_2_150", "gbm_3_50", "gbm_3_100", "gbm_3_150", 
  "treeBag", "bagFDA_prune2", "bagFDA_prune4", "bagFDA_prune8", "bagFDA_prune16", "rf_mtry2", "rf_mtry4", "rf_mtry8", 
  "rf_mtry16", "rf_mtry32", "rf_mtry64", "rf_mtry128", "rrf_mtry2", "rrf_mtry4", "rrf_mtry8", "rrf_mtry16",
  "rrf_mtry32", "rrf_mtry64", "rrf_mtry128", "cforest_mtry2", "cforest_mtry4", "cforest_mtry8", "cforest_mtry16",
  "cforest_mtry32", "cforest_mtry64", "cforest_mtry128", "parRF_mtry2","parRF_mtry4", "parRF_mtry8", "parRF_mtry16",
  "parRF_mtry32", "parRF_mtry64", "parRF_mtry128", "knn_k1", "knn_k2", "knn_k3", "knn_k5", "knn_k7", "knn_k9", 
  "Ibk_k1", "Ibk_k2", "Ibk_k3", "Ibk_k5", "Ibk_k7", "Ibk_k9", "pls_ncomp1", "pls_ncomp2", "simpls_ncomp1", 
  "simpls_ncomp2", "gcvEarth_d1", "gcvEarth_d2", "gcvEarth_d3", "RandomClass_A", "RandomClass_B", "RandomClass_C",
  "MajorityClass", "MinorityClass", "OptimalClass","PessimalClass")
}
```

## Set of Datasets

| Dataset		| Instances		| NumAtt	| ClassProportion		|
| ------------- |:-------------:| ---------:| ---------------------:|
|Chocardiogram 	|131 			| 11		| 2 Labels (67.2%, 32.8%) |
|Statlog-heart  	|270 			| 14		| 2 Labels (44.4%, 55.6%) |
|Hepatitis  	|155			| 20		| 2 Labels (79.4%,20.6%) |
|Ionosphere  	|323  			| 34 		| 2 Labels (65.3%, 34.7%) |
|Parkinsons  	|195 			| 23 		| 2 Labels (75.4%, 24.6%) |
|Seeds  	|210 			| 8 		| 3 Labels (33.3%, 33.3%, 33.3%) |
|Teaching  	|151 			| 6 		| 3 Labels (35.5%, 35.5%, 29%) |
|Vertebral-column  	|310 			| 7 		| 3 Labels (16.6%, 53.1%, 30.3%) |



## RData (outputs):

List of R variables collected using the multiclass datasets
 
R VARIABLES

************************************************

**all_param_mc (in file "irt_parameters_mc.RData")**

- List of the IRT parameters for each dataset. The element all_param[[i]] is realted to a dataset and 
stores a matrix of size "ninstances x 3", with the guessing, difficulty and discrimination parameters for each instance.  

Example: The parameters of the 100th instance of the 2nd dataset

```R
> all_param_mc[[2]][100,] 
 Gussng        Dffclt        Dscrmn 
 4.007872e-06  2.127100e+00 -1.177862e+00 
```


**all_abilities_mc (in file "algor_abilities_mc.RData")**

- Matrix of size "ndatasets x nalgorithms" with the abilities estimated by the IRT model. We have in this 
matrix ndatasets = 23 and nalgorithms = 15.

Example: algorithms' abilities in the 3rd dataset 

```R
> all_abilities_mc[3,]
 [1]  0.2174318  0.4485440  2.0641048  5.7090635  2.6863440  2.3428594
 [7] -1.1089645  2.2692792  0.7727770  1.2470157  2.6863440 -0.2820624
 [13]  0.8074555  1.3203026 -1.5295262
```

**all_accuracies_mc (in file "algor_accuracies_mc.RData")**

- Stores the accuracies of the 15 algorithms in the 23 datasets

Example: algorithms' accuracies in the 2nd dataset: 

```R
> all_accuracies_mc[2,]
 J48     J48Unp        LMT     logist        SMO         NB      Stump 
 0.7317073  0.7512195  0.7560976  0.6731707  0.6439024  0.5560976  0.3951220 
 IBK       JRip       PART        SMV        Ada    Bagging LogitBoost 
 0.5414634  0.6829268  0.6780488  0.6439024  0.3951220  0.6243902  0.7365854 
 Stacking 
 0.3170732 
```


**results_mc (in file "results_responses_mc.RData")**

- List of binary responses of the algorithms for each dataset. The element results[[i]] is related to a dataset and stores a matrix of size "ninstances x nalgorithms" with 1's (right responses) and 0's (wrong responses). It 
is useful to generate other measures like accuracy of the algorithms and average 0\1 per instance.

Example: responses in the 1st instance of the 3rd dataset

```R
> results[[3]][2,]
J48     J48Unp        LMT     logist        SMO         NB      Stump 
0          0          1          1          0          0          0 
IBK       JRip       PART        SMV        Ada    Bagging LogitBoost 
0          0          0          0          0          0          0 
Stacking 
0 
```
