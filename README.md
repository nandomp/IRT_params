# Making sense of IRT in machine learning (Expermiments & Code) [UPDATING...]




## Important code files 

- **MethodsResponse.R** Classifiers binary responses (TRUE,FALSE) for the instances in a dataset
- **IRT_model_fit_and_plot.R** IRT models (LTM and MIRT packages) and different plots.
- **3PLmodelsComparison.R** Comparative between four 3PL models (LTM, LTM RND, MIRT, MIRT RND). It generates the same plots as the previous file.
- **runExperiments.R** Run the experimets. Change the directory where your .csv are placed. 


##  Set of classifiers

| Family                         | ID            | Technique                                                 | Packages                     | Tuning Parameters                                     | #models |
|--------------------------------|---------------|-----------------------------------------------------------|------------------------------|-------------------------------------------------------|---------|
| Decision Trees                 | C5.0          | C5.0                                                      | C50, plyr                    | winnow                                                | 2       |
|                                | J48           | J48                                                       | RWeka                        | Unprunned                                             | 2       |
|                                | LMT           | Logistic Model Trees                                      | RWeka                        | C, A                                                  | 4       |
|                                | rpart         | CART                                                      | rpart                        |                                                       | 1       |
|                                | ctree         | Conditional Inference Tree                                | party                        | mincriterion                                          | 3       |
| Rule-based methods             | jRip          | Rule-Based Classifier                                     | RWeka                        | E                                                     | 2       |
|                                | PART          | PART decision lists                                       | rpart                        |                                                       | 1       |
| Discriminant analysis          | sda           | Shrinkage Discriminant Analysis                           | sda                          | diagonal, lambda                                      | 3       |
|                                | fda           | Flexible Discriminant Analysis                            | earth, mda                   | degree, nprune                                        | 3       |
|                                | mda           | Mixture Discriminant Analysis                             | mda                          | subclasses                                            | 3       |
| Bayesian                       | NB            | Naive Bayes                                               | RWeka                        |                                                       | 1       |
|                                | naiveBayes    | Naive Bayes                                               | e1071                        | laplace                                               | 1       |
| Neural Networks                | rbf           | Radial Basis Function Network                             | RSNNS                        | negativeThreshold                                     | 1       |
|                                | mlp           | Multi-Layer Perceptron                                    | RSNNS                        | size                                                  | 5       |
|                                | avNNet        | Model Averaged Neural Network                             | nnet                         | size, decay, bag                                      | 3       |
|                                | pcaNNet       | Neural Networks with Feature Extraction                   | nnet                         | size, decay                                           | 1       |
|                                | lvq           | Learning Vector Quantization                              | class                        | size, k                                               | 3       |
| Support Vector Machines        | SMO           | Sequential Minimal Optimization                           | RWeka                        |                                                       | 1       |
|                                | svmRadialCost | Support Vector Machines with Radial Basis Function Kernel | kernlab                      | C                                                     | 4       |
|                                | svmLinear     | Support Vector Machines with Linear Kernel                | kernlab                      | C                                                     | 6       |
|                                | svmPoly       | Support Vector Machines with Polynomial Kernel            | kernlab                      | degree, scale, C                                      | 9       |
|                                | gbm           | Stochastic Gradient Boosting                              | gbm, plyr                    | n.trees, interaction.depth, shrinkage, n.minobsinnode | 9       |
| Bagging                        | treebag       | Bagged CART                                               | ipred, plyr, e1071           |                                                       | 1       |
|                                | bagFDA        | Bagged Flexible Discriminant Analysis                     | earth, mda                   | degree, nprune                                        | 4       |
| Random Forests                 | rf            | Random Forest                                             | randomForest                 | mtry                                                  | 7       |
|                                | RRF           | Regularized Random Forest                                 | randomForest, RRF            | mtry                                                  | 7       |
|                                | cforest       | Conditional Inference Random Forest                       | party                        | mtry                                                  | 7       |
|                                | parRF         | Parallel Random Forest                                    | e1071, randomForest, foreach | mtry                                                  | 7       |
| Nearest neighbor methods       | knn           | k-Nearest Neighbors                                       |                              | k                                                     | 6       |
|                                | IBk           | k-Nearest Neighbors                                       | RWeka                        | K                                                     | 6       |
| Partial least squares          | pls           | Partial Least Squares                                     | pls                          | ncomp                                                 | 2       |
|                                | simpls        | Partial Least Squares                                     | pls                          | ncomp                                                 | 2       |
| Principal component regression | gcvEarth      | Multivariate Adaptive Regression Splines                  | earth                        | degree                                                | 3       |
| Base Lines                     | OptimalClass  | Optimal Classifier                                        |                              |                                                       | 1       |
|                                | PessimalClass | Pessimal Classifier                                       |                              |                                                       | 1       |
|                                | MajorityClass | Majority Classifier                                       |                              |                                                       | 1       |
|                                | MinorityClass | Minority Classifier                                       |                              |                                                       | 1       |
|                                | RandomClass   | Random Classifier                                         |                              |                                                       | 3       |


##  Set of Datasets (UCI repository)

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



##  RData (outputs)

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
