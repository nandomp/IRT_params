# Making sense of item response theory in machine learning (Expermiments & Code) [UPDATING...]

## Important code files:

- **MethodsResponse.R** Classifiers binary responses (TRUE,FALSE) for the instances in a dataset
- **IRT_model_fit_and_plot.R** IRT models (LTM and MIRT packages) and different plots.
- **3PLmodelsComparison.R** Comparative between four 3PL models (LTM, LTM RND, MIRT, MIRT RND). It generates the same plots as the previous file.
- **runExperiments.R** Run the experimets. Change the directory where your .csv are placed. 


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
