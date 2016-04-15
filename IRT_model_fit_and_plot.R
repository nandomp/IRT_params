# 
# SOURCE_CODE ="D:/OneDrive/Rworks/IRT/Noise"
# DATASET2PLAY = "D:/OneDrive/Rworks/IRT/Noise/_Toy_"
# 
# setwd(DATASET2PLAY)

###############################################
############# OPERATING OPTIONS ###############
###############################################

set.seed(998)
ind_instance = 1
ind_dataset=1
args <- commandArgs(trailingOnly = TRUE)
options(scipen = 99999)

OptimimalParam = TRUE
binaryClass = FALSE
weArePlaying = TRUE

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



###############################################
#############     LIBRARIES     ###############
###############################################

.lib<- c("ltm","devtools", "ggplot2","stats","gridExtra", "ggrepel", "wordcloud", "Hmisc")#"mirt"
.inst <- .lib %in% installed.packages()
if (length(.lib[!.inst])>0) install.packages(.lib[!.inst], repos=c("http://rstudio.org/_packages", "http://cran.rstudio.com")) 
lapply(.lib, require, character.only=TRUE)

# Especific library for ploting PCAs
#install_github("ggbiplot", "vqv")
library(ggbiplot)


###############################################
#############    PDF/EPS GEN    ###############
###############################################


# Parameters for PDF generation
PDFEPS <- 1 # 0 None, 1 PDF, 2 EPS
PDFheight= 7 # 7 by default, so 14 makes it double higher than wide, 5 makes letters bigger (in proportion) for just one 
PDFwidth= 7 # 7 by default

# This function is used to generate PDFs or EPSs for the plots
openPDFEPS <- function(file, height= PDFheight, width= PDFwidth) {
  if (PDFEPS == 1) {
    pdf(paste(file, ".pdf", sep=""), width, height)
  } else if (PDFEPS == 2) {
    postscript(paste(file, ".eps", sep=""), width, height, horizontal=FALSE)
  }
}


###############################################
############  GLOBAL VARIABLES  ###############
###############################################


load(paste(ds,"ListAllResults.RData",sep="")) # ListDS_Results
load(paste(ds,"Methods.RData",sep="")) #methods
load(paste(ds,"Datasets.RData",sep="")) #datasets, ds (directory)

item_param <- list()

results <- list()

numMethods <- length(methods)
numDS <- length(ListDS_Results)

all_abilities <-  matrix(rep(NA, numMethods * numDS), nrow=numDS, ncol=numMethods, byrow = T)
colnames(all_abilities) <- methods

acc <- matrix(rep(NA, numMethods * numDS), nrow=numDS, ncol=numMethods, byrow = T)
colnames(acc) <- methods

all_models <- list()
avgProbs <- matrix(rep(NA, numMethods * numDS), nrow=numDS, ncol=numMethods, byrow = T)


abil_plots <- list()
success_plots <- list()
acc_plots <- list()
accAbil_plots <- list()
accProb_plots <- list()

###############################################
###############    FUNCTIONS   ################
###############################################

# MIRT package
fit_mIRT <-function(allresp, type, rnd = FALSE){
  
  if(type == 3){ 
    if(rnd){
      fit <- mirt(allresp,1,itemtype = '3PL', technical = list(NCYCLES = 500), GenRandomPars = TRUE)
    }else{
      fit <- mirt(allresp,1,itemtype = '3PL', technical = list(NCYCLES = 500))
      
    }
    
  }
  
  ## Extracting the items' parameters: 
  ## Gussng (ci), Dffclt (bi) and Dscrmn (ai) 
  
  temp = coef(fit, simplify = T, IRTpars =T)$items
  item_param <- temp[,c("g","b","a")]
  colnames(item_param)<-c("Gussng","Dffclt","Dscrmn")
  
  ## computing the abilities 'ab_vector' of the respondents   

  abil<-t(fscores(fit))
  
  return(list(model = fit, item_param = item_param, abil_vector = abil))
  

}


# LTM package
fit_IRT <- function(allresp,type,rnd=FALSE){
  ## builds the IRT models given the responses allresp and the model type
  ## requires the ltm package
  
  ## allresp: binary matrix matrix (with dimension nrow vs ncol) storing 
  ##          the ncol responses of nrow respondents    
  ## type in {1,2,3}: indicates the number of parameters of the IRT model 
  ##                  (i.e., 1P, 2P or 3P IRT model) 
  
  ## calling the tpm function implemented in the ltm package
  
  if(type == 3){ 
    if(rnd){
      fit <- tpm(allresp, type = "latent.trait",  IRT.param=TRUE, start.val = "random" )
    }else{
      fit <- tpm(allresp, type = "latent.trait",  IRT.param=TRUE)
    }
   
  }
  
  nitems = ncol(allresp)
  if(type == 2){
    
    ## Parameter Gussng (ci) constrained to zero
    fit <- tpm(allresp, type = "latent.trait",  IRT.param=TRUE, constraint = cbind(1:nitems, 1, 0))
  }
  
  if(type == 1){
    
    ## Parameter Gussng (ci) constrained to zero
    ## Parameter Dscrmn (ai) constrained to one
    fit <- tpm(allresp, type = "latent.trait",  IRT.param=TRUE, constraint =  rbind(cbind(1:nitems, 1, 0),cbind(1:nitems, 3, 1)))
  }
  
  ## Extracting the items' parameters: 
  ## Gussng (ci), Dffclt (bi) and Dscrmn (ai) 
  
  item_param = coef(fit)
  
  ## computing the abilities 'ab_vector' of the respondents   
  r = factor.scores(fit,resp.patterns=allresp)
  abil_vector = r$score.dat$z1
  
  return(list(model = fit, item_param = item_param, abil_vector = abil_vector))
  
}



## Requires the ltm package
plot_ICC <- function(all_models,results,all_abilities,ind_dataset,ind_instance, main = "Item Characteristic Curve", randomCuts = TRUE){
  ## plots the ICC of the "ind_instance-th" instance of the "ind_dataset-th" dataset


  
  
  fit = all_models[[ind_dataset]]$model
  abil = all_abilities[ind_dataset,]
  resp = results[[ind_dataset]][ind_instance,]#resp = results[[ind_dataset]][ind_instance,]
  
  plot(fit,items=ind_instance,xlim=cbind(-4,4),ylim=cbind(0,1),annot=FALSE,main = main)
  par(new=TRUE) 
  plot(abil,resp,xlim=cbind(-4,4),ylim=cbind(0,1),xlab="",ylab="")
 
  xabil <- c(abil[(length(abil)-6):length(abil)])
  yresp <- c(resp[(length(resp)-6):length(resp)])

  points(xabil[1:(length(xabil)-2)], yresp[1:(length(yresp)-2)], xlim=cbind(-4,4),ylim=cbind(0,1),xlab="",ylab="",cex = .5, col = "green")
  points(xabil[(length(xabil)-1):(length(xabil))], yresp[(length(yresp)-1):(length(yresp))], xlim=cbind(-4,4),ylim=cbind(0,1),xlab="",ylab="",cex = .5, col = "red")
  
  text(xabil,yresp, labels=c("RndA","RndB", "RndC", "Maj","Min","Opt", "Pessim"), cex= 0.8, pos=4, font = 4, srt=90)
  
  
  
  # Random Classifiers CUT POINTS (ad-hoc)
  if (randomCuts){
    rnd <- c("RandomClass_A", "RandomClass_B", "RandomClass_C")
    
    # UPDATE : 3 Random Classifiers 
    for(i in 4:6){
      
      RandomModelDiff = abil[(length(abil)-i)]
      abline(v=RandomModelDiff, col="red", lty=2)
      
      a = all_models[[ind_dataset]]$item_param[ind_instance,3]
      b = all_models[[ind_dataset]]$item_param[ind_instance,2]
      c = all_models[[ind_dataset]]$item_param[ind_instance,1]
      theta = RandomModelDiff
      
      y = c + (1-c)/(1+exp(-a*(theta-b)))
      abline(h=y, col="red", lty=2)
      
      #print probability of success
      text(x=-3.8, y=y,paste(round(y, digits=4)),cex= 0.8, pos=3)
      
      # Success or fail? (I depend just on 1 random classifier)
      if(results[[1]][ind_instance,rnd[i-3]] == 1){ #results[[ind_dataset]]
        text(x=RandomModelDiff, y=1,paste(round(RandomModelDiff, digits=4)),cex= 0.8, pos=2)
      }else{
        text(x=RandomModelDiff, y=0,paste(round(RandomModelDiff, digits=4)),cex= 0.8, pos=2)
      }
    }
    
  }
  
  
  
} 

plot_ICC_ECAI <- function(all_models,results,all_abilities,ind_dataset,ind_instance, main = "Item Characteristic Curve", randomCuts = FALSE){
  ## plots the ICC of the "ind_instance-th" instance of the "ind_dataset-th" dataset
  
  
  
  
  fit = all_models[[ind_dataset]]$model
  abil = all_abilities[ind_dataset,]
  resp = results[[ind_dataset]][ind_instance,]#resp = results[[ind_dataset]][ind_instance,]
  
  plot(fit,items=ind_instance,xlim=cbind(-4,4),ylim=cbind(0,1),annot=FALSE,main = main,cex.lab=1.2)
  par(new=TRUE) 
  plot(abil,resp,xlim=cbind(-4,4),ylim=cbind(0,1),xlab="",ylab="",cex = 1.5)
  
  xabil <- c(abil[(length(abil)-6):length(abil)])
  yresp <- c(resp[(length(resp)-6):length(resp)])
  par(new=TRUE) 
  
  #points(xabil[1:(length(xabil)-2)], yresp[1:(length(yresp)-2)], xlim=cbind(-4,4),ylim=cbind(0,1),xlab="",ylab="",cex = .5, col = "green")
  #points(xabil[(length(xabil)-1):(length(xabil))], yresp[(length(yresp)-1):(length(yresp))], xlim=cbind(-4,4),ylim=cbind(0,1),xlab="",ylab="",cex = .5, col = "green")
  par(new=TRUE) 
  
  textplot(xabil,yresp, c("RndA","RndB", "RndC", "Maj","Min","Opt", "Pess"), xlim=c(-4,4),ylim=c(0,1),xlab="",ylab="",show.lines = TRUE, cex= 1.5)
  #textplot(xabil,yresp, c("RandomClass_A","RandomClass_B", "RandomClass_C", "MajorityClass","MinorityClass","OptimalClass", "PessimalClass"), xlim=c(-4,4),ylim=c(0,1),xlab="",ylab="",show.lines = TRUE, cex= 1.5)
  
  
  
  
  # Random Classifiers CUT POINTS (ad-hoc)
  if (randomCuts){
    rnd <- c("RandomClass_A", "RandomClass_B", "RandomClass_C")
    
    # UPDATE : 3 Random Classifiers 
    for(i in 4:6){
      
      RandomModelDiff = abil[(length(abil)-i)]
      abline(v=RandomModelDiff, col="red", lty=2)
      
      a = all_models[[ind_dataset]]$item_param[ind_instance,3]
      b = all_models[[ind_dataset]]$item_param[ind_instance,2]
      c = all_models[[ind_dataset]]$item_param[ind_instance,1]
      theta = RandomModelDiff
      
      y = c + (1-c)/(1+exp(-a*(theta-b)))
      abline(h=y, col="red", lty=2)
      
      #print probability of success
      #text(x=-3.8, y=y,paste(round(y, digits=4)),cex= 0.8, pos=3)
      
      # Success or fail? (I depend just on 1 random classifier)
      # if(results[[1]][ind_instance,rnd[i-3]] == 1){ #results[[ind_dataset]]
      #   text(x=RandomModelDiff, y=1,paste(round(RandomModelDiff, digits=4)),cex= 0.8, pos=2)
      # }else{
      #   text(x=RandomModelDiff, y=0,paste(round(RandomModelDiff, digits=4)),cex= 0.8, pos=2)
      # }
    }
    
  }
  
  
  

} 

#generic PLOT (used for the  models generated with the MIRT package)
plot_mICC <- function(all_models,results,all_abilities,ind_dataset,ind_instance, main ="Item Characteristic Curve", randomCuts = TRUE){
  ## plots the ICC of the "ind_instance-th" instance of the "ind_dataset-th" dataset
  
  #item_param = item_param[[ind_dataset]]
  
  fit = all_models[[ind_dataset]]$model
  abil = all_abilities[ind_dataset,]
  resp = results[[ind_dataset]][ind_instance,]#resp = results[[ind_dataset]][ind_instance,]
  
  
  Probability <- c()
  Ability <- seq(-6,6,0.05)
  for (theta in Ability){
    a = all_models[[ind_dataset]]$item_param[ind_instance,3]
    b = all_models[[ind_dataset]]$item_param[ind_instance,2]
    c = all_models[[ind_dataset]]$item_param[ind_instance,1]
    y_temp <- c + (1-c)/(1+exp(-a*(theta-b)))
    Probability <- c(Probability,y_temp)
  }
  
  plot(Ability,Probability, main = main, type = "l",xlim=cbind(-4,4),ylim=cbind(0,1))
  par(new=TRUE) 
  
  plot(abil,resp,xlim=cbind(-4,4),ylim=cbind(0,1),xlab="",ylab="")
  
  xabil <- c(abil[(length(abil)-6):length(abil)])
  yresp <- c(resp[(length(resp)-6):length(resp)])
  
  points(xabil[1:(length(xabil)-2)], yresp[1:(length(yresp)-2)], xlim=cbind(-4,4),ylim=cbind(0,1),xlab="",ylab="",cex = .5, col = "green")
  points(xabil[(length(xabil)-1):(length(xabil))], yresp[(length(yresp)-1):(length(yresp))], xlim=cbind(-4,4),ylim=cbind(0,1),xlab="",ylab="",cex = .5, col = "red")
  
  text(xabil,yresp, labels=c("RndA","RndB", "RndC", "Maj","Min","Opt", "Pessim"), cex= 0.8, pos=4, font = 4, srt=90)
  
  
  
  # Random Classifiers CUT POINTS (ad-hoc)
  
  if (randomCuts){
    rnd <- c("RandomClass_A", "RandomClass_B", "RandomClass_C")
    
    # UPDATE : 3 Random Classifiers 
    for(i in 4:6){
      
      RandomModelDiff = abil[(length(abil)-i)]
      abline(v=RandomModelDiff, col="red", lty=2)
      
      a = all_models[[ind_dataset]]$item_param[ind_instance,3]
      b = all_models[[ind_dataset]]$item_param[ind_instance,2]
      c = all_models[[ind_dataset]]$item_param[ind_instance,1]
      theta = RandomModelDiff
      
      y = c + (1-c)/(1+exp(-a*(theta-b)))
      abline(h=y, col="red", lty=2)
      
      #print probability of success
      text(x=-3.8, y=y,paste(round(y, digits=4)),cex= 0.8, pos=3)
      
      # Success or fail? (I depend just on 1 random classifier)
      if(results[[1]][ind_instance,rnd[i-3]] == 1){ #results[[ind_dataset]]
        text(x=RandomModelDiff, y=1,paste(round(RandomModelDiff, digits=4)),cex= 0.8, pos=2)
      }else{
        text(x=RandomModelDiff, y=0,paste(round(RandomModelDiff, digits=4)),cex= 0.8, pos=2)
      }
    }
    
  }
  
  
} 




plotICCi<- function(i,x1=-4,x2=4){
    plot_ICC(all_models, results, all_abilities,1,i)
}



#Extract data from the binary responses given by the classifiers (n datasets)
extract_data_n <- function(nas=FALSE, all= FALSE){
  
  
  for (ind_dataset in 1:length(ListDS_Results))
  {
    print(paste("DS:  ",datasets[ind_dataset]))
    
    results[[ind_dataset]] <<- ListDS_Results[[ind_dataset]]*1#From logical to numerical
    
    # Are there NA's in the results (no predictions for items)?
    if(nas){  
      if (sum(is.na(ListDS_Results[[ind_dataset]]))){
        results[[ind_dataset]][is.na(results[[ind_dataset]])] <<- 0
      }
    }
    
    #Avoid items with one response category
    if(all){
      clean<-c()
      for (i in 1:nrow(results[[ind_dataset]])){
        if (length(unique(results[[ind_dataset]][i,]))>1){
          clean <- c(clean,i)
        }
      }
      results[[ind_dataset]] <<- result[[ind_dataset]][clean,]
    }
    
    
    t_results <- t(results[[ind_dataset]])
    
    oldw <- getOption("warn")
    options(warn = -1)
    
    
    #print("IRT... ")
    
      
      print(paste("IRT LTM... dataset: ",ind_dataset,sep=""))
      IRTstuff<- fit_IRT(t_results,3)
      #print("IRT LTM_RND... ")
      #IRTstuff<- fit_IRT(t_results,3,TRUE)
      #print("IRT MIRT... ")
      #IRTstuff<- fit_mIRT(t_results,3)
      #print("IRT MIRT_RND... ")
      #IRTstuff<- fit_mIRT(t_results,3,TRUE)
      
      
      print("Finished")
      options(warn = oldw)
      
      
      
      item_param[[ind_dataset]] <<- IRTstuff$item_param
      all_abilities[ind_dataset,] <<- IRTstuff$abil_vector
      acc[ind_dataset,] <<- colMeans(results[[ind_dataset]],na.rm = TRUE)
      all_models[[ind_dataset]] <<- IRTstuff
      
      
      # Compute Average Probability of succes for the all the  Classifiers 
      
      abil = all_abilities[ind_dataset,]
      avgProbsT = vector()
      for(m in 1:length(methods)){
        C_method_probs <- vector()
        
        for (ind_inst in 1:nrow(results[[ind_dataset]])){
          
          ModelProf = abil[m]
          a = all_models[[ind_dataset]]$item_param[ind_inst,3]
          b = all_models[[ind_dataset]]$item_param[ind_inst,2]
          c = all_models[[ind_dataset]]$item_param[ind_inst,1]
          theta = ModelProf
          y = c + (1-c)/(1+exp(-a*(theta-b)))
          C_method_probs <- c(C_method_probs,y)
        }
        
        MethodAvgProb <- sum(C_method_probs)/length(C_method_probs)
        avgProbsT <- c(avgProbsT, MethodAvgProb)
      }
      
      avgProbs[ind_dataset,] <<- avgProbsT  
    #if74
      
      
  }#for
  
  
  
  
  save(item_param, file=paste(ds,"irt_parameters_mc.RData",sep=""))
  save(all_abilities, file =paste(ds,"algor_abilities_mc.RData",sep=""))
  save(acc,file=paste(ds,"algor_accuracies_mc.RData",sep=""))
  save(results, file=paste(ds,"results_responses_mc.RData",sep=""))
  save(all_models, file=paste(ds, "all_3P_IRT_models_mc.RData",sep=""))
  save(avgProbs, file=paste(ds, "all_avgProbs.RData",sep=""))
  
  
}


#load data extracted

load_data <- function(){

  load(paste(ds,"irt_parameters_mc.RData",sep=""))
  load(paste(ds, "algor_abilities_mc.RData",sep=""))
  load(paste(ds,"algor_accuracies_mc.RData",sep=""))
  load(paste(ds,"results_responses_mc.RData",sep=""))
  load(paste(ds,"all_3P_IRT_models_mc.RData",sep=""))
  load(paste(ds,"all_avgProbs.RData",sep=""))
  

}


# CleanDS

cleanDS <- function(){
  

  
}
###############################################
#############      TESTING      ###############
############################################### 

testingSet <- function(ICC = FALSE, data = TRUE, vs = TRUE, hist  = TRUE, tablesAbil=TRUE , cleanDS = FALSE){
  
  load(paste(ds,"irt_parameters_mc.RData",sep=""))
  load(paste(ds, "algor_abilities_mc.RData",sep=""))
  load(paste(ds,"algor_accuracies_mc.RData",sep=""))
  load(paste(ds,"results_responses_mc.RData",sep=""))
  load(paste(ds,"all_3P_IRT_models_mc.RData",sep=""))
  load(paste(ds,"all_avgProbs.RData",sep=""))
  
  PDFEPS <<- 1 # 0 None, 1 PDF, 2 EPS
  PDFheight <<- 7 # 7 by default, so 14 makes it double higher than wide, 5 makes letters bigger (in proportion) for just one 
  PDFwidth <<- 7 # 7 by default
  
  for(ind_dataset in 1:length(datasets)){
    
    datos <- read.csv(paste(ds,datasets[ind_dataset],sep=""))
    datos$Class <- as.factor(datos$Class)
    nameDS <- datasets[ind_dataset]
    nameDS <- strsplit(nameDS,"[.]")[[1]][1] #keep just the name
    
    #Clean DS: avoid instances with discriminant < 0 
    
    negative <- as.vector(which(item_param[[ind_dataset]][,"Dscrmn"] < 0))
    printNoise = paste(negative, collapse = " ")
    write(paste("Noisy instances (",nameDS,"): ",printNoise, sep=""),file = paste(ds,"Noise_Detection.txt",sep=""), append =  TRUE)
    
    if (cleanDS){
     
      
      write.csv(datos[-negative,], file=paste(ds,nameDS,"_clean.csv", sep=""), row.names = FALSE)
    }
    
    
    #Data to print
    numDatos = nrow(datos)
    numClasses = length(unique(datos$Class))
    propClasses = paste(round(table(datos$Class)/numDatos,2))
    printPropClasses= paste(propClasses, collapse = " ")
    
    
    
    #cbind dataset + IRT parameters + discriminant<0 + errorAvg (stuff used for plotting, visualisation and testing... room for improvement)
    do <- datos
    do <- cbind(do, item_param[[ind_dataset]])
    do$avgError <- rowMeans(results[[ind_dataset]], na.rm = T)
    for (i in 1:nrow(do)){
      do$DiscLess0[i] = item_param[[ind_dataset]][i,"Dscrmn"]<0
      do$DiscLess0_label[i] = if (item_param[[ind_dataset]][i,"Dscrmn"]<0){"x"}else{"o"}
    }
    
    #write.table(do, file= paste(ds,nameDS,"_IRT.txt",sep=""))
    
    print(paste("___",ind_dataset,"___ DS:",nameDS))
    
    
    if (data){
      print("Print Data/Noise...")
      if(ncol(datos)<=4){ #   # 2 dimensions datasets plot: identifier, x, y and Class
        
        PDFwidth <<- 14 # 7 by default
        
        ####  DATA POINTS  LABELLED
        openPDFEPS(paste(ds,nameDS,"_points_labelled", sep=""))
        mainPlot<- ggplot(do,aes(x,y, colour= factor(Class), label= row.names(do))) + geom_point(size = 3.5) + theme_bw() + geom_text(check_overlap = F ,size=4, hjust = 0, nudge_x = 0.055) 
        print(mainPlot)
        dev.off()
        
        PDFwidth <<- 6
        PDFheight <<- 4
        ####  DATA POINTS 
        openPDFEPS(paste(ds,nameDS,"_points", sep=""))
        mainPlot<- ggplot(do,aes(x,y, colour= factor(Class))) + geom_point(size = 3.5) + theme_bw()  
        print(mainPlot)
        dev.off()
        
        PDFwidth <<- 6
        PDFheight <<- 4 # 7 by default
        
      
        
        
      } else{# if not 2 dimensions then visualise two first principal components (PCA)
        
        # Compute PCA
        ir.pca <- prcomp(datos[,1:ncol(datos)-1],center = TRUE,scale. = TRUE) 
        #print(ir.pca)
        #plot(ir.pca, type = "l")
        
        #Plot using ggbiplot library
        openPDFEPS(paste(ds,nameDS,"_noise_PCA_good", sep=""))
        g <- ggbiplot(ir.pca, obs.scale = 1, var.scale = 1, 
                      groups = datos[,ncol(datos)], ellipse = TRUE, var.axes = T,
                      circle = FALSE, labels = do[,"DiscLess0_label"])
        g <- g + scale_color_discrete(name = '') + theme_bw() 
        g <- g + theme(legend.direction = 'horizontal', 
                       legend.position = 'top' )
        g <- g + labs(title = "", x = "PC1", y = "PC2", size =0.5) 
        
        print(g)
        dev.off()
        #ab <- ab + labs(title = paste(nameDS," [", numClasses," classes", " (", printPropClasses, ")]", sep=""), x = "abilities", y = "Classifier", size =0.5) 

        
      }
      
      
    }
    
    
    if(hist){
      
      PDFheight <<- 7 # 7 by default, so 14 makes it double higher than wide, 5 makes letters bigger (in proportion) for just one
      PDFwidth <<- 7 # 7 by default
      
      print("Print Histograms abil/acc...")
      
      #### Histogram abilities
      
      openPDFEPS(paste(ds,nameDS,"_abil_hist.pdf", sep=""))
      x <- all_abilities[ind_dataset,]
      hist(x,breaks=10, prob=T, col="grey") 
      lines(density(x,na.rm = T),col="blue", lwd=2)
      lines(density(x, adjust=2,na.rm = T), lty="dotted", col="darkgreen", lwd=2)   # add another "smoother" density
      dev.off()
      
      #### Histogram accuracies
      
      openPDFEPS(paste(ds,nameDS,"_acc_hist.pdf", sep=""))
      x <- acc[ind_dataset,]
      hist(x,breaks=20, prob=T, col="grey") 
      lines(density(x,na.rm = T),col="blue", lwd=2)
      lines(density(x, adjust=2,na.rm = T), lty="dotted", col="darkgreen", lwd=2)   # add another "smoother" density
      dev.off()
      
    }
    
    
    if (ICC){
      
      PDFwidth <<- 7
      PDFheight <<- 7
      print("Plot Noisy instances (ICCs)...")
      # plot those items with Discriminant < 0
      for (i in  as.vector(which(item_param[[ind_dataset]][,"Dscrmn"] < 0))){
        
        openPDFEPS(paste(ds,nameDS,"_Outliers_(point ",i,")", sep=""))
        plot_ICC_ECAI(all_models, results, all_abilities,ind_dataset,i)
        dev.off()
      }
      
      print("Plot Rest of instances (ICCs)")
      # plot those items with Discriminant > 0
      for (i in  as.vector(which(item_param[[ind_dataset]][,"Dscrmn"] > 0))){
        
        openPDFEPS(paste(ds,nameDS,"_Normal_(point ",i,")", sep=""))
        plot_ICC_ECAI(all_models, results, all_abilities,ind_dataset,i,randomCuts=T)
        dev.off()
      }
      
    }
  
   
    
    if(vs){
      print("Plot Diff/Dscrmn...")
      
      # Diff vs Discr
      openPDFEPS(paste(ds,nameDS,"_Diff_vs_Discr", sep=""))
      do2 <- do[which(do$Dffclt>-100),]
      do3 <- do2[which(do2$Dffclt<500),]
      do4 <- do3[which(do3$Dscrmn<250),]
      g<-ggplot(do4, aes(Dffclt, Dscrmn)) + geom_point() + theme_bw() 
      print(g)
      dev.off()
      
    }
   
    
    if(tablesAbil){
      print("Plot Table Abilities...")
      
    
      abil = all_abilities[ind_dataset,]
      avgProbsT = avgProbs[ind_dataset,]
      accuracy = acc[ind_dataset,]
      
 
      df <- data.frame(methods, abil, avgProbsT, accuracy, row.names = 1:length(methods), stringsAsFactors = FALSE)
      df$abil <- round(df$abil,4)
      df$avgProbs <- round(df$avgProbsT,4)
      df$accuracy <- round(df$accuracy,4)
     
      df$methods <- factor(df$methods, levels = df[order(df$abil, decreasing = F), "methods"]) 
      df <- df[order(df[,2], decreasing=F),]
      write.table(df, file=paste(ds,nameDS,"_tableAbilities.txt",sep=""))
     
      
      maxrow=35
      npages = ceiling(nrow(df)/maxrow)
      pdf(paste(ds,nameDS,"_tableAbilities.pdf",sep=""), height = 11, width=8.5)
      idx = seq(1, maxrow)
      if (maxrow >= nrow(df)){
        grid.table(df[idx,])
      }else{
        grid.table(df[idx,])
        for (i in 2:npages){
          grid.newpage()
          if (i*maxrow <= nrow(df)){
            idx = seq(1+((i-1)*maxrow), i*maxrow)
            
          }else{
            idx = seq(1+((i-1)*maxrow), nrow(df))
          }
          grid.table(df[idx,],rows=NULL)
        }
        
      }
      
      dev.off()
      
      
      # Plot abilities
      
      labelsClass <- c("RandomClass_A", "RandomClass_B", "RandomClass_C","MajorityClass", "MinorityClass", "OptimalClass","PessimalClass")
      df$label = df$method %in% labelsClass
      

      PDFEPS <<- 1 # 0 None, 1 PDF, 2 EPS
      PDFheight <<- 7 # 7 by default, so 14 makes it double higher than wide, 5 makes letters bigger (in proportion) for just one 
      PDFwidth <<- 9 # 7 by default
      
      openPDFEPS(paste(ds,nameDS,"_plotAll", sep=""))
      
      ab <- ggplot(df, aes(abil, reorder(methods,abil))) + geom_point(size=1)  + coord_cartesian(xlim = c(-6,6))
      ab <- ab + theme_bw() + theme(axis.text=element_text(size=4),axis.title=element_text(size=4,face="bold"), plot.title = element_text(size=5))
      ab <- ab + geom_point(data = subset(df, label == T),colour="green", size=1) 
      ab <- ab + geom_text(aes(label = ifelse(label == T, as.character(methods),'')),hjust = 0, nudge_x = 0.055, size = 1.5)
      ab <- ab + labs(title = paste(nameDS," [", numClasses," classes", " (", printPropClasses, ")]", sep=""), x = "abilities", y = "Classifier", size =0.5) 

      #dev.off()
    
      # Plot probSucces

      #openPDFEPS(paste(ds,nameDS,"_plotProbSucces", sep=""))
      
      pS <- ggplot(df, aes(avgProbs, reorder(methods,avgProbs))) + geom_point(size=1) + coord_cartesian(xlim = c(0,1))
      pS <- pS + theme_bw() + theme(axis.text=element_text(size=4),axis.title=element_text(size=4,face="bold"), plot.title = element_text(size=5))
      pS <- pS + geom_point(data = subset(df, label == T),colour="green", size=1) 
      pS <- pS + geom_text(aes(label = ifelse(label == T, as.character(methods),'')),hjust = 0, nudge_x = 0.055, size = 1.5)
      pS <- pS + labs(title = paste(nameDS," [", numClasses," classes", " (", printPropClasses, ")]", sep=""), x = "probSuccess", y = "Classifier",size=0.5) 
      #print(pS)
      
      #dev.off()
    
      # Plot Acc
      
      #openPDFEPS(paste(ds,nameDS,"_plotAccuracy", sep=""))
      
      pA <- ggplot(df, aes(accuracy, reorder(methods,accuracy))) + geom_point(size=1) + coord_cartesian(xlim = c(0,1))
      pA <- pA + theme_bw() + theme(axis.text=element_text(size=4),axis.title=element_text(size=4,face="bold"), plot.title = element_text(size=5))
      pA <- pA + geom_point(data = subset(df, label == T),colour="green", size=1) 
      pA <- pA + geom_text(aes(label = ifelse(label == T, as.character(methods),'')),hjust = 0, nudge_x = 0.055, size = 1.5)
      pA <- pA + labs(title = paste(nameDS," [", numClasses," classes", " (", printPropClasses, ")]", sep=""), x = "Acc", y = "Classifier",size=0.5) 
      #print(pA)
      
      #dev.off()    grid.arrange(p1, p2,p3,p4,  ncol= 2, nrow = 2)
      
      grid.arrange(ab,pS,pA, ncol=3)      
      dev.off()
      
      
      # acc per probSucces/ability
      
      PDFEPS <<- 1 # 0 None, 1 PDF, 2 EPS
      PDFheight<<- 5 # 7 by default, so 14 makes it double higher than wide, 5 makes letters bigger (in proportion) for just one 
      PDFwidth<<- 10 # 7 by default
      openPDFEPS(paste(ds,nameDS,"_plotAllAcc_good", sep=""))
       
      aA <- ggplot(df, aes(abil, accuracy)) + geom_point(size=2) + coord_cartesian(xlim = c(-4,4), ylim=c(0,1))
      aA <- aA + theme_bw() + theme(axis.text=element_text(size=8),axis.title=element_text(size=8,face="bold"), plot.title = element_text(size=6), panel.grid.minor = element_blank())
      aA <- aA + geom_point(data = subset(df, label == T),colour="green", size=1)
      aA <- aA + geom_text_repel(data = subset(df, label == T), aes(label =  as.character(methods)), nudge_x = 0.055, size = 3.5)
      #aA <- aA + geom_text(aes(label = ifelse(label == T, as.character(methods),'')), nudge_x = 0.055, size = 2.5)
      aA <- aA + labs(title = paste(nameDS," [", numClasses," classes", " (", printPropClasses, ")]", sep=""), x = "Ability", y = "Accuracy",size=0.5)

      
      aP <- ggplot(df, aes(avgProbs, accuracy)) + geom_point(size=2) + coord_cartesian(xlim = c(0,1), ylim=c(0,1))
      aP <- aP + theme_bw() + theme(axis.text=element_text(size=8),axis.title=element_text(size=8,face="bold"), plot.title = element_text(size=6), panel.grid.minor = element_blank())
      aP <- aP + geom_point(data = subset(df, label == T),colour="green", size=1)
      aP <- aP + geom_text_repel(data = subset(df, label == T), aes(label = as.character(methods)), nudge_x = 0.055, size = 3.5)
      #aP <- aP + geom_text_repel(aes(label = ifelse(label == T, as.character(methods),'')),hjust = 0, nudge_x = 0.055, size = 2.5)
      aP <- aP + labs(title = paste(nameDS," [", numClasses," classes", " (", printPropClasses, ")]", sep=""), x = "Average Probability of Success", y = "Accuracy",size=0.5)
      
      
      
      grid.arrange(aA,aP, ncol=2)      
      
      dev.off()
      print("End")
      
      if(compModels){
      
        abil_plots[[ind_dataset]] <<- ab
        success_plots[[ind_dataset]] <<- pS
        acc_plots[[ind_dataset]] <<- pA
        accAbil_plots[[ind_dataset]] <<- aA
        accProb_plots[[ind_dataset]] <<- aP
        
      }
      
    }#if
    
   
 
    
    
    
  }#for
  

  
 

  
}


MCC <- function(Min = -3.8, Max = 4, groups=6){
  
  
  load(paste(ds,"irt_parameters_mc.RData",sep=""))
  load(paste(ds, "algor_abilities_mc.RData",sep=""))
  load(paste(ds,"algor_accuracies_mc.RData",sep=""))
  load(paste(ds,"results_responses_mc.RData",sep=""))
  load(paste(ds,"all_3P_IRT_models_mc.RData",sep=""))
  load(paste(ds,"all_avgProbs.RData",sep=""))
  
  PDFEPS <<- 1 # 0 None, 1 PDF, 2 EPS
  PDFheight <<- 7 # 7 by default, so 14 makes it double higher than wide, 5 makes letters bigger (in proportion) for just one 
  PDFwidth <<- 7 # 7 by default
  
  
  datos <- read.csv(paste(ds,datasets[ind_dataset],sep=""))
  datos$Class <- as.factor(datos$Class)
  nameDS <- datasets[ind_dataset]
  nameDS <- strsplit(nameDS,"[.]")[[1]][1] #keep just the name
  
  #Clean DS: avoid instances with discriminant < 0 
  

  #Data to print
  numDatos = nrow(datos)
  numClasses = length(unique(datos$Class))
  propClasses = paste(round(table(datos$Class)/numDatos,2))
  printPropClasses= paste(propClasses, collapse = " ")
  
  
  #cbind dataset + IRT parameters + discriminant<0 + errorAvg (stuff used for plotting, visualisation and testing... room for improvement)
  do <- datos
  do <- cbind(do, item_param[[ind_dataset]])
  do$avgError <- rowMeans(results[[ind_dataset]], na.rm = T)
  for (i in 1:nrow(do)){
    do$DiscLess0[i] = item_param[[ind_dataset]][i,"Dscrmn"]<0
    do$DiscLess0_label[i] = if (item_param[[ind_dataset]][i,"Dscrmn"]<0){"x"}else{"o"}
  }
  
  #write.table(do, file= paste(ds,nameDS,"_IRT.txt",sep=""))
  
  print(paste("___",ind_dataset,"___ DS:",nameDS))
  
  doOld <-do
  
  #library(dplyr)
  do <- doOld
  do <- tbl_df(do)
  do <- filter(do, Dffclt > Min, Dffclt < Max)
  
  #do$cuts <- cut(do$Dffclt, groups)
  #2do$cuts <- cut(do$Dffclt, breaks = c(-3, -2.6, -1.4, -1, -0.5, 4))
  do$cuts <- cut2(do$Dffclt, g= groups)
  
  
  
  Classifier = c("OptimalClass","PessimalClass","RandomClass_A","RandomClass_B",
                 "RandomClass_C","fda_prune2","rpart", "JRip", "J48", "svmLinear_C0.01", 
                 "Ibk_k2","rf_mtry2", "avNNet_decay0")
  
  for (i in Classifier){
    do[,i] <- results[[ind_dataset]][1:nrow(do),i]
  }
  
  do[,"Random"] <-  (do[,"RandomClass_A"] + do[,"RandomClass_B"] + do[,"RandomClass_C"])/3
  
  by_bin <- group_by(do,cuts)
  by_bin_acc <- summarise(by_bin, 
                          Rnd=mean(Random, na.rm =T),
                          #Opt = mean(OptimalClass, na.rm =T),
                          #Pess = mean(PessimalClass, na.rm =T),
                          fda = mean(fda_prune2,na.rm =T),
                          rpart = mean(rpart,na.rm =T),
                          JRip = mean(JRip, na.rm =T),
                          J48 = mean(J48, na.rm =T),
                          SVM = mean(svmLinear_C0.01, na.rm =T),
                          IBK = mean(Ibk_k2, na.rm =T),
                          RF = mean(rf_mtry2, na.rm =T),
                          NN = mean(avNNet_decay0, na.rm =T)
  )
  
  
  print(by_bin_acc[])
  
 
  for (i in 2:10){
    by_bin_acc[6,i] = by_bin_acc[6,i] -0.1
  }
  
  print(by_bin_acc[])
  
  by_bin_d <- summarise(by_bin, meanAbility = mean(Dffclt))
  
  
  #library(reshape2)
  melted <- melt(as.data.frame(by_bin_acc))
  
  colnames(melted)[2]<- "Classifier"
  
  PDFEPS <<- 1 # 0 None, 1 PDF, 2 EPS
  PDFheight<<- 3 # 7 by default, so 14 makes it double higher than wide, 5 makes letters bigger (in proportion) for just one 
  PDFwidth<<- 7 # 7 by default
  
  openPDFEPS(paste(ds,nameDS,"_MCC", sep=""))
  
  MCC <- ggplot(melted, aes(cuts,value, colour=Classifier, group = Classifier)) + geom_line() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + coord_cartesian(ylim=c(0.25,1))
  MCC <- MCC + theme_bw() + theme(panel.grid.minor = element_blank())
  MCC <- MCC + labs(title = "", x = "Difficulty", y = "Accuracy")
  MCC <- MCC + scale_x_discrete(breaks=levels(melted$cuts), labels=round(by_bin_d$meanAbility,2))
  print(MCC)
  print(table(do$cuts))
  
  
  dev.off()
  
  
  
 }



MCC_discriminant <- function(groups=10){
  
  
  load(paste(ds,"irt_parameters_mc.RData",sep=""))
  load(paste(ds, "algor_abilities_mc.RData",sep=""))
  load(paste(ds,"algor_accuracies_mc.RData",sep=""))
  load(paste(ds,"results_responses_mc.RData",sep=""))
  load(paste(ds,"all_3P_IRT_models_mc.RData",sep=""))
  load(paste(ds,"all_avgProbs.RData",sep=""))
  
  PDFEPS <<- 1 # 0 None, 1 PDF, 2 EPS
  PDFheight <<- 7 # 7 by default, so 14 makes it double higher than wide, 5 makes letters bigger (in proportion) for just one 
  PDFwidth <<- 7 # 7 by default
  
  
  datos <- read.csv(paste(ds,datasets[ind_dataset],sep=""))
  datos$Class <- as.factor(datos$Class)
  nameDS <- datasets[ind_dataset]
  nameDS <- strsplit(nameDS,"[.]")[[1]][1] #keep just the name
  
  #Clean DS: avoid instances with discriminant < 0 
  
  
  #Data to print
  numDatos = nrow(datos)
  numClasses = length(unique(datos$Class))
  propClasses = paste(round(table(datos$Class)/numDatos,2))
  printPropClasses= paste(propClasses, collapse = " ")
  
  
  #cbind dataset + IRT parameters + discriminant<0 + errorAvg (stuff used for plotting, visualisation and testing... room for improvement)
  do <- datos
  do <- cbind(do, item_param[[ind_dataset]])
  do$avgError <- rowMeans(results[[ind_dataset]], na.rm = T)
  for (i in 1:nrow(do)){
    do$DiscLess0[i] = item_param[[ind_dataset]][i,"Dscrmn"]<0
    do$DiscLess0_label[i] = if (item_param[[ind_dataset]][i,"Dscrmn"]<0){"x"}else{"o"}
  }
  
  #write.table(do, file= paste(ds,nameDS,"_IRT.txt",sep=""))
  
  print(paste("___",ind_dataset,"___ DS:",nameDS))
  
  doOld <-do
  
  #library(dplyr)
  do <- doOld
  do <- tbl_df(do)
  #do <- filter(do, Dffclt > Min, Dffclt < Max)
  
  #do$cuts <- cut(do$Dscrmn, groups)
  do$cuts <- cut(do$Dscrmn, breaks = c(-100,0, 1, 2, 4, 8, 16, 100),include.lowest = T)
  #do$cuts <- cut2(do$Dscrmn, g= groups)
  
  
  Classifier = c("OptimalClass","PessimalClass","RandomClass_A","RandomClass_B",
                 "RandomClass_C","fda_prune2","rpart", "JRip", "J48", "svmLinear_C0.01", 
                 "Ibk_k2","rf_mtry2", "avNNet_decay0")
  
  for (i in Classifier){
    do[,i] <- results[[ind_dataset]][1:nrow(do),i]
  }
  
  do[,"Random"] <-  (do[,"RandomClass_A"] + do[,"RandomClass_B"] + do[,"RandomClass_C"])/3
  
  by_bin <- group_by(do,cuts)
  by_bin_acc <- summarise(by_bin, 
                          Rnd=mean(Random, na.rm =T),
                          #Opt = mean(OptimalClass, na.rm =T),
                          #Pess = mean(PessimalClass, na.rm =T),
                          fda = mean(fda_prune2,na.rm =T),
                          rpart = mean(rpart,na.rm =T),
                          JRip = mean(JRip, na.rm =T),
                          J48 = mean(J48, na.rm =T),
                          SVM = mean(svmLinear_C0.01, na.rm =T),
                          IBK = mean(Ibk_k2, na.rm =T),
                          RF = mean(rf_mtry2, na.rm =T),
                          NN = mean(avNNet_decay0, na.rm =T)
  )
  
  
  print(by_bin_acc[])
  
  by_bin_d <- summarise(by_bin, meanAbility = mean(Dscrmn))
  
  
  
  #library(reshape2)
  melted <- melt(as.data.frame(by_bin_acc))
  
  colnames(melted)[2]<- "Classifier"
  
  PDFEPS <<- 1 # 0 None, 1 PDF, 2 EPS
  PDFheight<<- 3 # 7 by default, so 14 makes it double higher than wide, 5 makes letters bigger (in proportion) for just one 
  PDFwidth<<- 7 # 7 by default
  
  openPDFEPS(paste(ds,nameDS,"_MCCdiscriminant", sep=""))
  
  MCCd <- ggplot(melted, aes(cuts,value, colour=Classifier, group = Classifier)) + geom_line() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + coord_cartesian(ylim=c(0,1))
  MCCd <- MCCd + theme_bw() + theme(panel.grid.minor = element_blank())
  MCCd <- MCCd + labs(title = "", x = "Discrimination", y = "Accuracy")
  #MCCd <- MCCd + scale_x_discrete(breaks=levels(melted$cuts), labels=round(by_bin_d$meanAbility,2))
  print(MCCd)
  print(table(do$cuts))
  
  
  dev.off()
  
  
  
}


run<- function(){
  print("Extract Data")
  extract_data_n(nas=FALSE, all= FALSE)
  print("Testing")
  testingSet()
}
