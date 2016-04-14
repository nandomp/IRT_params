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
options(scipen = 999)

OptimimalParam = TRUE
binaryClass = FALSE
weArePlaying = TRUE


if (weArePlaying){
  ds <- "_Toy_/"
  
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

.lib<- c("ltm","devtools", "ggplot2","mirt","stats","devtools","gridExtra")
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
PDFwidth= 9 # 7 by default

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
  #resp = results[[ind_dataset]][ind_instance,]
  resp = results[[1]][ind_instance,]
  
  plot(fit,items=ind_instance,xlim=cbind(-4,4),ylim=cbind(0,1),annot=FALSE,main = main)
  par(new=TRUE) 
  plot(abil,resp,xlim=cbind(-4,4),ylim=cbind(0,1),xlab="",ylab="")
 
  xabil <- c(abil[(length(abil)-6):length(abil)])
  yresp <- c(resp[(length(resp)-6):length(resp)])

  points(xabil[1:(length(xabil)-2)], yresp[1:(length(yresp)-2)], xlim=cbind(-4,4),ylim=cbind(0,1),xlab="",ylab="",cex = .5, col = "green")
  points(xabil[(length(xabil)-1):(length(xabil))], yresp[(length(yresp)-1):(length(yresp))], xlim=cbind(-4,4),ylim=cbind(0,1),xlab="",ylab="",cex = .5, col = "red")
  
  text(xabil,yresp, labels=c("RndA","RndB", "RndC", "Maj","Min","Opt", "Dread"), cex= 0.8, pos=4, font = 4, srt=90)
  
  
  
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

#generic PLOT (used for the  models generated with the MIRT package)
plot_mICC <- function(all_models,results,all_abilities,ind_dataset,ind_instance, main ="Item Characteristic Curve", randomCuts = TRUE){
  ## plots the ICC of the "ind_instance-th" instance of the "ind_dataset-th" dataset
  
  #item_param = item_param[[ind_dataset]]
  
  fit = all_models[[ind_dataset]]$model
  abil = all_abilities[ind_dataset,]
  resp = results[[1]][ind_instance,]#resp = results[[ind_dataset]][ind_instance,]
  
  
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
  
  text(xabil,yresp, labels=c("RndA","RndB", "RndC", "Maj","Min","Opt", "Dread"), cex= 0.8, pos=4, font = 4, srt=90)
  
  
  
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

# openPDFEPS("ICC_376")
# plotICCi(146,-4,4)
# dev.off()

#ind_dataset = 1


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
    
    
    print("IRT... ")
    # print("IRT LTM... ")
    # IRTstuff<- fit_IRT(t_results,3)
    # print("IRT LTM_RND... ")
    # IRTstuff<- fit_IRT(t_results,3,TRUE)
    # print("IRT MIRT... ")
    # IRTstuff<- fit_mIRT(t_results,3)
    print("IRT MIRT_RND... ")
    IRTstuff<- fit_mIRT(t_results,3,TRUE)
   
    
    print("Finished")
    options(warn = oldw)
    
  
      
    item_param[[ind_dataset]] <<- IRTstuff$item_param
    all_abilities[ind_dataset,] <<- IRTstuff$abil_vector
    acc[ind_dataset,] <<- colMeans(results[[ind_dataset]],na.rm = TRUE)
    all_models[[ind_dataset]] <<- IRTstuff
    

  }
  
  save(item_param, file=paste(ds,"irt_parameters_mc.RData",sep=""))
  save(all_abilities, file =paste(ds,"algor_abilities_mc.RData",sep=""))
  save(acc,file=paste(ds,"algor_accuracies_mc.RData",sep=""))
  save(results, file=paste(ds,"results_responses_mc.RData",sep=""))
  save(all_models, file=paste(ds, "all_3P_IRT_models_mc.RData",sep=""))

  
}


#load data extracted
load_data <- function(){

  load(paste(ds,"irt_parameters_mc.RData",sep=""))
  load(paste(ds, "algor_abilities_mc.RData",sep=""))
  load(paste(ds,"algor_accuracies_mc.RData",sep=""))
  load(paste(ds,"results_responses_mc.RData",sep=""))
  load(paste(ds,"all_3P_IRT_models_mc.RData",sep=""))

}



###############################################
#############      TESTING      ###############
###############################################


testingSet <- function(ICC = FALSE){
  
  # load("irt_parameters_mc.RData")
  # load("algor_abilities_mc.RData")
  # load("algor_accuracies_mc.RData")
  # load("results_responses_mc.RData")
  # load("all_3P_IRT_models_mc.RData")
  # load("Methods.RData")
  
  for(ind_dataset in 1:length(datasets)){
    
    datos <- read.csv(paste(ds,datasets[ind_dataset],sep=""))
    datos$Class <- as.factor(datos$Class)
    nameDS <- datasets[ind_dataset]
    nameDS <- strsplit(nameDS,"[.]")[[1]][1] #keep just the name
    
    #cbind dataset + IRT parameters + discriminant<0 + errorAvg (stuff used for plotting, visualisation and testing... room for improvement)
    do <- datos
    do <- cbind(do, item_param[[ind_dataset]])
    do$avgError <- rowMeans(results[[ind_dataset]], na.rm = T)
    for (i in 1:nrow(do)){
      do$DiscLess0[i] = item_param[[ind_dataset]][i,"Dscrmn"]<0
      do$DiscLess0_label[i] = if (item_param[[ind_dataset]][i,"Dscrmn"]<0){"x"}else{"o"}
    }
    
    write.csv(do, file= paste(ds,nameDS,"_IRT.csv",sep=""))
    
    print(paste("___",ind_dataset,"___ DS:",nameDS))
    print("Print Data/Noise...")
    
    if(ncol(datos)<=4){ #   # 2 dimensions datasets plot: identifier, x, y and Class
   
   
      ####  DATA POINTS
      openPDFEPS(paste(ds,nameDS,"_points", sep=""))
      mainPlot<- ggplot(do,aes(x,y, colour= factor(Class), label= X)) + geom_point(size = 5.5) + geom_text(check_overlap = F ,size=4, hjust = 0, nudge_x = 0.055)
      print(mainPlot)
      dev.off()
      
      #### DATA POINTS + NOISE
      openPDFEPS(paste(ds,nameDS,"_noise", sep=""))
      p <- ggplot(do, aes(x,y, colour= Class, label= X)) + geom_point(size = 5.5) + geom_text(check_overlap = TRUE ,size=2, hjust = 0, nudge_x = 0.055)
      p <- p + geom_point(data = subset(do, DiscLess0 == T),colour="black", size=1.5)  
      dev.off()
      
    } else{# if not 2 dimensions then visualise two first principal components (PCA)
      
      # Compute PCA
      ir.pca <- prcomp(datos[,1:ncol(datos)-1],center = TRUE,scale. = TRUE) 
      print(ir.pca)
      plot(ir.pca, type = "l")
      
      #Plot using ggbiplot library
      openPDFEPS(paste(ds,nameDS,"_noise_PCA", sep=""))
      g <- ggbiplot(ir.pca, obs.scale = 1, var.scale = 1, 
                    groups = datos[,ncol(datos)], ellipse = TRUE, 
                    circle = TRUE, labels = do[,"DiscLess0_label"])
      g <- g + scale_color_discrete(name = '')
      g <- g + theme(legend.direction = 'horizontal', 
                     legend.position = 'top')
      
      print(g)
      dev.off()
      
    }
    
    
    
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
    
    
    if (ICC){
      print("Plot Noisy instances (ICCs)...")
      # plot those items with Discriminant < 0
      for (i in  as.vector(which(item_param[[ind_dataset]][,"Dscrmn"] < 0))){
        
        openPDFEPS(paste(ds,nameDS,"_Outliers_(point ",i,")", sep=""))
        plot_mICC(all_models[[ind_dataset]], results[[ind_dataset]], all_abilities[ind_dataset,],1,i)
        dev.off()
      }
      
      print("Plot Rest of instances (ICCs)")
      # plot those items with Discriminant > 0
      for (i in  as.vector(which(item_param[,"Dscrmn"] > 0))){
        
        openPDFEPS(paste(ds,nameDS,"_Normal_(point ",i,")", sep=""))
        plot_mICC(all_models[[ind_dataset]], results[[ind_dataset]], all_abilities[ind_dataset,],1,i)
        dev.off()
      }
      
    }
   
    
    print("Plot Diff/Dscrmn...")
    
    # Diff vs Discr
    openPDFEPS(paste(ds,nameDS,"_Diff_vs_Discr", sep=""))
    do2 <- do[which(do$Dffclt>-100),]
    do3 <- do2[which(do2$Dffclt<500),]
    do4 <- do3[which(do3$Dscrmn<250),]
    g<-ggplot(do4, aes(Dffclt, Dscrmn)) + geom_point()
    print(g)
    dev.off()
    
    
    print("Plot Table Abilities...")
    
    # Compute Average Probability of succes for the all the  Classifiers 
    
    abil = all_abilities[ind_dataset,]
    avgProbs = vector()
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
      avgProbs <- c(avgProbs, MethodAvgProb)
    }
    
    accuracy = acc[ind_dataset,]
    df = data.frame(cbind(methods, abil, avgProbs, accuracy))
    df[order(-df[,2],df[,4]),]
    
    write.csv(df, file=paste(ds,nameDS,"_tableAbilities.csv",sep=""))
    
    maxrow=35
    npages = ceiling(nrow(df)/maxrow)
    pdf(paste(ds,nameDS,"_tableAbilities.pdf",sep=""), height = 11, width=8.5)
    idx = seq(1, maxrow)
    if (maxrow >= nrow(df)){
      grid.table(df[idx,])
    }else{
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
    

    
  }
  
  
}


run<- function(){
  print("Extract Data")
  extract_data_n(nas=FALSE, all= FALSE)
  print("Testing")
  testingSet()
}








