###############################################
############# OPERATING OPTIONS ###############
###############################################
set.seed(998)
ind_instance = 1
ind_dataset=1
args <- commandArgs(trailingOnly = TRUE)
options(scipen = 999)

OptimimalParam = TRUE
binaryClass = TRUE
weArePlaying = TRUE

MODELS2COMPARE = 4


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

.lib<- c("ltm","devtools", "ggplot2","mirt","stats","devtools","gridExtra", "gridExtra")
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

all_abilities <-  matrix(rep(NA, numMethods * MODELS2COMPARE), nrow=MODELS2COMPARE, ncol=numMethods, byrow = T)
colnames(all_abilities) <- methods

acc <- matrix(rep(NA, numMethods * MODELS2COMPARE), nrow=MODELS2COMPARE, ncol=numMethods, byrow = T)
colnames(acc) <- methods

all_models <- list()



###############################################
###############    FUNCTIONS   ################
###############################################


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


# ind_dataset = 1
# ind_instance = 1
# main = "Item Characteristic Curve"

## Requires the ltm package
plot_ICC <- function(all_models,results,all_abilities,ind_dataset,ind_instance, main = "Item Characteristic Curve"){
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


## Requires the mirt package (UPDATE: NO)
plot_mICC <- function(all_models,results,all_abilities,ind_dataset,ind_instance, main ="Item Characteristic Curve"){
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
  
  #plot_ICC_limits(all_models, results, all_abilities,1,i,x1,x2)
  plot_ICC(all_models, results, all_abilities,1,i)
}


load_data <- function(){
  
  
  load("irt_parameters_mc.RData")
  load( "algor_abilities_mc.RData")
  load("algor_accuracies_mc.RData")
  load("results_responses_mc.RData")
  load( "all_3P_IRT_models_mc.RData")
  load("Methods.RData")
  
}






### EXperiments for the Hangout (31/03/16)
extract_data_comp_4hangout <- function(ind_dataset=1, nas=FALSE, all= FALSE, ICC = FALSE, data = FALSE, vs = FALSE, hist  = FALSE, tablesAbil=TRUE , cleanDS = FALSE){
  
  


  results[[ind_dataset]] <-ListDS_Results[[ind_dataset]]*1#From logical to numerical
  
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
  IRTstuff <- list()
  print("IRT LTM... ")
  IRTstuff[[1]]<- fit_IRT(t_results,3)
  print("IRT LTM_RND... ")
  IRTstuff[[2]]<- fit_IRT(t_results,3,TRUE)
  print("IRT MIRT... ")
  IRTstuff[[3]]<- fit_mIRT(t_results,3)
  print("IRT MIRT_RND... ")
  IRTstuff[[4]]<- fit_mIRT(t_results,3,TRUE)
  
  print("Finished")
  
  options(warn = oldw)
  
  item_param[[1]] <<- IRTstuff[[1]]$item_param
  item_param[[2]] <<- IRTstuff[[2]]$item_param
  item_param[[3]] <<- IRTstuff[[3]]$item_param
  item_param[[4]] <<- IRTstuff[[4]]$item_param
  
  
  all_abilities[1,] <<- IRTstuff[[1]]$abil_vector
  all_abilities[2,] <<- IRTstuff[[2]]$abil_vector
  all_abilities[3,] <<- IRTstuff[[3]]$abil_vector
  all_abilities[4,] <<- IRTstuff[[4]]$abil_vector
  
  
  
  acc = colMeans(results[[ind_dataset]],na.rm = TRUE)
  
  all_models[[1]] <<- IRTstuff[[1]]
  all_models[[2]] <<- IRTstuff[[2]]
  all_models[[3]] <<- IRTstuff[[3]]
  all_models[[4]] <<- IRTstuff[[4]]
  
  
  
  
  save(item_param, file=paste(ds,"irt_parameters_mc.RData",sep=""))
  save(all_abilities, file =paste(ds,"algor_abilities_mc.RData",sep=""))
  save(acc,file=paste(ds,"algor_accuracies_mc.RData",sep=""))
  save(results, file=paste(ds,"results_responses_mc.RData",sep=""))
  save(all_models, file=paste(ds, "all_3P_IRT_models_mc.RData",sep=""))
  
  
  
  ######################################################################################################################################
  
  load(paste(ds,"irt_parameters_mc.RData",sep=""))
  load(paste(ds, "algor_abilities_mc.RData",sep=""))
  load(paste(ds,"algor_accuracies_mc.RData",sep=""))
  load(paste(ds,"results_responses_mc.RData",sep=""))
  load(paste(ds,"all_3P_IRT_models_mc.RData",sep=""))
  
  
  datos <- read.csv(paste(ds,datasets[ind_dataset],sep=""))
  datos$Class <- as.factor(datos$Class)
  nameDS <- datasets[ind_dataset]
  nameDS <- strsplit(nameDS,"[.]")[[1]][1] #keep just the name
  
  #Clean DS: avoid instances with discriminant < 0 
  if (cleanDS){
    negative <- as.vector(which(item_param[[ind_dataset]][,"Dscrmn"] < 0))
    write.csv(datos[-negative,], file=paste(ds,nameDS,"_clean.csv", sep=""))
  }
  
  
  #Data to print
  numDatos = nrow(datos)
  numClasses = length(unique(datos$Class))
  propClasses = paste(round(table(datos$Class)/numDatos,2))
  printPropClasses= paste(propClasses, collapse = " ")
  
  
  
  
  #cbind dataset + IRT parameters + discriminant<0 + errorAvg
  do1 <- datos
  do1<-cbind(do1, item_param[[1]])
  do1$avgError <- rowMeans(results[[1]], na.rm = T)
  for (i in 1:nrow(do1)){
    do1$DiscLess0[i] = item_param[[1]][i,"Dscrmn"]<0
    do1$DiscLess0_label[i] = if (item_param[[1]][i,"Dscrmn"]<0){"x"}else{"o"}
  }
  
  
  do2 <- datos
  do2<-cbind(do2, item_param[[2]])
  do2$avgError <- rowMeans(results[[1]], na.rm = T)
  for (i in 1:nrow(do2)){
    do2$DiscLess0[i] = item_param[[2]][i,"Dscrmn"]<0
    do2$DiscLess0_label[i] = if (item_param[[2]][i,"Dscrmn"]<0){"x"}else{"o"}
  }
  
  do3 <- datos
  do3<-cbind(do3, item_param[[3]])
  do3$avgError <- rowMeans(results[[1]], na.rm = T)
  for (i in 1:nrow(do3)){
    do3$DiscLess0[i] = item_param[[3]][i,"Dscrmn"]<0
    do3$DiscLess0_label[i] = if (item_param[[3]][i,"Dscrmn"]<0){"x"}else{"o"}
  }
  
  do4 <- datos
  do4<-cbind(do4, item_param[[4]])
  do4$avgError <- rowMeans(results[[1]], na.rm = T)
  for (i in 1:nrow(do4)){
    do4$DiscLess0[i] = item_param[[4]][i,"Dscrmn"]<0
    do4$DiscLess0_label[i] = if (item_param[[4]][i,"Dscrmn"]<0){"x"}else{"o"}
  }
  
  
  
  if(data){
    # 2 dimensionsplot
    print("Print Data/Noise")
    library(gridExtra)
    
    if(ncol(datos)==4){ 
      openPDFEPS(paste(nameDS,"_points", sep=""))
      x <- datos[,1]
      y <- datos[,2]
      mainPlot<- ggplot(datos,aes(x,y, colour= factor(Class), label= X)) + geom_point(size = 5.5) + geom_text(check_overlap = F ,size=4, hjust = 0, nudge_x = 0.055)
      #vp <- viewport(width = 0.1, height =0.1, x = 0.5, y = 0.5)
      print(mainPlot)
      #print(mainPlot,vp =vp)
      dev.off()
      
      
      
      openPDFEPS(paste(nameDS,"_noise", sep=""))
      
      p1 <- ggplot(do1, aes(x,y, colour= Class, label= X)) + geom_point(size = 5.5) + geom_text(check_overlap = TRUE ,size=2, hjust = 0, nudge_x = 0.055)
      p1 <- p1 + geom_point(data = subset(do1, DiscLess0 == T),colour="black", size=1.5)  
      
      p2 <- ggplot(do2, aes(x,y, colour= Class, label= X)) + geom_point(size = 5.5) + geom_text(check_overlap = TRUE ,size=2, hjust = 0, nudge_x = 0.055)
      p2 <- p2 + geom_point(data = subset(do2, DiscLess0 == T),colour="black", size=1.5)  
      
      p3 <- ggplot(do3, aes(x,y, colour= Class, label= X)) + geom_point(size = 5.5) + geom_text(check_overlap = TRUE ,size=2, hjust = 0, nudge_x = 0.055)
      p3 <- p3 + geom_point(data = subset(do3, DiscLess0 == T),colour="black", size=1.5)  
      
      p4 <- ggplot(do4, aes(x,y, colour= Class, label= X)) + geom_point(size = 5.5) + geom_text(check_overlap = TRUE ,size=2, hjust = 0, nudge_x = 0.055)
      p4 <- p4 + geom_point(data = subset(do4, DiscLess0 == T),colour="black", size=1.5)  
      
      grid.arrange(p1, p2,p3,p4,  ncol= 2, nrow = 2)
      
      dev.off()
      
    } else{
      
      #not 2 dimensions => PCA
      
      library(stats)
      library(ggbiplot)
      
      ir.pca <- prcomp(datos[,1:ncol(datos)-1],center = TRUE,scale. = TRUE) 
      print(ir.pca)
      plot(ir.pca, type = "l")
      
      #library(devtools)
      #install_github("ggbiplot", "vqv")
      
      openPDFEPS(paste(nameDS,"_noise_PCA", sep=""))
      
      
      g1 <- ggbiplot(ir.pca, obs.scale = 1, var.scale = 1, 
                     groups = datos[,ncol(datos)], ellipse = TRUE, 
                     circle = TRUE, labels = do1[,"DiscLess0_label"])
      g1 <- g1 + scale_color_discrete(name = '')
      g1 <- g1 + theme(legend.direction = 'horizontal', 
                       legend.position = 'top')
      
      g2 <- ggbiplot(ir.pca, obs.scale = 1, var.scale = 1, 
                     groups = datos[,ncol(datos)], ellipse = TRUE, 
                     circle = TRUE, labels = do2[,"DiscLess0_label"])
      g2 <- g2 + scale_color_discrete(name = '')
      g2 <- g2 + theme(legend.direction = 'horizontal', 
                       legend.position = 'top')
      
      g3 <- ggbiplot(ir.pca, obs.scale = 1, var.scale = 1, 
                     groups = datos[,ncol(datos)], ellipse = TRUE, 
                     circle = TRUE, labels = do3[,"DiscLess0_label"])
      g3 <- g3 + scale_color_discrete(name = '')
      g3 <- g3 + theme(legend.direction = 'horizontal', 
                       legend.position = 'top')
      
      g4 <- ggbiplot(ir.pca, obs.scale = 1, var.scale = 1, 
                     groups = datos[,ncol(datos)], ellipse = TRUE, 
                     circle = TRUE, labels = do4[,"DiscLess0_label"])
      g4 <- g4 + scale_color_discrete(name = '')
      g4 <- g4 + theme(legend.direction = 'horizontal', 
                       legend.position = 'top')
      
      
      
      grid.arrange(g1, g2,g3,g4, ncol= 2, nrow = 2)
      
      
      dev.off()
      
      
    }
    
  }
 
  
  
  if(hist){
    print("Print Histograms abil/acc")
    
    #### Histogram abilities
    
    openPDFEPS(paste(nameDS,"_abil_hist.pdf", sep=""))
    
    old.par <- par(mfrow=c(2, 2))
    
    abilities <- all_abilities[1,]
    hist(abilities,breaks=20, prob=T, col="grey", main = "Histogram of abilities (ltm)") 
    lines(density(abilities,na.rm = T),col="blue", lwd=2)
    lines(density(abilities, adjust=2,na.rm = T), lty="dotted", col="darkgreen", lwd=2)   # add another "smoother" density
    
    abilities <- all_abilities[2,]
    hist(abilities,breaks=20, prob=T, col="grey", main = "Histogram of abilities (ltm rnd)") 
    lines(density(abilities,na.rm = T),col="blue", lwd=2)
    lines(density(abilities, adjust=2,na.rm = T), lty="dotted", col="darkgreen", lwd=2)   # add another "smoother" density
    
    abilities <- all_abilities[3,]
    hist(abilities,breaks=20, prob=T, col="grey", main = "Histogram of abilities (mirt)") 
    lines(density(abilities,na.rm = T),col="blue", lwd=2)
    lines(density(abilities, adjust=2,na.rm = T), lty="dotted", col="darkgreen", lwd=2)   # add another "smoother" density
    
    abilities <- all_abilities[4,]
    hist(abilities,breaks=20, prob=T, col="grey", main = "Histogram of abilities (mirt rnd)") 
    lines(density(abilities,na.rm = T),col="blue", lwd=2)
    lines(density(abilities, adjust=2,na.rm = T), lty="dotted", col="darkgreen", lwd=2)   # add another "smoother" density
    
    par(old.par)
    
    dev.off()
    
  }
  
  if(hist){
  
    #### Histogram accuracies
    
    openPDFEPS(paste(nameDS,"_acc_hist.pdf", sep=""))
    
    x <- acc
    hist(x,breaks=20, prob=T, col="grey") 
    lines(density(x,na.rm = T),col="blue", lwd=2)
    lines(density(x, adjust=2,na.rm = T), lty="dotted", col="darkgreen", lwd=2)   # add another "smoother" density
    
    dev.off()
    
    
    
    
    print("Print Noise ICC")
    # plot those items with Discriminant < 0
    for (i in  as.vector(which(item_param[[1]][,"Dscrmn"] < 0))){
      
      openPDFEPS(paste(nameDS,"_Outliers_(point ",i,")", sep=""))
      old.par <- par(mfrow=c(2, 2))
      
      plot_ICC(all_models, results, all_abilities,1,i, main="Item Characteristic Curve (LTM)")
      
      plot_ICC(all_models, results, all_abilities,2,i, main="Item Characteristic Curve (LTM RND)")
      
      plot_mICC(all_models, results, all_abilities,3,i, main="Item Characteristic Curve (MIRT)")
      
      plot_mICC(all_models, results, all_abilities,4,i, main="Item Characteristic Curve (MIRT RND)")
      
      par(old.par)
      dev.off()
      
    }
    
    
      
  }
  
 
  if (ICC){
    # plot those items with Discriminant > 0
    for (i in  as.vector(which(item_param[[1]][,"Dscrmn"] > 0))){
      
      openPDFEPS(paste(nameDS,"_Normal_(point ",i,")", sep=""))
      old.par <- par(mfrow=c(2, 2))
      
      
      plot_ICC(all_models, results, all_abilities,1,i, main="Item Characteristic Curve (LTM)")
      
      plot_ICC(all_models, results, all_abilities,2,i, main="Item Characteristic Curve (LTM RND)")
      
      plot_mICC(all_models, results, all_abilities,3,i, main="Item Characteristic Curve (MIRT)")
      
      plot_mICC(all_models, results, all_abilities,4,i, main="Item Characteristic Curve (MIRT RND)")
      
      
      par(old.par)
      dev.off()
    
    }
  
  }
  
  if(vs){
  print("Print Diff/Dscrmn")
  
    # Diff vs Discr
    openPDFEPS(paste(nameDS,"_Diff_vs_Discr", sep=""))
    
    do1A <- do1[which(do1$Dffclt>-100),]
    do1B <- do1A[which(do1A$Dffclt<500),]
    do1C <- do1B[which(do1B$Dscrmn<250),]
    g1<-ggplot(do1C, aes(Dffclt, Dscrmn)) + geom_point()
    
    do2A <- do2[which(do2$Dffclt>-100),]
    do2B <- do2A[which(do2A$Dffclt<500),]
    do2C <- do2B[which(do2B$Dscrmn<250),]
    g2<-ggplot(do2C, aes(Dffclt, Dscrmn)) + geom_point()
    
    do3A <- do3[which(do3$Dffclt>-100),]
    do3B <- do3A[which(do3A$Dffclt<500),]
    do3C <- do3B[which(do3B$Dscrmn<250),]
    g3<-ggplot(do3C, aes(Dffclt, Dscrmn)) + geom_point()
    
    do4A <- do4[which(do4$Dffclt>-100),]
    do4B <- do4A[which(do4A$Dffclt<500),]
    do4C <- do4B[which(do4B$Dscrmn<250),]
    g4<-ggplot(do4C, aes(Dffclt, Dscrmn)) + geom_point()
    
    
    grid.arrange(g1, g2,g3,g4, ncol= 2, nrow = 2)
    
    
    dev.off()
  
  }
  
  
  if(tablesAbil){
  print("Print Abilities")
  
  # Compute Average Probability of succes for the all the  Classifiers 
  
    for (model in 1:MODELS2COMPARE){
      
      
      abil = all_abilities[model,]
      avgProbs = vector()
      for(m in 1:length(methods)){
        C_method_probs <- vector()
        
        for (ind_inst in 1:nrow(results[[ind_dataset]])){
          
          ModelProf = abil[m]
          a = all_models[[model]]$item_param[ind_inst,3]
          b = all_models[[model]]$item_param[ind_inst,2]
          c = all_models[[model]]$item_param[ind_inst,1]
          theta = ModelProf
          y = c + (1-c)/(1+exp(-a*(theta-b)))
          C_method_probs <- c(C_method_probs,y)
        }
        
        MethodAvgProb <- sum(C_method_probs)/length(C_method_probs)
        avgProbs <- c(avgProbs, MethodAvgProb)
      }
      
      accuracy = acc[ind_dataset,]
      df <- data.frame(cbind(methods, abil, avgProbs, accuracy), row.names = 1:length(methods))
      df$abil <- as.numeric(as.character(abil))
      df$avgProbs <- as.numeric(as.character(avgProbs))
      df$accuracy <- as.numeric(as.character(accuracy))
      
      df <- df[order(df[,2], decreasing=FALSE),]
      
      write.table(df, file=paste(ds,nameDS,"_model_",model,"_tableAbilities.txt",sep=""))
      
      maxrow=35
      npages = ceiling(nrow(df)/maxrow)
      pdf(paste(ds,nameDS,"_model_",model,"_tableAbilities.pdf",sep=""), height = 11, width=8.5)
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
      
      labelsClass <- c("RandomClass_A", "RandomClass_B", "RandomClass_C","MajorityClass", "MinorityClass", "OptimalClass","DreadfulClass")
      df$label = df$method %in% labelsClass
      

      PDFEPS <- 1 # 0 None, 1 PDF, 2 EPS
      PDFheight= 15 # 7 by default, so 14 makes it double higher than wide, 5 makes letters bigger (in proportion) for just one 
      PDFwidth= 30 # 7 by default
      
      openPDFEPS(paste(ds,nameDS,"_model_",model,"_plotAll", sep=""))
      

      ab <- ggplot(df, aes(abil, reorder(methods,abil))) + geom_point(size=1)  + coord_cartesian(xlim = c(-4,4))
      ab <- ab + theme(axis.text=element_text(size=4),axis.title=element_text(size=4,face="bold"), plot.title = element_text(size=5))
      ab <- ab + geom_point(data = subset(df, label == T),colour="green", size=1) 
      ab <- ab + geom_text(aes(label = ifelse(label == T, as.character(methods),'')),hjust = 0, nudge_x = 0.055, size = 1.5)
      ab <- ab + labs(title = paste(nameDS," [", numClasses," classes", " (", printPropClasses, ")]", sep=""), x = "abilities", y = "Classifier", size =0.5) 
      
      
      #dev.off()
      
      
      # Plot probSucces
      
      #openPDFEPS(paste(ds,nameDS,"_plotProbSucces", sep=""))
      
      pS <- ggplot(df, aes(avgProbs, reorder(methods,avgProbs))) + geom_point(size=1) + coord_cartesian(xlim = c(0,1))
      pS <- pS + theme(axis.text=element_text(size=4),axis.title=element_text(size=4,face="bold"), plot.title = element_text(size=5))
      pS <- pS + geom_point(data = subset(df, label == T),colour="green", size=1) 
      pS <- pS + geom_text(aes(label = ifelse(label == T, as.character(methods),'')),hjust = 0, nudge_x = 0.055, size = 1.5)
      pS <- pS + labs(title = paste(nameDS," [", numClasses," classes", " (", printPropClasses, ")]", sep=""), x = "probSuccess", y = "Classifier",size=0.5) 
      #print(pS)
      
      #dev.off()
      
      
      # Plot Acc
      
      #openPDFEPS(paste(ds,nameDS,"_plotAccuracy", sep=""))
      
      pA <- ggplot(df, aes(accuracy, reorder(methods,accuracy))) + geom_point(size=1) + coord_cartesian(xlim = c(0,1))
      pA <- pA + theme(axis.text=element_text(size=4),axis.title=element_text(size=4,face="bold"), plot.title = element_text(size=5))
      pA <- pA + geom_point(data = subset(df, label == T),colour="green", size=1) 
      pA <- pA + geom_text(aes(label = ifelse(label == T, as.character(methods),'')),hjust = 0, nudge_x = 0.055, size = 1.5)
      pA <- pA + labs(title = paste(nameDS," [", numClasses," classes", " (", printPropClasses, ")]", sep=""), x = "Acc", y = "Classifier",size=0.5) 
      #print(pA)
      
      #dev.off()    grid.arrange(p1, p2,p3,p4,  ncol= 2, nrow = 2)
      
      grid.arrange(ab,pS,pA, ncol=3)      
      dev.off()

    }#for
  
  }#if
  
}#function











