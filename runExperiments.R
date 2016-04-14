SOURCE_CODE ="D:/OneDrive/Rworks/IRT/Noise"

runExp <- function(){
  source("methodsResponses.R")
  source("IRT_model_fit_and_plot.R")
  run()
  
}


runComparison_Hangout <- function(){
  source("methodsResponses.R")
  source("3PLmodelsComparison.R")
  extract_data_comp_4hangout()
}


arff2csv <- function(){
  ds <- "_Toy_/"
  datasets <- list.files(ds, pattern = "*arff$")
  for(i in 1:length(datasets)){
    datos <- read.arff(paste(ds,datasets[i],sep=""))
    colnames(datos)[ncol(datos)]<-"Class"  #Changed by Adolfo: 04/04/2016
    
    nameDS <- datasets[i]
    nameDS <- strsplit(nameDS,"[.]")[[1]][1] #keep just the name
    
    write.csv(datos, file=paste(ds,nameDS,".csv",sep=""))
  }
  
}

cleanDS_rownames <- function(){
  ds <- "_Toy_/"
  datasets <- list.files(ds, pattern = "*csv$")
  for(i in 1:length(datasets)){
    datos <- read.csv(paste(ds,datasets[i],sep=""))
    nameDS <- datasets[i]
    nameDS <- strsplit(nameDS,"[.]")[[1]][1] #keep just the name
    write.csv(datos[,2:ncol(datos)], file=paste(ds,nameDS,".2csv",sep=""), row.names = FALSE)
  }
}