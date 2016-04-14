#install.packages("mlbench", repos=c("http://rstudio.org/_packages", "http://cran.rstudio.com")) 
library(mlbench)
library(ggplot2)
library(dplyr)

set.seed(288)

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

# type == 1 => cassini 
# type == 2 => 2dnormals

genDS <- function(type, examples, noise, c=3){
  

  set.seed(288)
  examples <- examples
  noise <- noise
  numC <- c
  
  if (type == 1){
    x<-mlbench.cassini(examples)
  }else if(type == 2){
    x <- mlbench.2dnormals(examples,numC)
  }
  
  dataNoise <- data.frame(x=x$x[,1], y = x$x[,2], Class=x$classes)
  
  dataNoise <- dataNoise[sample(nrow(dataNoise)),]

  
  for (i in sample(nrow(dataNoise),noise)){
    
      newClass = sample(unique(dataNoise$Class),1)
      
      while (newClass == dataNoise$Class[i]){
        newClass = sample(unique(dataNoise$Class),1)
      }
      
      dataNoise$Class[i] <- newClass
    
  }
  
  
  
  
  if (type == 1){t <- "Cassini"}
  if (type == 2){t <- "2dNormals"}
  
  openPDFEPS(paste(t,"_",c,"c_", examples,"e_",noise,"n", sep=""))
  #mainPlot<- ggplot(dataNoise,aes(x,y, colour= factor(Class)))
  #print(mainPlot)
  plot(dataNoise$x,dataNoise$y,col=dataNoise$Class, pch=16)
  dev.off()
  

  
  write.csv(dataNoise,file=paste(t,"_",c,"c_", examples,"e_",noise,"n.csv",sep=""), row.names = FALSE)


}




genDS_incNoise <- function(type=1, examples=200, c=3, extra = 1){
  
  
  examples <- examples
  numC <- c
  
  if (type == 1){
    x<-mlbench.cassini(examples)
  }else if(type == 2){
    x <- mlbench.2dnormals(examples,numC)
  }
  
  dataNoise <- data.frame(x=x$x[,1], y = x$x[,2], Class=x$classes)
  
  dataNoise <- dataNoise[sample(nrow(dataNoise)),]
  print(paste("DS: ", nrow(dataNoise)))
  
  noiseSample = sample(nrow(dataNoise),examples/5)
  print(paste("Noise: ", length(noiseSample)))
  
  for (i in seq(0,100,10)){
    up2 = (examples/100 * i)/5
    sample <- noiseSample[0:up2]  
    print(paste("Samples: ",length(sample)))
    
    printsample = paste(sample, collapse = " ")
    write(paste("Noisy instances (",i/5,"%): ",printsample, sep=""),file = "Noise_information.txt", append =  TRUE)
    
    
    for (j in sample){
      
      newClass = sample(unique(dataNoise$Class),1)
      
      while (newClass == dataNoise$Class[j]){
        newClass = sample(unique(dataNoise$Class),1)
      }
      
      dataNoise$Class[j] <- newClass
      
    }
    
    if (type == 1){t <- "Cassini"}
    if (type == 2){t <- "2dNormals"}
    
    openPDFEPS(paste(extra,"__",t,"_",c,"c_", examples,"e_",i/5,"n", sep=""))
    
    p<- ggplot(dataNoise,aes(x,y, colour= factor(Class), label= row.names(dataNoise))) + geom_point(size = 5.5) + geom_text(check_overlap = F ,size=4, hjust = 0, nudge_x = 0.055)
    print(p)
    
    #plot(dataNoise$x,dataNoise$y,col=dataNoise$Class, pch=16)
    dev.off()
    
    write.csv(dataNoise,file=paste(extra,"__",t,"_",c,"c_", examples,"e_",i/5,"n.csv",sep=""), row.names = FALSE)
  }
    
}
  
run4avg <- function(){
  for(i in 1:10){
    genDS_incNoise(1,200,3,letters[i]) 
  }
  
}
  
  
  
  


runGenDS <- function(type = 1, instances = 200){
  
  for(i in 1:10){
    noise <- i * 5
    genDS(type,instances,noise)
  }
  
}




Noisify <- function(data) {
  
  if (is.vector(data)) {
    noise <- runif(length(data), -0.00001, 0.00001)
    noisified <- data + noise
  } else {
    length <- dim(data)[1] * dim(data)[2]
    noise <- matrix(runif(length, -0.0001, 0.00001), dim(data)[1])
    noisified <- data + noise
  }
  return(noisified)
}