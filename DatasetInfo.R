#################################################
MyUnique <- function(data, col) {
  #E: 1st column contains different values
  E = unique(data[col])
  for (e in 1:nrow(E)) {
    #E: 2nd column contains the frequency of each value
    E[e,2]=length(which(data[,col]==E[e,1]))
  }
  E
}


library("xtable")
#nombresFiles<-c("breast-w.arff","spect_test.arff","cylinder-bands.arff","liver-disorders.arff","haberman.arff","tic-tac-toe.arff","sonar.arff","credit-g.arff","vote.arff","diabetes.arff","breast-cancer.arff", "heart-statlog.arff","hepatitis.arff","kr-vs-kp.arff","ionosphere.arff","credit-a.arff")
#nomclasses<-c("benign","0","band","1","1","negative","Rock","good","democrat","tested_negative","no-recurrence-events","absent","DIE","won","b","+")

PATH_ACT = "./_Toy_/"
#PATH_ACT = "./data_multiclass/"

nombresFiles<-datasets<-list.files(PATH_ACT, pattern = "\\.csv$") #$ means that this is the end of the string

##### tabla Datasets
library("RWeka")
ld<-length(nombresFiles)
numInst<-c(1:ld)
numat<-c(1:ld)
numcol<-c(1:ld)
descr<-c(1:ld)
# avec<-c(1:ld)
# sdc<-c(1:ld)
# lmax<-c(1:ld)
# lmin<-c(1:ld)

for (id in 1:ld)
{
  w <- read.csv(paste(PATH_ACT,nombresFiles[id],sep=""))
  #nomclasse<-names(w)[length(names(w))]
  nat<-length(w[1,])
  numat[id]<-length(w[1,])
  numInst[id]<-length(w[,1])

  ETQ=MyUnique(w,ncol(w))
  #TOT=sum(ETQ[,ncol(ETQ)])
  suffix<-NULL
  for (i in 1:nrow(ETQ)) {
    #suffix<-paste(suffix,(ETQ[i,2]/TOT)*100.0,sep="")
    suffix<-paste(suffix,round(ETQ[i,2]/numInst[id]*100,1),"%",sep="")
    if (i<nrow(ETQ)) {
      suffix<-paste(suffix,",",sep="")
    }
  }
  descr[id]<-paste(nrow(ETQ)," Labels (",suffix,")",sep="")
  
#   avec[id]<-mean(w[,nat])
#   sdc[id]<-sd(w[,nat])
#   lmax[id]<-max(w[,nat])
#   lmin[id]<-min(w[,nat])
}
# ddata<-data.frame(dataset=datasets,NumInst=numcol,NumAtt=numat,Ave=avec,Sd=sdc,Max=lmax,Min=lmin)

ddata<-data.frame(dataset=datasets,NumInst=numInst,NumAtt=numat,ClassProportion=descr)

Tabdat <- xtable(ddata, caption= "Caption text", digits=3)

sink("datasets.txt")
print(Tabdat,size="\\tiny")
cat("\\end{document}")
sink()


#print(xtable(ddata), type = "html", include.rownames=FALSE, html.table.attributes=list("border='0' cellpadding='5' "))



pdf2png <- function(){
  
  
  library("animation")

  pdf<-list.files(PATH_ACT, pattern = "\\.pdf$") #$ means that this is the end of the string
  ld<-length(pdf)
  
  for (id in 1:ld)
  {
    
    file<- pdf[id]
    file <- paste(PATH_ACT,file,sep="")
    im.convert(file, output = paste(file,".png",sep=""), extra.opts="-density 150")
  }
}

