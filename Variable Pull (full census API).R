library(acs)
library(censusapi)
library(tidyverse)

key <-  "YOUR KEY"


universe <- censusapi::listCensusApis()

#Variable extraction does not work for pums data
pums <- grepl("pums",universe$name)
uni2 <- universe[pums==FALSE,]


vars <- c()
for (i in 1:length(uni2$title)){
  t <- makeVarlist(name=uni2$name[i],vintage = uni2$vintage[i],find="*")
  vars <- c(vars,list(t))
}
#did not run for cps data 


vars3 <- c()
for (i in 360:800){
  t <- makeVarlist(name=uni2$name[i],vintage = uni2$vintage[i],find="*")
  vars3 <- c(vars3,list(t))
}

#no data for 411
vars4 <- c()unli
for (i in 412:800){
  t <- makeVarlist(name=uni2$name[i],vintage = uni2$vintage[i],find="*")
  vars4 <- c(vars4,list(t))
}

#timeseries = no variables (667-723)

vars5 <- c()
for (i in 724:748){
  t <- makeVarlist(name=uni2$name[i],vintage = uni2$vintage[i],find="*")
  vars5 <- c(vars5,list(t))
}


uni3 <- uni2[c(1:212,360:410,412:666,724:748),]
uni3$vars <- c(vars,vars3,vars4,vars5)

#uni2$variables <- vars

#max of 50 variables can be pulled per api call 
# pull each name/vintage and combine

uni4 <- unnest(uni3,cols=c('vars'))
