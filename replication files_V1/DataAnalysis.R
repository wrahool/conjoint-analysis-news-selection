setwd("C:/ASC/21/SelectiveProject/data/replication files")
rm(list=ls())

library(sandwich)

#get corrected standard error 
cluster_se_glm <- function(model, cluster){
  M <- length(unique(cluster))
  N <- length(cluster)
  K <- model$rank
  dfc <- (M/(M - 1)) * ((N - 1)/(N - K))
  uj  <- apply(estfun(model), 2, function(x) tapply(x, factor(cluster), sum));
  rcse.cov <- dfc * sandwich(model, meat. = crossprod(uj)/N)
  return(rcse.cov)
}

#get table of results  
get_RobustTable<-function(model,dat){
  out.s <- lm(model,data = dat)
  V.s <- cluster_se_glm(out.s, dat$id)
  #effects
  effects<-coef(out.s)[-1]
  #se
  ses<-sqrt(diag(V.s)[-1])
  #95ci
  pv<-pmin((1-pt(effects/ses,out.s$df.residual))*2,pt(effects/ses,out.s$df.residual)*2) 
  r<-data.frame(effects, ses ,effects - qnorm(.975)*ses, effects + qnorm(.975)*ses,pv)
  colnames(r)<-c("coefficient","Robust SE", "CI-L","CI-R","p-value")
  return (r)
}

#load the data
load(file = "data.rdata")

#results in MAIN TEXTS: 
#figure 1 - main effects
#forced response
get_RobustTable(ForcedResponse~highEnd+oppOutlet+ownOutlet+oppMsg+ownMsg,dat)
#rated response
get_RobustTable(RatedResponse~highEnd+oppOutlet+ownOutlet+oppMsg+ownMsg,dat)

#figure 2 - interaction with StrongPID
#forced response
get_RobustTable(ForcedResponse~highEnd*strongPID+oppOutlet*strongPID+ownOutlet*strongPID+
                  oppMsg*strongPID+ownMsg*strongPID,dat)
#rated response
get_RobustTable(RatedResponse~highEnd*strongPID+oppOutlet*strongPID+ownOutlet*strongPID+
                  oppMsg*strongPID+ownMsg*strongPID,dat)

#Robustness 1-deduplication
library(dplyr)
dat$EndDate<-as.character(dat$EndDate)
dat %>% group_by(PairNum,Order,Issue) %>% filter(EndDate==min(EndDate)) -> dedupDat

#Figure A3 - main effects
get_RobustTable(ForcedResponse~highEnd+oppOutlet+ownOutlet+oppMsg+ownMsg,dedupDat)
get_RobustTable(RatedResponse~highEnd+oppOutlet+ownOutlet+oppMsg+ownMsg,dedupDat)

#Figure A4 - interaction with StrongPID
get_RobustTable(ForcedResponse~highEnd*strongPID+oppOutlet*strongPID+ownOutlet*strongPID+
                  oppMsg*strongPID+ownMsg*strongPID,dedupDat)
get_RobustTable(RatedResponse~highEnd*strongPID+oppOutlet*strongPID+ownOutlet*strongPID+
                  oppMsg*strongPID+ownMsg*strongPID,dedupDat)

#Robustness 2: remove all invalid response (rated responses contrasted with forced responses)
#remove all invalid responses
dat %>% group_by(id,Issue) %>% filter(sum(ForcedResponse*RatedResponse)-
                                        sum((1-ForcedResponse)*RatedResponse)>=0) -> validDat
#Figure A5 - main effects
get_RobustTable(ForcedResponse~highEnd+oppOutlet+ownOutlet+oppMsg+ownMsg,validDat)
get_RobustTable(RatedResponse~highEnd+oppOutlet+ownOutlet+oppMsg+ownMsg,validDat)

#Figure A6 - interaction with strongPID
get_RobustTable(ForcedResponse~highEnd*strongPID+oppOutlet*strongPID+ownOutlet*strongPID+
                  oppMsg*strongPID+ownMsg*strongPID,validDat)
get_RobustTable(RatedResponse~highEnd*strongPID+oppOutlet*strongPID+ownOutlet*strongPID+
                  oppMsg*strongPID+ownMsg*strongPID,validDat)

#robust 3-Run models separately for democrats and republicans
library(dplyr)
Democrat<-dat[which(dat$PartyID<=3),]
Republican<-dat[which(dat$PartyID>=5),]

#main effects
#Figure A7 - main effects for Democrats
get_RobustTable(ForcedResponse~highEnd+oppOutlet+ownOutlet+oppMsg+ownMsg,Democrat)
get_RobustTable(RatedResponse~highEnd+oppOutlet+ownOutlet+oppMsg+ownMsg,Democrat)

#Figure A8 - main effects for Repbulicans
get_RobustTable(ForcedResponse~highEnd+oppOutlet+ownOutlet+oppMsg+ownMsg,Republican)
get_RobustTable(RatedResponse~highEnd+oppOutlet+ownOutlet+oppMsg+ownMsg,Republican)

#interaction with strong PID
#Figure A9 - interaction with strong PID for Democrats
get_RobustTable(ForcedResponse~highEnd*strongPID+oppOutlet*strongPID+ownOutlet*strongPID+
                  oppMsg*strongPID+ownMsg*strongPID,Democrat)
get_RobustTable(RatedResponse~highEnd*strongPID+oppOutlet*strongPID+ownOutlet*strongPID+
                  oppMsg*strongPID+ownMsg*strongPID,Democrat)

#Figure A10 - interaction with Strong PID for Republicans
get_RobustTable(ForcedResponse~highEnd*strongPID+oppOutlet*strongPID+ownOutlet*strongPID+
                  oppMsg*strongPID+ownMsg*strongPID,Republican)
get_RobustTable(RatedResponse~highEnd*strongPID+oppOutlet*strongPID+ownOutlet*strongPID+
                  oppMsg*strongPID+ownMsg*strongPID,Republican)


