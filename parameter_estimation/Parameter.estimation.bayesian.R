# compute likelihood
rm(list=ls())
library(ggplot2)
# source the functions
source("computational_model/softmax.R")
source("computational_model/lik_Bayesian.R")
source("computational_model/searchGlobal.R")
source("computational_model/fit_Bayesian.R")
source("computational_model/BICcompute.R")

DataAll<-read.csv("output_files/day2_accuracy.csv")
participants<-unique(DataAll$SubNum)

# set boundaries for the optimization algorithm
alphaBound<-c(0,1)
betaBound<-c(0,'i')

# set the initial Q
initialQ<-0.25

# set the number of the starting points for the optimization function
startPoints<-2

# initialize matrix to store the parameters
Parameters<-matrix(NA, nrow = length(participants),ncol = 4) 
colnames(Parameters)<-c("PartNum","beta", "BIC", "LL") #names of the columns

# loop by participant
for (j in 1: length(participants)){
  
  # print a message showing on which participant the script is working on
  print(paste("Working on participant", j))
  
  # subset the data for the current participant
  DataSub<-DataAll[DataAll$SubNum==participants[j],]
  
  # estimate alpha and beta, calculate the time
  start_time<-Sys.time() # take the starting time
  est<-fit_Bayesian(DataSub,betaBound, initialQ)
  end_time<-Sys.time() # take the ending time
  print(end_time-start_time) # calculate how long it took to estimate the parameters for that participant
  
  # extract alpha and beta from estimation results
  Beta<-est$alphabetaPAR
  
  # feed the RWM with the alpha and beta obtained to get Qs and PE
  #par<-lik_Bayesian(DataSub, Beta, 2, initialQ)
  
  # now, get PE
  #PE<-sum(par$Delta, na.rm=T)
  
  # add the data to a data long dataframe
  if (!exists("Datalong")){
    Datalong<-lik_Bayesian(DataSub, Beta, 3, initialQ)
  } else{
    Datalong<-rbind(Datalong,lik_Bayesian(DataSub, Beta, 3, initialQ)) 
  }
  
  # prepare results for saving
  Parameters[j, c(1:4)]<-c(DataSub$SubNum[1],
                            Beta, est$BIC, est$logLikel)
}

# beep for when the parameter estimation has finished
#install.packages("beepr")
library(beepr)
beep(8)

# save parameters
write.csv(Parameters, "output_files/estimated_parameters_bayesian.csv", row.names = F)

# save data from reinforcement learning
write.csv (Datalong, "output_files/RLdata_bayesian.csv", row.names = F)

Datalong$butterfly<-as.factor(Datalong$butterfly)
levels(Datalong$butterfly)<-c("white", "grey")

# plot the estimated probabilities for each flower
ggplot(Datalong[Datalong$SubNum==2,], aes(x=trialNum))+
  
  geom_line(aes(y=Q1), size = 1.5, color = "blue")+
  geom_line(aes(y=Q2),size = 1.5, color = "darkgreen")+
  geom_line(aes(y=Q3),size = 1.5, color = "brown")+
  geom_line(aes(y=Q4), size = 1.5,color = "orange")+
  # geom_line(aes(y=Delta), color = "red")+
  
  
  geom_vline(xintercept = c(16, 52,88, 124))+
  #facet_grid(butterfly~SubNum)+
  facet_wrap(butterfly~., ncol = 3)

# now prediction error
Datalong$Delta<-NA
Datalong$respFlower<-as.numeric(Datalong$respFlower)
for (j in 1:nrow(Datalong)){
 
  if (!is.na(Datalong$respFlower[j])){

  Datalong$Delta[j]<-Datalong[j, paste("Delta", Datalong$respFlower[j], sep="")]

  }
}

ggplot(Datalong[Datalong$SubNum==2,], aes(x=trialNum))+
  geom_line(aes(y=Delta),size = 1.5, color = "darkred")+
  
  
  
  geom_vline(xintercept = c(16, 52,88, 124))+
  #facet_grid(butterfly~SubNum)+
  facet_wrap(butterfly~., ncol = 3)


# now I want the uncertainty, calculated as 1/variance of the prob
# probs<-c("P1", "P2", "P3", "P4")
# for (n in 1:nrow(Datalong)){
# Datalong$uncertainty[n]<-1/(var(unlist(Datalong[n,probs]))+1)
# }



# # now calculated as the negative sum of probabilities
# for (n in 1:nrow(Datalong)){
#   Datalong$uncertainty2[n]<- -sum(  unlist(Datalong[n,probs]) *log(unlist(Datalong[n,probs])))
# }


# uncertainty  as the variance of the probabilities
ggplot(Datalong[Datalong$SubNum==2,], aes(x=trialNum))+
  
  geom_line(aes(y=uncertainty), size = 1.5, color = "blue")

ggplot(Datalong[Datalong$SubNum==2,], aes(x=trialNum))+
  geom_line(aes(y=uncertainty2),size = 1.5, color = "darkgreen")+
  geom_vline(xintercept = c(16, 52,88, 124))

# learning rate
ggplot(Datalong[Datalong$SubNum==2,], aes(x=trialNum))+
  geom_line(aes(y=lr),size = 1.5, color = "darkred")+
  geom_vline(xintercept = c(16, 52,88, 124))

# change pointpp
ggplot(Datalong[Datalong$SubNum==2,], aes(x=trialNum))+
  geom_line(aes(y=CPP),size = 1.5, color = "darkorange")+
  geom_vline(xintercept = c(16, 52,88, 124))

# plot the estimated probabilities for each flower
ggplot(Datalong[Datalong$SubNum==7,], aes(x=trialNum))+
  
  geom_line(aes(y=uncertainty), size = 1.5, color = "blue")

# now for all
# do it only for one category
# plot the estimated probabilities for each flower
ggplot(DataAll[DataAll$SubNum==2 & Datalong$butterfly=="white",], aes(x=trialNum))+
  
  geom_line(aes(y=Q1), size = 1.5, color = "blue")+
  geom_line(aes(y=Q2),size = 1.5, color = "darkgreen")+
  geom_line(aes(y=Q3),size = 1.5, color = "brown")+
  geom_line(aes(y=Q4), size = 1.5,color = "orange")+
  # geom_line(aes(y=Delta), color = "red")+
  
  
  geom_vline(xintercept = c(16, 52,88, 124))
  #facet_grid(butterfly~SubNum)+
  #facet_wrap(butterfly~., ncol = 3)

ggplot(Datalong[Datalong$SubNum==2 & Datalong$butterfly=="white",], aes(x=trialNum))+
  geom_line(aes(y=Delta),size = 1.5, color = "darkred")+
  
  
  
  geom_vline(xintercept = c(16, 52,88, 124))+
  #facet_grid(butterfly~SubNum)+
  facet_wrap(butterfly~., ncol = 3)


# now I want the uncertainty, calculated as 1/variance of the prob
# probs<-c("P1", "P2", "P3", "P4")
# for (n in 1:nrow(Datalong)){
# Datalong$uncertainty[n]<-1/(var(unlist(Datalong[n,probs]))+1)
# }



# # now calculated as the negative sum of probabilities
# for (n in 1:nrow(Datalong)){
#   Datalong$uncertainty2[n]<- -sum(  unlist(Datalong[n,probs]) *log(unlist(Datalong[n,probs])))
# }


# uncertainty  as the variance of the probabilities
ggplot(Datalong[Datalong$SubNum==2 & Datalong$butterfly=="white",], aes(x=trialNum))+
  
  geom_line(aes(y=uncertainty), size = 1.5, color = "blue")+
geom_vline(xintercept = c(16, 52,88, 124))

ggplot(Datalong[Datalong$SubNum==2 & Datalong$butterfly=="white",], aes(x=trialNum))+
  geom_line(aes(y=uncertainty2),size = 1.5, color = "darkgreen")+
  geom_vline(xintercept = c(16, 52,88, 124))

# learning rate
ggplot(Datalong[Datalong$SubNum==2 & Datalong$butterfly=="white",], aes(x=trialNum))+
  geom_line(aes(y=lr),size = 1.5, color = "darkred")+
  geom_vline(xintercept = c(16, 52,88, 124))

# change pointpp
ggplot(Datalong[Datalong$SubNum==3,], aes(x=trialNum))+
  geom_line(aes(y=CPP),size = 1.5, color = "darkorange")+
  geom_vline(xintercept = c(16, 52,88, 124))

# plot the estimated probabilities for each flower
ggplot(Datalong[Datalong$SubNum==7,], aes(x=trialNum))+
  
  geom_line(aes(y=uncertainty), size = 1.5, color = "blue")

