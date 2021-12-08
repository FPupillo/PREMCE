# compute likelihood
rm(list=ls())
library(ggplot2)
library(pracma) # for the function sigmoid
# source the functions
source("computational_model/softmax.R")
source("computational_model/lik_RescorlaWagner_LRchoice.R")
source("computational_model/fit_RescorlaWagner_LRchoice.R")
source("computational_model/BICcompute.R")
source("helper_functions/getobs.R")

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
Parameters<-matrix(NA, nrow = length(participants),ncol = 6) 
colnames(Parameters)<-c("PartNum", "beta","eta","k", "BIC", "LL") #names of the columns

# loop by participant
for (j in 1: length(participants)){
  
  # print a message showing on which participant the script is working on
  print(paste("Working on participant", j))
  
  # subset the data for the current participant
  DataSub<-DataAll[DataAll$SubNum==participants[j],]
  
  # estimate alpha and beta, calculate the time
  start_time<-Sys.time() # take the starting time
  est<-fit_RescorlaWagner_LRchoice(DataSub, betaBound, etaBound = c(-20, 20),
                                   kBound = c(-30, 30),
                                   initialQ = initialQ )
  end_time<-Sys.time() # take the ending time
  print(end_time-start_time) # calculate how long it took to estimate the parameters for that participant
  
  # extract alpha and beta from estimation results
  beta<-est$alpha[1]
  eta<-est$alpha[2]
  k<-est$alpha[3]
  # feed the RWM with the alpha and beta obtained to get Qs and PE
  par<-lik_RescorlaWagner_LRchoice(DataSub,  beta, eta, k, 2, initialQ)
  
  # now, get PE
  PE<-sum(par$Delta, na.rm=T)
  
  # add the data to a data long dataframe
  if (!exists("Datalong")){
    Datalong<-lik_RescorlaWagner_LRchoice(DataSub, beta, eta, k, 3, initialQ)
  } else{
    Datalong<-rbind(Datalong,lik_RescorlaWagner_LRchoice(DataSub, beta, eta, k, 3, initialQ)) 
  }
  
  # prepare results for saving
  Parameters[j, c(1:6)]<-c(DataSub$SubNum[1],
                            beta,eta, k, est$BIC, est$logLikel)
}

# beep for when the parameter estimation has finished
#install.packages("beepr")
library(beepr)
beep(8)

# save parameters
write.csv(Parameters, "output_files/estimated_parameters_LRchoice.csv", row.names = F)

# save data from reinforcement learning
write.csv (Datalong, "output_files/RLdata_LRchoice.csv", row.names = F)

Datalong$character<-as.factor(Datalong$character)
levels(Datalong$character)<-c("m2", "m5")

# plot the estimated probabilities for each flower
ggplot(Datalong[Datalong$SubNum==2,], aes(x=trialNum))+
  
  geom_line(aes(y=P1), size = 1.5, color = "blue")+
  geom_line(aes(y=P2),size = 1.5, color = "darkgreen")+
  geom_line(aes(y=P3),size = 1.5, color = "brown")+
  geom_line(aes(y=P4), size = 1.5,color = "orange")+
  # geom_line(aes(y=Delta), color = "red")+
  
  
  geom_vline(xintercept = c(18, 55,90, 126))+
  #facet_grid(butterfly~SubNum)+
  facet_wrap(character~., ncol = 3)

# now prediction error
Datalong$PEobs<-NA
  for (o in 1: nrow(Datalong)){
    if (Datalong$corrCat[o]==1){
      Datalong$PEobs[o]<-Datalong$Delta1[o]
    }else if(Datalong$corrCat[o]==2){
      Datalong$PEobs[o]<-Datalong$Delta2[o]
    }else if(Datalong$corrCat[o]==3){
      Datalong$PEobs[o]<-Datalong$Delta3[o]
    }else if(Datalong$corrCat[o]==4){
      Datalong$PEobs[o]<-Datalong$Delta4[o]
    }
  }

ggplot(Datalong[Datalong$SubNum==2,], aes(x=trialNum))+
   geom_line(aes(y=PEobs),size = 1.5, color = "darkred")+
  
  
  geom_vline(xintercept = c(16, 50,86, 122))+
  #facet_grid(butterfly~SubNum)+
  facet_wrap(character~., ncol = 3)


# by befAft
# distribution of PE by condition
Datalong$befAft<-as.factor(Datalong$befAft)
PEobsdistr<-ggplot(Datalong, aes(x= PEobs, fill=befAft))
print(
  PEobsdistr+geom_histogram()+ facet_grid( ~ befAft)+ggtitle("PE observational")
)

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
ggplot(Datalong[Datalong$SubNum==2,], aes(x=trialNum))+ ylim(c(0,1))+
  
  geom_line(aes(y=uncertainty), size = 1.5, color = "blue")

ggplot(Datalong[Datalong$SubNum==4,], aes(x=trialNum))+
  geom_line(aes(y=uncertainty2),size = 1.5, color = "darkred")+
  geom_vline(xintercept = c(16, 50,86, 122))
  
# plot the estimated probabilities for each flower
ggplot(Datalong[Datalong$SubNum==7,], aes(x=trialNum))+
  
  geom_line(aes(y=uncertainty), size = 1.5, color = "blue")

# now for all
ggplot(Datalong, aes(x=trialNum, y = uncertainty))+stat_summary(fun.y="mean",geom="line")
  
Uncdistr<-ggplot(Datalong, aes(y= uncertainty, x = befAft, fill=befAft))
print(
  Uncdistr+geom_boxplot()#+ facet_grid( ~ befAft)+ggtitle("Uncertainty observational")
)
