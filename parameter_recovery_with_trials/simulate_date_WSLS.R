#------------------------------------------------------------------------------#
# simulating trials switch points and character and analyse results - WSLS 
# created: "Thu Oct 28 16:50:46 2021"
#------------------------------------------------------------------------------#
rm(list=ls())

#soruce the functions
source("helper_functions/taskSim.R")
source("simulation_functions/simulate_RescorlaWagner_feedb.R")
source(("helper_functions/softmax.R"))
source(("helper_functions/chooseMultinom.R"))
source(("helper_functions/BICcompute.R"))
source("helper_functions/getCat.R")
source("helper_functions/preSim.R")
source("helper_functions/getcorrCat.R")

# load the libraries
library(ggplot2)

#------------------------------------------------------------------------------#
# parameters
#------------------------------------------------------------------------------#
Pcong<-0.75
Ntrial<-24
Ncharacter<-1
switchN<-3
categN<-4
#------------------------------------------------------------------------------#

# simulate data
Data<-taskSim(Pcong, Ntrial, Ncharacter, switchN, categN)

# prepare data
Data$cuedCharacter<-Data$character

# process the data
for (n in 1:nrow(Data)){
  Data[n,1:4]<-paste0("stimuli/",Data[n,1:4], ".png")
}

# simulate data
sim<-simulate_RescorlaWagner_feedb(Data=Data,alpha =0.60, beta =7, 
           initialV = 0.25)

# plot the estimated probabilities
ggplot(sim, aes(x=trialN))+
  geom_line(aes(y=P1), size = 1.5, color = "blue")+
  geom_line(aes(y=P2),size = 1.5, color = "darkgreen")+
  geom_line(aes(y=P3),size = 1.5, color = "brown")+
  geom_line(aes(y=P4), size = 1.5,color = "orange")+
  # geom_line(aes(y=Delta), color = "red")+
  theme_bw()+
  
  geom_vline(xintercept = c (Ntrial,  Ntrial*2, Ntrial*3))+
  facet_wrap(character~., ncol = 3)+
  geom_vline(xintercept = c (Ntrial,  Ntrial*2, Ntrial*3))+
  scale_colour_manual(values = c(P1 = 'blue',
                                 P2 = 'darkgreen', 
                                 P3 = "brown",
                                 P4 = "orange"))+
  labs(y = "Estimated choice probabilities")+
  #facet_grid(butterfly~SubNum)+
  facet_wrap(character~., ncol = 3)

ggsave(filename = "figures/estimatedCP_RW_alpha=0.60_beta=7.jpg") 


# now the prediction error
# plot the estimated probabilities
ggplot(sim, aes(x=trialN))+
  # geom_line(aes(y=P1), size = 1.5, color = "blue")+
  # geom_line(aes(y=P2),size = 1.5, color = "darkgreen")+
  # geom_line(aes(y=P3),size = 1.5, color = "brown")+
  # geom_line(aes(y=P4), size = 1.5,color = "orange")+
  geom_line(aes(y=as.numeric(Delta)), color = "red")+
  theme_bw()+
  
  geom_vline(xintercept = c (Ntrial,  Ntrial*2, Ntrial*3))+
  facet_wrap(character~., ncol = 3)+
  geom_vline(xintercept = c (Ntrial,  Ntrial*2, Ntrial*3))+
  labs(y = "Prediction Error")+
  #facet_grid(butterfly~SubNum)+
  facet_wrap(character~., ncol = 3)

