#------------------------------------------------------------------------------#
# simulating trials switch points and character and analyse results - HMM
# created: "Thu Oct 28 16:50:46 2021"
#------------------------------------------------------------------------------#
rm(list=ls())

#soruce the functions
source("simulate_data/taskSim2.R")
source("computational_model/simulation_functions/simulate_HMM.R")
source(("computational_model/softmax.R"))
source(("computational_model/chooseMultinom.R"))
source(("computational_model/BICcompute.R"))
source("helper_functions/getCat.R")
source("helper_functions/preSim.R")
source("helper_functions/getcorrCat.R")
source("simulate_data/taskSim2.R")

#------------------------------------------------------------------------------#
# parameters
#------------------------------------------------------------------------------#
Pcong<-0.75
Ntrial<-24
Ncharacter<-1
switchN<-3
categN<-4
#------------------------------------------------------------------------------#

Data<-taskSim(Pcong, Ntrial, Ncharacter, switchN, categN)

# prepare data
Data$cuedCharacter<-Data$character

for (n in 1:nrow(Data)){
  Data[n,1:4]<-paste0("stimuli/",Data[n,1:4], ".png")
}

# simulate data
sim<-simulate_HMM(Data=Data,c=0.5, beta =7, 
           gamma=0.1, 
           initialPs = 0.25)

ggplot(sim, aes(x=trialN))+
  
  geom_line(aes(y=PS_pre1), size = 1.5, color = "blue")+
  geom_line(aes(y=PS_pre2),size = 1.5, color = "darkgreen")+
  geom_line(aes(y=PS_pre3),size = 1.5, color = "brown")+
  geom_line(aes(y=PS_pre4), size = 1.5,color = "orange")+
  # geom_line(aes(y=Delta), color = "red")+
  theme_bw()+
  
  geom_vline(xintercept = c (Ntrial,  Ntrial*2, Ntrial*3))+
  #facet_grid(butterfly~SubNum)+
  facet_wrap(character~., ncol = 3)

# now susprise
ggplot(sim, aes(x=trialN))+
  
  # geom_line(aes(y=P1), size = 1.5, color = "blue")+
  # geom_line(aes(y=P2),size = 1.5, color = "darkgreen")+
  # geom_line(aes(y=P3),size = 1.5, color = "brown")+
  # geom_line(aes(y=P4), size = 1.5,color = "orange")+
   geom_line(aes(y=surprise), color = "red")+
  theme_bw()+
  
  geom_vline(xintercept = c (Ntrial,  Ntrial*2, Ntrial*3))+
  #facet_grid(butterfly~SubNum)+
  facet_wrap(character~., ncol = 3)

# now entropy
ggplot(sim, aes(x=trialN))+
  
  # geom_line(aes(y=P1), size = 1.5, color = "blue")+
  # geom_line(aes(y=P2),size = 1.5, color = "darkgreen")+
  # geom_line(aes(y=P3),size = 1.5, color = "brown")+
  # geom_line(aes(y=P4), size = 1.5,color = "orange")+
  geom_line(aes(y=entropy), color = "red")+
  theme_bw()+
  
  geom_vline(xintercept = c (Ntrial,  Ntrial*2, Ntrial*3))+
  #facet_grid(butterfly~SubNum)+
  facet_wrap(character~., ncol = 3)

ggplot(sim, aes(x=trialN))+
  
  # geom_line(aes(y=P1), size = 1.5, color = "blue")+
  # geom_line(aes(y=P2),size = 1.5, color = "darkgreen")+
  # geom_line(aes(y=P3),size = 1.5, color = "brown")+
  # geom_line(aes(y=P4), size = 1.5,color = "orange")+
  geom_line(aes(y=unlist(Delta)), color = "red")+
  theme_bw()+
  
  geom_vline(xintercept = c (Ntrial,  Ntrial*2, Ntrial*3))+
  #facet_grid(butterfly~SubNum)+
  facet_wrap(character~., ncol = 3)
