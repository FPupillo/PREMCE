#------------------------------------------------------------------------------#
# simulating trials switch points and character and analyse results - Pearce Hall2
# created: "Thu Oct 28 16:50:46 2021"
#------------------------------------------------------------------------------#
rm(list=ls())

#soruce the functions
source("helper_functions/taskSim.R")
source("simulation_functions/simulate_PearceHall2.R")
source(("helper_functions/softmax.R"))
source(("helper_functions/chooseMultinom.R"))
source(("helper_functions/BICcompute.R"))
source("likelihood_functions/lik_PearceHall2.R")
source("fitting_functions/fit_PearceHall2.R")
source("helper_functions/getCat.R")
source("helper_functions/preSim.R")
source("helper_functions/getcorrCat.R")

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
sim<-simulate_PearceHall2(Data=Data,alpha=0.5, beta =7, 

           initialV = 0.25)

ggplot(sim, aes(x=trialN))+
  
  geom_line(aes(y=P1), size = 1.5, color = "blue")+
  geom_line(aes(y=P2),size = 1.5, color = "darkgreen")+
  geom_line(aes(y=P3),size = 1.5, color = "brown")+
  geom_line(aes(y=P4), size = 1.5,color = "orange")+
  # geom_line(aes(y=Delta), color = "red")+
  theme_bw()+
  
  geom_vline(xintercept = c (Ntrial,  Ntrial*2, Ntrial*3))+
  #facet_grid(butterfly~SubNum)+
  facet_wrap(character~., ncol = 3)

# now Learning rate
ggplot(sim, aes(x=trialN))+
  
  # geom_line(aes(y=P1), size = 1.5, color = "blue")+
  # geom_line(aes(y=P2),size = 1.5, color = "darkgreen")+
  # geom_line(aes(y=P3),size = 1.5, color = "brown")+
  # geom_line(aes(y=P4), size = 1.5,color = "orange")+
   geom_line(aes(y=alpha), color = "red")+
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
