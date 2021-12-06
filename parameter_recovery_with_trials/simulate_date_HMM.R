#------------------------------------------------------------------------------#
# simulating trials switch points and character and analyse results - bayesian 2
# created: "Thu Oct 28 16:50:46 2021"
#------------------------------------------------------------------------------#
rm(list=ls())

library(ggplot2)
#soruce the functions
source("helper_functions/taskSim.R")
source("simulation_functions/simulate_Bayesian2.R")
source(("helper_functions/softmax.R"))
source(("helper_functions/chooseMultinom.R"))
source(("helper_functions/BICcompute.R"))
source("likelihood_functions/lik_HMM.R")
source("fitting_functions/fit_HMM_gamma.R")
source("helper_functions/getCat.R")
source("helper_functions/preSim.R")
source("helper_functions/getcorrCat.R")

#------------------------------------------------------------------------------#
# parameters
#------------------------------------------------------------------------------#
Pcong<-0.75
Ntrial<-40
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
sim<-simulate_Bayesian2(Data=Data,Beta=0.1, 
           hazardRate = 3/20,  
           initialQ = 0.25)

ggplot(sim, aes(x=trialN))+
  
  geom_line(aes(y=PS_pre1), size = 1.5, color = "blue")+
  geom_line(aes(y=PS_pre2),size = 1.5, color = "darkgreen")+
  geom_line(aes(y=PS_pre3),size = 1.5, color = "brown")+
  geom_line(aes(y=PS_pre4), size = 1.5,color = "orange")+
  # geom_line(aes(y=Delta), color = "red")+
  theme_bw()+
  
  geom_vline(xintercept = c (Ntrial,  Ntrial*2, Ntrial*3))+
  labs(y = "Estimated choice probabilities")
  

ggplot(sim, aes(x=trialN))+
  
  geom_line(aes(y=Q1), size = 1.5, color = "blue")+
  geom_line(aes(y=Q2),size = 1.5, color = "darkgreen")+
  geom_line(aes(y=Q3),size = 1.5, color = "brown")+
  geom_line(aes(y=Q4), size = 1.5,color = "orange")+
  # geom_line(aes(y=Delta), color = "red")+
  theme_bw()+
  
  geom_vline(xintercept = c (Ntrial,  Ntrial*2, Ntrial*3))+
  labs(y = "Estimated choice probabilities")

ggsave(filename = "figures/estimatedCP_HMM_c=0.4_gamma=0.1.jpg") 


  #facet_grid(butterfly~SubNum)+
 # facet_wrap(character~., ncol = 3)

# now susprise
ggplot(sim, aes(x=trialN))+
  
  # geom_line(aes(y=P1), size = 1.5, color = "blue")+
  # geom_line(aes(y=P2),size = 1.5, color = "darkgreen")+
  # geom_line(aes(y=P3),size = 1.5, color = "brown")+
  # geom_line(aes(y=P4), size = 1.5,color = "orange")+
   geom_line(aes(y=surprise), color = "red")+
  theme_bw()+
  
  geom_vline(xintercept = c (Ntrial,  Ntrial*2, Ntrial*3))
  
  #facet_grid(butterfly~SubNum)+
  #facet_wrap(character~., ncol = 3)

  ggsave(filename = "figures/surprise_HMM_c=0.4_gamma=0.1.jpg") 

  # now uncertainty
  ggplot(sim, aes(x=trialN))+
    
    # geom_line(aes(y=P1), size = 1.5, color = "blue")+
    # geom_line(aes(y=P2),size = 1.5, color = "darkgreen")+
    # geom_line(aes(y=P3),size = 1.5, color = "brown")+
    # geom_line(aes(y=P4), size = 1.5,color = "orange")+
    geom_line(aes(y=uncertainty), color = "red")+
    theme_bw()+
    
    geom_vline(xintercept = c (Ntrial,  Ntrial*2, Ntrial*3))
  
  #facet_grid(butterfly~SubNum)+
  #facet_wrap(character~., ncol = 3)
  

  #  # now le
  ggplot(sim, aes(x=trialN))+
    
    # geom_line(aes(y=P1), size = 1.5, color = "blue")+
    # geom_line(aes(y=P2),size = 1.5, color = "darkgreen")+
    # geom_line(aes(y=P3),size = 1.5, color = "brown")+
    # geom_line(aes(y=P4), size = 1.5,color = "orange")+
    geom_line(aes(y=lr), color = "red")+
    theme_bw()+
    
    geom_vline(xintercept = c (Ntrial,  Ntrial*2, Ntrial*3))
  
  #facet_grid(butterfly~SubNum)+
  #facet_wrap(character~., ncol = 3)
  
  ggsave(filename = "figures/surprise_HMM_c=0.4_gamma=0.1.jpg") 
   
  
# now entropy
ggplot(sim, aes(x=trialN))+
  
  # geom_line(aes(y=P1), size = 1.5, color = "blue")+
  # geom_line(aes(y=P2),size = 1.5, color = "darkgreen")+
  # geom_line(aes(y=P3),size = 1.5, color = "brown")+
  # geom_line(aes(y=P4), size = 1.5,color = "orange")+
  geom_line(aes(y=entropy), color = "red")+
  theme_bw()+
  
  geom_vline(xintercept = c (Ntrial,  Ntrial*2, Ntrial*3))
  #facet_grid(butterfly~SubNum)+
  #facet_wrap(character~., ncol = 3)

ggsave(filename = "figures/entropy_HMM_c=0.4_gamma=0.1.jpg") 


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

ggsave(filename = "figures/entropy_HMM_c=0.4_gamma=0.1.jpg") 
