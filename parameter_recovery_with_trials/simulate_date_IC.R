#------------------------------------------------------------------------------#
# simulating trials switch points and character and analyse results - IC 
# created: ""Mon Dec  6 18:31:05 2021"
#------------------------------------------------------------------------------#
rm(list=ls())

library(ggplot2)
#soruce the functions
source("helper_functions/taskSim.R")
source("simulation_functions/simulate_RescorlaWagner_IC.R")
source(("helper_functions/softmax.R"))
source(("helper_functions/chooseMultinom.R"))
source(("helper_functions/BICcompute.R"))
source("fitting_functions/fit_HMM_gamma.R")
source("helper_functions/getCat.R")
source("helper_functions/preSim.R")
source("helper_functions/getcorrCat.R")
source(("helper_functions/update_RW.R"))


#------------------------------------------------------------------------------#
# parameters
#------------------------------------------------------------------------------#
Pcong<-0.75
Ntrial<-24
Ncharacter<-1
switchN<-3
categN<-2
#------------------------------------------------------------------------------#

Data<-taskSim(Pcong, Ntrial, Ncharacter, switchN, categN)

# prepare data
Data$cuedCharacter<-Data$character

for (n in 1:nrow(Data)){
  Data[n,1:4]<-paste0("stimuli/",Data[n,1:4], ".png")
}

# simulate data
sim<-simulate_RescorlaWagner_IC(Data=Data,
                        alpha =0.2 , beta = 3
                        ,  initialV = 0.25)



  


#ggsave(filename = "figures/estimatedCP_IC_weight_c=0.4_gamma=0.1.jpg") 


  #facet_grid(butterfly~SubNum)+
 # facet_wrap(character~., ncol = 3)

# estimated CP
ggplot(sim, aes(x=trialN))+
  
   geom_line(aes(y=P1), size = 1.5, color = "blue")+
   geom_line(aes(y=P2),size = 1.5, color = "darkgreen")+
   geom_line(aes(y=P3),size = 1.5, color = "brown")+
   geom_line(aes(y=P4), size = 1.5,color = "orange")+
  # geom_line(aes(y=surprise), color = "red")+
  theme_bw()+
  
  geom_vline(xintercept = c (Ntrial,  Ntrial*2, Ntrial*3))
  
  #facet_grid(butterfly~SubNum)+
  #facet_wrap(character~., ncol = 3)

 # ggsave(filename = "figures/surp_c=0.4_gamma=0.1.jpg") 


# calculate uncertainty as the variance of the probabilities
sim$uncertainty<-NA
for (n in 1:nrow(sim)){
sim$uncertainty[n]<-1/(var(unlist(sim[n,c("V1", "V2", "V3", "V4" )]))+0.1)
}
  
# now entropy
ggplot(sim, aes(x=trialN))+
  
  # geom_line(aes(y=P1), size = 1.5, color = "blue")+
  # geom_line(aes(y=P2),size = 1.5, color = "darkgreen")+
  # geom_line(aes(y=P3),size = 1.5, color = "brown")+
  # geom_line(aes(y=P4), size = 1.5,color = "orange")+
  geom_line(aes(y=uncertainty), color = "red")+
  geom_line(aes(y=Delta1), color = "blue")+
  
  theme_bw()+
  
  geom_vline(xintercept = c (Ntrial,  Ntrial*2, Ntrial*3))
  #facet_grid(butterfly~SubNum)+
  #facet_wrap(character~., ncol = 3)

ggsave(filename = "figures/entropy_HMM_c=0.4_gamma=0.1.jpg") 



