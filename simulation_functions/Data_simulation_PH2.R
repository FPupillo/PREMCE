#------------------------------------------------------------------------------#
# Data simulation PH2, based on equation 8 of Pearce & Hall
# created: Mon Oct  4 09:27:27 2021"
#
# Simulate data at different values and explore behaviour of different agents
#
#------------------------------------------------------------------------------#

rm(list=ls())

library(ggplot2)
library(dplyr)
library(gridExtra)

source("simulation_functions/simulate_PearceHall2.R")
source(("helper_functions/softmax.R"))
source(("helper_functions/chooseMultinom.R"))
source(("helper_functions/BICcompute.R"))
source(("helper_functions/cumAcc.R"))
source("helper_functions/getOpt.R")
source("helper_functions/isOpt.R")
source("helper_functions/perOpt.R")
source("helper_functions/getCatresp.R")


source("helper_functions/taskSim.R")

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

for (n in 1:nrow(Data)){
  Data[n,1:4]<-paste0("stimuli/",Data[n,1:4], ".png")
}

#different alpha values and beta
sims<-30

# virtual part
part<-1

# store them in a data frame
simData<-vector()

as<-seq(0.1, 1, length.out = 5)

beta<-seq(1, 20, length.out = 5)

#etas<-seq(0, 1, length.out = 5)
# create dataframe for performance
simPerf<-as.data.frame(matrix(NA, ncol = 4, nrow = sims*5*5))
names(simPerf)<-c("part","alpha", "beta", "perOptim")

# simulate data    
pb<-txtProgressBar(min=0, max=sims*length(beta)*length(alpha) )

for (simul in 1:sims){
  for (a in as){
  for (b in beta){{
        
      
  sim<-simulate_PearceHall2(Data=Data, alpha_0 =a , beta = b, 
                   initialV = 0.25)

  # calculate percentage optimal choice
  # rename variables
 sim$task_resp.keys<-sim$response
 sim$character<-sim$cuedCharacter
 
 # get optimal choice
 sim<-getOpt(sim)
 
 # trasform it
 sim$respCat<-paste0("stimuli/",sim$respCat, ".png")
 #sim<-getCatresp(sim)
 
 sim<-isOpt(sim)
 
 sim<-perOpt(sim)
 
 sim$a<-a
 sim$beta<-b
 
 sim$simN<-sims[simul]
 
 sim$part<-part
 
 simData<-rbind(simData, sim)
 
 # now the dataframe for calculating optimal choice
 simPerf$part[part]<-part
 simPerf$alpha[part]<-a
 
 simPerf$beta[part]<-b

 simPerf$perOptim[part]<-sim$perOptimalChoice[nrow(sim)]
 
 part<-part+1
 
 setTxtProgressBar(pb, part-1) 
 
 
}
}
  }
  }


# save the simulation

# save the data
#save(list = c("simPerf", "simData"), file = "output_files/simulationData_PH2.Rdata")
load("output_files/simulationData_PH.Rdata")

# summarise it first
Datawide<- simData %>%
  group_by( trialN, a, beta) %>%
  summarise(mean = mean(perOptimalChoice), sd = sd(perOptimalChoice)) 

Datawide$a<-as.factor(Datawide$a)

Datawide$beta<-as.factor(Datawide$beta)


ggplot(Datawide, aes(x = trialN , y =mean, group = a, colour = a)) +
  geom_vline(xintercept = c(Ntrial, Ntrial*2,Ntrial*3))+
  
  #stat_summary(fun.y="mean",geom="line") +
  geom_line()+
  facet_wrap(.~beta)

ggplot(Datawide, aes(x = trialN , y =mean, group = a, colour = a)) +
  geom_vline(xintercept = c(Ntrial, Ntrial*2,Ntrial*3))+
  
  #stat_summary(fun.y="mean",geom="line") +
  geom_line()+
  facet_wrap(.~beta)


#ggsave("output_files/simulatedData_RWfeedb.jpg")
  
# now create the before/after change point graphs
AccData<-simData  %>%
  group_by(part,switch_cond)  %>%
  mutate(befAft=c(rep(12), rep("beforeCP", times=12)))


ggplot(AccData, aes(befAft, isoptimal))+
  geom_bar(aes(befAft, isoptimal, fill = befAft),
           position="dodge",stat="summary", fun.y="mean", SE=T)+
  stat_summary(fun.data = "mean_cl_boot", size = 0.8, geom="errorbar", width=0.2 )+
  #geom_jitter( size=1,width=0.1)+
  theme(axis.text.x = element_blank())+ # we are showing the different levels through the colors, so we can avoid naming the bars
  theme(axis.ticks.x = element_blank())+

  theme_bw()+
  #geom_line(data = groupData, aes(befAft, accuracy,group = SubNum), size=1, alpha=0.2, stat="summary")+

  #facet_grid(.~switch_cond)+
  theme(strip.text.x = element_text(size = 13))+
  ylab("Accuracy")+
  scale_fill_manual("legend", values = c("afterCP" = "darkgoldenrod1", "beforeCP" = "darkred"))+
  #theme(axis.text.x = element_text( size = 20))+
  theme(axis.title=element_text(size=14,face="bold"))
  #theme(legend.position = "none")+
  
#   
# aggregate simperf at simulation level
simPerfAgg<-simPerf %>%
  group_by(alpha,beta) %>%
  summarise( dev = sd(perOptim),
             perOptim = mean(perOptim) ,
             n = n(),
            se = dev/sqrt(n),
            upperSE = perOptim+se,
            lowerSE = perOptim-se)

#simPerfAgg$alpha<-as.factor(simPerfAgg$alpha)

#simPerfAgg$beta<-as.factor(simPerfAgg$beta)

# plot matrix
p1<-ggplot(data = simPerfAgg, aes(x = alpha, y = beta, fill = perOptim))+
  geom_tile()+
  scale_fill_gradient(low="black", high="white",name ="p(correct \nchoice)") + theme_bw()

# plot line graph
p2<-ggplot(data=simPerfAgg, aes(x=beta,y=perOptim, color = alpha, group=alpha)) + 
  theme_bw() + 
  geom_errorbar(aes(ymin = lowerSE, ymax = upperSE), width = 1) + 
  geom_line() + scale_color_continuous(name = "learning rate") 

p3<-ggplot(data=simPerfAgg, aes(x=alpha,y=perOptim, color=beta, group=beta)) + 
  theme_bw() + 
  geom_errorbar(aes(ymin = lowerSE, ymax = upperSE)) + 
  geom_line() + scale_color_continuous(name = "beta") 


# arrange the two
grid.arrange(p1, p2, p3,ncol=2)

ggsave("output_files/optimalperf.jpg")
  
# now for only one simulation
sim<-simulate_PearceHall2(Data=Data,
                                    alpha =0.75 , beta = 5,  initialV = 0.25)
ggplot(sim, aes(x=trialN))+
  
  geom_line(aes(y=P1), size = 1.5, color = "blue")+
  geom_line(aes(y=P2),size = 1.5, color = "darkgreen")+
  geom_line(aes(y=P3),size = 1.5, color = "brown")+
  geom_line(aes(y=P4), size = 1.5,color = "orange")+
  #geom_line(aes(y=Delta), color = "red")+
  theme_bw()+
  
  geom_vline(xintercept = c (Ntrial,  Ntrial*2, Ntrial*3))+
  ylab("Choice Probabilities")




ggsave("figures/estimatedCP_PH2_alpha=0.75_beta=5.jpg")

ggplot(sim, aes(x=trialN))+
  
  geom_line(aes(y=alpha), color = "red")+
  theme_bw()+
  
  geom_vline(xintercept = c (Ntrial,  Ntrial*2, Ntrial*3))+
  ylab("learning rate")

ggsave("figures/learningrate_PH2_alpha=0.75_beta=5.jpg")

