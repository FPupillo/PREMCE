#------------------------------------------------------------------------------#
# Data simulation
# created: Mon Oct  4 09:27:27 2021"
#
# Simulate data at different values and explore behaviour of different agents
#
#------------------------------------------------------------------------------#

rm(list=ls())

library(ggplot2)
library(dplyr)
library(gridExtra)

source("simulation_functions/simulate_RescorlaWagner_simple.R")
source("helper_functions/update_RW.R")
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

alpha<-seq(0, 1, length.out = 10)

beta<-seq(1, 25, length.out = 5)

# create dataframe for performance
simPerf<-as.data.frame(matrix(NA, ncol = 4, nrow = sims*length(beta)*length(alpha)))
names(simPerf)<-c("part", "perOptim")

# simulate data    
pb<-txtProgressBar(min=0, max=sims*length(beta)*length(alpha) )

for (simul in 1:sims){
  for (a in alpha){
  for (b in beta){
    
  sim<-simulate_RescorlaWagner_simple(Data=Data,
                                     alpha =a , beta = b,  initialV = 0.25)

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
 
 sim$alpha<-a
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

# save the simulation

# save the data
save(list = c("simPerf", "simData"), file = "output_files/simulationData_simple.Rdata")
#load("output_files/simulationData_feedb.Rdata")

# summarise it first
Datawide<- simData %>%
  group_by( trialN, alpha, beta) %>%
  summarise(mean = mean(perOptimalChoice), sd = sd(perOptimalChoice)) 

Datawide$alpha<-as.factor(Datawide$alpha)

Datawide$beta<-as.factor(Datawide$beta)

ggplot(Datawide, aes(x = trialN , y =mean, group = alpha, colour = alpha)) +
  geom_vline(xintercept = c(Ntrial, Ntrial*2,Ntrial*3))+
  
  #stat_summary(fun.y="mean",geom="line") +
  geom_line()+
  facet_wrap(.~beta)



#ggsave("figures/simulatedData_RWsimple.jpg")
  
# now create the before/after change point graphs
AccData<-simData  %>%
  group_by(part, beta,switch_cond)  %>%
  mutate(befAft=c(rep("afterCP", times=12), rep("beforeCP", times=12)))


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
simPerfAgg<-simPerf[,c(1,2,5,6)] %>%
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
p2<-ggplot(data=simPerfAgg, aes(x=beta,y=perOptim,  color=alpha, group = alpha)) + 
  theme_bw() + 
  geom_errorbar(aes(ymin = lowerSE, ymax = upperSE), width = 1) + 
  geom_line() + scale_color_continuous(name = "learning rate") 

p3<-ggplot(data=simPerfAgg, aes(x=alpha,y=perOptim, color=beta, group=beta)) + 
  theme_bw() + 
  geom_errorbar(aes(ymin = lowerSE, ymax = upperSE)) + 
  geom_line() + scale_color_continuous(name = "beta") 

# arrange the two
plotOpt<-grid.arrange(p1, p2, p3,ncol=2)

ggsave("figures/optimalperf_RW_simple.jpg", plot = plotOpt)

# now for only one simulation
sim<-simulate_RescorlaWagner_simple(Data=Data,
                        alpha =0.75 , beta = 10,  initialV = 0.25)
ggplot(sim, aes(x=trialN))+
  
  geom_line(aes(y=P1), size = 1.5, color = "blue")+
  geom_line(aes(y=P2),size = 1.5, color = "darkgreen")+
  geom_line(aes(y=P3),size = 1.5, color = "brown")+
  geom_line(aes(y=P4), size = 1.5,color = "orange")+
  # geom_line(aes(y=Delta), color = "red")+
  theme_bw()+
  
  geom_vline(xintercept = c (Ntrial,  Ntrial*2, Ntrial*3))+
  ylab("Choice Probabilities")
  #facet_grid(butterfly~SubNum)+
  #facet_wrap(character~., ncol = 3)

ggsave("figures/estimatedCP_RW_simple_alpha=0.75_beta=10.jpg")
