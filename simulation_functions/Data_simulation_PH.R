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

source("computational_model/simulation_functions/simulate_PearceHall.R")
source(("computational_model/softmax.R"))
source(("computational_model/chooseMultinom.R"))
source(("computational_model/BICcompute.R"))
source(("helper_functions/cumAcc.R"))
#source("computational_model/likelihood_functions/lik_WSLS.R")
#source("computational_model/fitting_functions/fit_WSLS.R")
source("helper_functions/getOpt.R")
source("helper_functions/isOpt.R")
source("helper_functions/perOpt.R")
source("helper_functions/getCatresp.R")


# the different models are
# RW - RW agent
# Bayesian_dorf - an agent that takes into account the beliefs about
#                 CPP and incorporates them in a changing LR
# Bayesian_Nassar - an agent that takes into account the beliefs about
#                 CPP in a reduced Bayesian model
# DualLR - a model that adapts the learning rate depending on the belief on the
#         CPP

# agents<-c("RW", "Bayesian_dorf", "Bayesian_Nassar", "DualLR")
#
# # first, retrieve the structure of the trials
listPath<-"lists/"
#
# # now the day1
day1list<-read.csv(paste0(listPath, "tasklistA1.csv"))
#
# # now day2
day2list<-vector()
for (l in c("Aplus", "B", "C", "D")){

  currlist<-read.csv(paste0(listPath, "tasklist", l, "1", ".csv"))

  day2list<-rbind(day2list, currlist)

}

#day2list<-read.csv("simulate_data/day2list2.csv")


# select the agent
#currAgent<-agents[1]

# simulate behaviour at different alpha values and beta
sims<-1

# virtual part
part<-1

# store them in a data frame
simData<-vector()

as<-seq(0.1, 1, length.out = 5)

beta<-seq(1, 20, length.out = 5)

ks<-seq(0, 1, length.out = 5)

etas<-seq(0, 1, length.out = 5)
# create dataframe for performance
simPerf<-as.data.frame(matrix(NA, ncol = 6, nrow = sims*5^4))
names(simPerf)<-c("part","alpha", "beta", "k", "eta", "perOptim")

# simulate data    
pb<-txtProgressBar(min=0, max=sims*length(beta)*length(alpha) )

for (simul in 1:sims){
  for (a in as){
  for (b in beta){
    for (k in ks){
      for (eta in etas){
        
      
  sim<-simulate_PearceHall(Data=day2list, alpha_0 =a , beta = b, k=k, 
                           eta = eta, initialQ = 0.25)

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
 sim$k<-k
 
 sim$eta<-eta
 
 sim$simN<-sims[simul]
 
 sim$part<-part
 
 simData<-rbind(simData, sim)
 
 # now the dataframe for calculating optimal choice
 simPerf$part[part]<-part
 simPerf$alpha[part]<-a
 
 simPerf$beta[part]<-b
 simPerf$k[part]<-k
 simPerf$eta[part]<-eta
 
 simPerf$perOptim[part]<-sim$perOptimalChoice[nrow(sim)]
 
 part<-part+1
 
 setTxtProgressBar(pb, part-1) 
 
 
}
}
  }
  }
}

# save the simulation

# save the data
#save(list = c("simPerf", "simData"), file = "output_files/simulationData_PH.Rdata")
load("output_files/simulationData_PH.Rdata")

# summarise it first
Datawide<- simData %>%
  group_by( trialN, a, beta, k, eta) %>%
  summarise(mean = mean(perOptimalChoice), sd = sd(perOptimalChoice)) 

Datawide$a<-as.factor(Datawide$a)

Datawide$beta<-as.factor(Datawide$beta)

ggplot(Datawide[Datawide$k==0.5,], aes(x = trialN , y =mean, group = a, colour = a)) +
  geom_vline(xintercept = c(48, 96,144))+
  
  #stat_summary(fun.y="mean",geom="line") +
  geom_line()+
  facet_wrap(eta~beta)

ggplot(Datawide[Datawide$a==0.55 & Datawide$beta==10.5,], aes(x = trialN , y =mean, group = a, colour = a)) +
  geom_vline(xintercept = c(48, 96,144))+
  
  #stat_summary(fun.y="mean",geom="line") +
  geom_line()+
  facet_wrap(eta~k)


#ggsave("output_files/simulatedData_RWfeedb.jpg")
  
# now create the before/after change point graphs
AccData<-simData  %>%
  group_by(part, beta,switch_cond)  %>%
  mutate(befAft=c(rep("afterCP", times=24), rep("beforeCP", times=24)))


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
  group_by(alpha,beta, k, eta) %>%
  summarise( dev = sd(perOptim),
             perOptim = mean(perOptim) ,
             n = n(),
            se = dev/sqrt(n),
            upperSE = perOptim+se,
            lowerSE = perOptim-se)

simPerfAgg$alpha<-as.factor(simPerfAgg$alpha)

simPerfAgg$beta<-as.factor(simPerfAgg$beta)

# plot matrix
p1<-ggplot(data = simPerfAgg[simPerfAgg$beta==3,], aes(x = alpha, y = k, fill = perOptim))+
  geom_tile()+
  scale_fill_gradient(low="black", high="white",name ="p(correct \nchoice)") + theme_bw()

# plot line graph
p2<-ggplot(data=simPerfAgg, aes(x=beta,y=perOptim, color = alpha, group=alpha)) + 
  theme_bw() + 
# geom_errorbar(aes(ymin = lowerSE, ymax = upperSE), width = 1) + 
  geom_line() + scale_color_continuous(name = "learning rate") 

p3<-ggplot(data=simPerfAgg, aes(x=alpha,y=perOptim, color=beta, group=beta)) + 
  theme_bw() + 
  #geom_errorbar(aes(ymin = lowerSE, ymax = upperSE)) + 
  geom_line() + scale_color_continuous(name = "beta") 

# arrange the two
grid.arrange(p1, p2, p3,ncol=2)

ggsave("output_files/optimalperf.jpg")
  

# select only beta less than 10

AccDataSub<-AccData %>% filter(beta <  10.000000 )

# plot as a function of before or after
AccDataSub$alpha<-as.factor(AccDataSub$alpha)
alphalev<-levels(AccDataSub$alpha)

ggplot(AccDataSub[AccDataSub$alpha==alphalev[4]  | 
                    AccDataSub$alpha==alphalev[8]| 
                    AccDataSub$alpha==  alphalev[12] | 
                    AccDataSub$alpha== alphalev[16] | 
                    AccDataSub$alpha==alphalev[20],], aes(befAft, isoptimal, color = befAft))+
  # geom_bar(aes(befAft, isoptimal, fill = befAft),
  #         position="dodge",stat="summary", fun.y="mean", SE=T)+
  stat_summary(fun.data = "mean_cl_boot", size = 0.8, geom="errorbar", width=0.2 )+
  #geom_jitter( size=1,width=0.1)+
  theme(axis.text.x = element_blank())+ # we are showing the different levels through the colors, so we can avoid naming the bars
  theme(axis.ticks.x = element_blank())+
  
  #theme_bw()+
  #geom_line(data = groupData, aes(befAft, accuracy,group = SubNum), size=1, alpha=0.2, stat="summary")+
  
  facet_grid(alpha~beta)+
  theme(strip.text.x = element_text(size = 13))+ 
  ylab("Accuracy")+
  scale_fill_manual("legend", values = c("afterCP" = "darkgoldenrod1", "beforeCP" = "darkred"))+
  theme(axis.text.x = element_text( size = 20))+
  theme(axis.title=element_text(size=14,face="bold"))+
  theme(legend.position = "none")+
  ylim(0,1)

# map participants' estimate in the plot
# retrieve data
estimates<-read.csv("output_files/estimated_parameters_obsALL.csv")

# plot the graph with adding the data
ggplot(data = simPerfAgg, aes(x = alpha, y = beta, fill = perOptim))+
  geom_tile()+
  scale_fill_gradient(low="black", high="white",name ="p(correct \nchoice)") +
  geom_point(data = estimates, aes(x=alpha, y = beta, fill=NULL))+
  
  theme_bw()

  ggplot(data = estimates, aes(x=alpha, y = beta))+
  geom_point()
#
  
  ggplot(sim, aes(x=trialN))+
    
    geom_line(aes(y=P1), size = 1.5, color = "blue")+
    geom_line(aes(y=P2),size = 1.5, color = "darkgreen")+
    geom_line(aes(y=P3),size = 1.5, color = "brown")+
    geom_line(aes(y=P4), size = 1.5,color = "orange")+
    # geom_line(aes(y=Delta), color = "red")+
    theme_bw()+
    
    geom_vline(xintercept = c (48,  96, 142))+
  #facet_grid(butterfly~SubNum)+
    facet_wrap(character~., ncol = 3)
  
  
  ggplot(sim, aes(x=trialN))+
    
    geom_line(aes(y=Delta), size = 1.5, color = "red")+
    # geom_line(aes(y=P2),size = 1.5, color = "darkgreen")+
    # geom_line(aes(y=P3),size = 1.5, color = "brown")+
    # geom_line(aes(y=P4), size = 1.5,color = "orange")+
    # geom_line(aes(y=Delta), color = "red")+
    geom_vline(xintercept = c (48,  96, 142))+
    facet_wrap(character~., ncol = 3)
  