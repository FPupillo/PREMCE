#------------------------------------------------------------------------------#
# Data simulation
# created: Mon Oct 8th 09:27:27 2021"
#
# Simulate data at different values and explore behaviour of different agents
#
#------------------------------------------------------------------------------#

rm(list=ls())

library(ggplot2)
library(dplyr)
library(gridExtra)

source("computational_model/simulation_functions/simulate_dualLRwN.R")
source(("computational_model/softmax.R"))
source(("computational_model/chooseMultinom.R"))
source(("computational_model/BICcompute.R"))
source("helper_functions/getOpt.R")
source("helper_functions/isOpt.R")
source("helper_functions/getCatresp.R")
source("helper_functions/perOpt.R")
source("simulate_data/helper_functions/changeChar.R")
source("helper_functions/cumAcc.R")

# simulate data
# first, retrieve the structure of the trials
listPath<-"lists/"

# now the day1
day1list<-read.csv(paste0(listPath, "tasklistA1.csv"))

# now day2
day2list<-vector()
for (l in c("Aplus", "B", "C", "D")){
  
  currlist<-read.csv(paste0(listPath, "tasklist", l, "1", ".csv"))
  
  day2list<-rbind(day2list, currlist)
  
}

# simulate behaviour at different alpha values and beta
sims<-5

#alpha<-seq(0.1, 1, length.out =20)
beta<-seq(1, 7, length.out = 5)

# virtual part
part<-1

simData<-vector()

# the beta for the sigmoid
Ns<-floor(seq(1,10, length.out=5))

# create dataframe for performance
simPerf<-as.data.frame(matrix(NA, ncol = 4, nrow = sims*length(Ns)
                              *length(beta)))
names(simPerf)<-c("part",  "beta","hazardRate",  "perOptim")

# simulate data    
pb<-txtProgressBar(min=0, max= length(beta))

for (simul in 1:sims){
  for (b in beta){
    for (N in Ns){
      sim<-simulate_dualLRwN(Data=day2list, alphafast = 0.5, alphaslow = 0.3,
                             Beta=b, N = 15,
                           initialQ = 0.25)
      
      # calculate percentage optimal choice
      sim$task_resp.keys<-sim$response
      sim$character<-sim$cuedCharacter
      
      sim<-getOpt(sim)
      
      sim<-getCatresp(sim)
      
      sim<-isOpt(sim)
      
      sim<-perOpt(sim)
      
      sim$beta<-b
      sim$N<-N
      sim$simN<-sims[simul]
      
      sim$part<-part
      
      simData<-rbind(simData, sim)
      
      # now the dataframe for calculating optimal choice
      simPerf$part[part]<-part
      #simPerf$alpha[part]<-a
      simPerf$beta[part]<-b
      simPerf$sigmoidBeta[part]<-N
      
      simPerf$perOptim[part]<-sim$perOptimalChoice[nrow(sim)]
      
      
      setTxtProgressBar(pb, part) 
      
      part<-part+1
      
    }
 }
  print(paste("simulation N", simul))
}

# ggplot(sim, aes(x=trialN, y = perOptimalChoice))+ geom_line()+
#   geom_vline(xintercept = c(48, 96,144))
#   facet_wrap(.~s)

# save the data
#save(list = c("simPerf", "simData"), file = "output_files/simulationDataBayesian2.Rdata")
save("output_files/simulationDualLRwN.Rdata")

load("output_files/simulationDualLRwN.Rdata")

# summarise it first
Datawide<- sim %>%
  group_by( trialN, s, beta) %>%
  summarise(mean = mean(perOptimalChoice), sd = sd(perOptimalChoice))

# Datawide$beta<-as.factor(Datawide$beta)
# 
# Datawide$s<-as.factor(Datawide$s)
# 
ggplot(sim, aes(x = trialN , y =CPP)) +
  geom_vline(xintercept = c(48, 96,144))+

  #stat_summary(fun.y="mean",geom="line") +
  geom_line()+
  theme_bw()
  #facet_wrap(.~s)
# 
# ggsave("output_files/simulatedData.jpg")
# 
# # now create the before/after change point graphs
# AccData<-simData  %>%
#   group_by(part, s, beta,switch_cond)  %>%
#   mutate(befAft=c(rep("afterCP", times=24), rep("beforeCP", times=24)))
# 
# ccData$h<-round(AccData$h, 3)
# 
# 
# AccData$beta <-round(AccData$beta, 3)
# AccData$beta<-round(AccData$beta, 3)
# 
# 
# ggplot(AccData, aes(befAft, isoptimal, color = befAft))+
#   # geom_bar(aes(befAft, isoptimal, fill = befAft),
#   #         position="dodge",stat="summary", fun.y="mean", SE=T)+
#   stat_summary(fun.data = "mean_cl_boot", size = 0.8, geom="errorbar", width=0.2 )+
#   #geom_jitter( size=1,width=0.1)+
#   theme(axis.text.x = element_blank())+ # we are showing the different levels through the colors, so we can avoid naming the bars
#   theme(axis.ticks.x = element_blank())+
#   
#   theme_bw()+
#   #geom_line(data = groupData, aes(befAft, accuracy,group = SubNum), size=1, alpha=0.2, stat="summary")+
#   
#   facet_grid(.~s)+
#   theme(strip.text.x = element_text(size = 13))+
#   ylab("% optimal choice")+
#   scale_fill_manual("legend", values = c("afterCP" = "darkgoldenrod1", "beforeCP" = "darkred"))+
#   theme(axis.text.x = element_blank())
# #theme(axis.title=element_text(size=14,face="bold"))+
# #theme(legend.position = "none")+
# #ylim(0,1)
# 
# ggplot(AccData, aes(befAft, CPP, color = befAft))+
#   # geom_bar(aes(befAft, isoptimal, fill = befAft),
#   #         position="dodge",stat="summary", fun.y="mean", SE=T)+
#   stat_summary(fun.data = "mean_cl_boot", size = 0.8, geom="errorbar", width=0.2 )+
#   #geom_jitter( size=1,width=0.1)+
#   theme(axis.text.x = element_blank())+ # we are showing the different levels through the colors, so we can avoid naming the bars
#   theme(axis.ticks.x = element_blank())+
#   
#   theme_bw()+
#   #geom_line(data = groupData, aes(befAft, accuracy,group = SubNum), size=1, alpha=0.2, stat="summary")+
#   
#   facet_grid(.~s)+
#   theme(strip.text.x = element_text(size = 13))+
#   ylab("Learning Rate")+
#   scale_fill_manual("legend", values = c("afterCP" = "darkgoldenrod1", "beforeCP" = "darkred"))+
#   #theme(axis.text.x = element_text( size = 20, angle = 45))+
#   theme(axis.text.x = element_blank())
# #theme(axis.title=element_text(size=14,face="bold"))
# #theme(legend.position = "none")
# #ylim(0,1)
# 
# AccData$beta<-round(AccData$beta, 3)
# 
# ggplot(AccData, aes(befAft, lr, color = befAft))+
#   # geom_bar(aes(befAft, isoptimal, fill = befAft),
#   #         position="dodge",stat="summary", fun.y="mean", SE=T)+
#   stat_summary(fun.data = "mean_cl_boot", size = 0.8, geom="errorbar", width=0.2 )+
#   #geom_jitter( size=1,width=0.1)+
#   theme(axis.text.x = element_blank())+ # we are showing the different levels through the colors, so we can avoid naming the bars
#   theme(axis.ticks.x = element_blank())+
#   
#   theme_bw()+
#   #geom_line(data = groupData, aes(befAft, accuracy,group = SubNum), size=1, alpha=0.2, stat="summary")+
#   
#   facet_grid(.~beta)+
#   theme(strip.text.x = element_text(size = 13))+
#   ylab("Learning Rate")+
#   scale_fill_manual("legend", values = c("afterCP" = "darkgoldenrod1", "beforeCP" = "darkred"))+
#   #theme(axis.text.x = element_text( size = 20, angle = 45))+
#   theme(axis.text.x = element_blank())
# #theme(axis.title=element_text(size=14,face="bold"))
# #theme(legend.position = "none")
# #ylim(0,1)
# 
# # change point probability
# ggplot(AccData, aes(befAft, CPP, color = befAft))+
#   # geom_bar(aes(befAft, isoptimal, fill = befAft),
#   #         position="dodge",stat="summary", fun.y="mean", SE=T)+
#   stat_summary(fun.data = "mean_cl_boot", size = 0.8, geom="errorbar", width=0.2 )+
#   #geom_jitter( size=1,width=0.1)+
#   theme(axis.text.x = element_blank())+ # we are showing the different levels through the colors, so we can avoid naming the bars
#   theme(axis.ticks.x = element_blank())+
#   
#   theme_bw()+
#   #geom_line(data = groupData, aes(befAft, accuracy,group = SubNum), size=1, alpha=0.2, stat="summary")+
#   
#   facet_grid(h~beta)+
#   theme(strip.text.x = element_text(size = 13))+
#   ylab("CPP")+
#   scale_fill_manual("legend", values = c("afterCP" = "darkgoldenrod1", "beforeCP" = "darkred"))+
#   #theme(axis.text.x = element_text( size = 20, angle = 45))+
#   theme(axis.text.x = element_blank())
# 
# # runlength
# ggplot(AccData, aes(befAft, expRLength, color = befAft))+
#   # geom_bar(aes(befAft, isoptimal, fill = befAft),
#   #         position="dodge",stat="summary", fun.y="mean", SE=T)+
#   stat_summary(fun.data = "mean_cl_boot", size = 0.8, geom="errorbar", width=0.2 )+
#   #geom_jitter( size=1,width=0.1)+
#   theme(axis.text.x = element_blank())+ # we are showing the different levels through the colors, so we can avoid naming the bars
#   theme(axis.ticks.x = element_blank())+
#   
#   theme_bw()+
#   #geom_line(data = groupData, aes(befAft, accuracy,group = SubNum), size=1, alpha=0.2, stat="summary")+
#   
#   #facet_grid(.~beta)+
#   theme(strip.text.x = element_text(size = 13))+
#   ylab("ExpRunLength")+
#   scale_fill_manual("legend", values = c("afterCP" = "darkgoldenrod1", "beforeCP" = "darkred"))+
#   #theme(axis.text.x = element_text( size = 20, angle = 45))+
#   theme(axis.text.x = element_blank())
# 
# # uncertainty
# ggplot(AccData[AccData$beta ==15.778,], aes(befAft, uncertainty, color = befAft))+
#   # geom_bar(aes(befAft, isoptimal, fill = befAft),
#   #         position="dodge",stat="summary", fun.y="mean", SE=T)+
#   stat_summary(fun.data = "mean_cl_boot", size = 0.8, geom="errorbar", width=0.2 )+
#   #geom_jitter( size=1,width=0.1)+
#   theme(axis.text.x = element_blank())+ # we are showing the different levels through the colors, so we can avoid naming the bars
#   theme(axis.ticks.x = element_blank())+
#   
#   theme_bw()+
#   #geom_line(data = groupData, aes(befAft, accuracy,group = SubNum), size=1, alpha=0.2, stat="summary")+
#   
#   facet_wrap(.~h)+
#   theme(strip.text.x = element_text(size = 13))+
#   ylab("uncertainty")+
#   scale_fill_manual("legend", values = c("afterCP" = "darkgoldenrod1", "beforeCP" = "darkred"))+
#   #theme(axis.text.x = element_text( size = 20, angle = 45))+
#   theme(axis.text.x = element_blank())
# 
# 
# # aggregate simperf at simulation level
# simPerfAgg<-simPerf %>%
#   group_by(hazardRate ,beta) %>%
#   summarise( dev = sd(perOptim),
#              perOptim = mean(perOptim) ,
#              n = n(),
#              se = dev/sqrt(n),
#              upperSE = perOptim+se,
#              lowerSE = perOptim-se)
# 
# 
# # plot matrix
# p1<-ggplot(data = simPerfAgg, aes(x = hazardRate, y = beta, fill = perOptim))+
#   geom_tile()+
#   scale_fill_gradient(low="black", high="white",name ="p(correct \nchoice)") + theme_bw()
# 
# # plot line graph
# p2<-ggplot(data=simPerfAgg, aes(x=beta,y=perOptim, color=hazardRate, group=hazardRate)) + 
#   theme_bw() + 
#   #geom_errorbar(aes(ymin = lowerSE, ymax = upperSE), width = 0.001) + 
#   geom_line() + scale_color_continuous(name = "hazard rate") 
# 
# p3<-ggplot(data=simPerfAgg, aes(x=hazardRate,y=perOptim, color=beta, group=beta)) + 
#   theme_bw() + 
#   #geom_errorbar(aes(ymin = lowerSE, ymax = upperSE), width = 0.001) + 
#   geom_line() + scale_color_continuous(name = "beta") 
# 
# # arrange the two
# grid.arrange(p1, p2, p3,ncol=2)
# 
# ggsave("output_files/optimalperf.jpg")
# 
# 
# # select only beta less than 10
# 
# AccDataSub<-AccData %>% filter(beta <  10.000000 )
# 
# # plot as a function of before or after
# AccDataSub$h<-as.factor(AccDataSub$h)
# #alphalev<-levels(AccDataSub$alpha)
# 
# ggplot(AccDataSub, aes(befAft, isoptimal, color = befAft))+
#   # geom_bar(aes(befAft, isoptimal, fill = befAft),
#   #         position="dodge",stat="summary", fun.y="mean", SE=T)+
#   stat_summary(fun.data = "mean_cl_boot", size = 0.8, geom="errorbar", width=0.2 )+
#   #geom_jitter( size=1,width=0.1)+
#   theme(axis.text.x = element_blank())+ # we are showing the different levels through the colors, so we can avoid naming the bars
#   theme(axis.ticks.x = element_blank())+
#   
#   #theme_bw()+
#   #geom_line(data = groupData, aes(befAft, accuracy,group = SubNum), size=1, alpha=0.2, stat="summary")+
#   
#   facet_grid(h~beta)+
#   theme(strip.text.x = element_text(size = 13))+ 
#   ylab("Accuracy")+
#   scale_fill_manual("legend", values = c("afterCP" = "darkgoldenrod1", "beforeCP" = "darkred"))+
#   theme(axis.text.x = element_text( size = 20))+
#   theme(axis.title=element_text(size=14,face="bold"))+
#   theme(legend.position = "none")+
#   ylim(0,1)
# 
# simData$h<-as.factor(simData$h)
# 
# simData$beta<-as.factor(simData$beta)
# 
# # plot expected run length
# ggplot(simData[simData$h=="0.0208333333333333" & simData$beta=="13.6666666666667" ,], aes(x=trial, y = expRLength, colour=expRLength))+
#   scale_fill_gradient(low="black", high="white",name ="p(correct \nchoice)") + theme_bw()+
#   geom_vline(xintercept = c(48, 96,144))+
#   facet_wrap(.~ cuedCharacter)+
#   geom_point()
# 
# ggplot(simData[simData$h=="0.0208333333333333" & simData$beta=="7.33333333333333" ,], aes(x=trial,y = lr))+
#   geom_line(color = "red")+
#   geom_vline(xintercept = c(48, 96,144))+
#   facet_wrap(.~ cuedCharacter)
# 
# ggplot(simData[simData$h == 0.015625, ], aes(x=trial,y = uncertainty))+
#   geom_line(color = "red")+
#   geom_vline(xintercept = c(48, 96,144))+
#   facet_grid(cuedCharacter~ beta)
# 
# ggplot(simData[simData$h=="0.0208333333333333" & simData$beta=="1" ,], aes(x=trial,y = CPP))+
#   geom_line(color = "red")+
#   geom_vline(xintercept = c(48, 96,144))+
#   facet_wrap(.~ cuedCharacter)
# 
# # create a table with the task structure 
# day2list1$trial
# 
