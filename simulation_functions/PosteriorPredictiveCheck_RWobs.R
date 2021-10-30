#------------------------------------------------------------------------------#
# simulated vs actual - posterior predictive check - RW model obs
# created:"Wed Oct 20 16:18:13 2021"
#
# S
#
#------------------------------------------------------------------------------#

rm(list=ls())

library(ggplot2)
library(dplyr)
library(gridExtra)

source("computational_model/simulation_functions/simulate_RescorlaWagner_obsALL.R")
source(("computational_model/softmax.R"))
source(("computational_model/chooseMultinom.R"))
source(("computational_model/BICcompute.R"))
source(("helper_functions/cumAcc.R"))
source("computational_model/likelihood_functions/lik_RescorlaWagner_obsALL.R")
source("computational_model/fitting_functions/fit_RescorlaWagner_obsALL.R")
source("helper_functions/getOpt.R")
source("helper_functions/isOpt.R")
source("helper_functions/getCatresp.R")
source("helper_functions/perOpt.R")
source("helper_functions/selPart.R")




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

# retrieve the estimates
estimates<-read.csv("output_files/estimated_parameters_obsALL.csv")

# initial Q
initialQ<-0.25

# get parameters
alphaMean<-mean(estimates$alpha)
alphaSD<-sd(estimates$alpha)
betaMean<-mean(estimates$beta)
betaSD<-sd(estimates$beta)

alphas<-estimates$alpha
betas<-estimates$beta

simAll<-vector()
# create simulated data
for (j in 1:nrow(estimates)){
  
  # simulate
  sim<-simulate_RescorlaWagner_obsALL(Data=day2list, 
                                      alpha = alphas[j], 
                                      beta = betas[j], initialQ = initialQ)
  
  # calculate percentage optimal choice
  # rename variables
  sim$task_resp.keys<-sim$response
  sim$character<-sim$cuedCharacter  
  
  # get optimal choice
  sim<-getOpt(sim)
  
  sim<-getCatresp(sim)
  
  sim<-isOpt(sim)
  
  sim<-perOpt(sim)
  
  # subset
  df<-sim[,c("first_character", "second_character", "switch_cond", "optimalChoice" ,"respCat" ,"isoptimal" ,             
           "perOptimalChoice" , "cumAcc", "trialNbychar" , "perOptimalChoiceByMurks",
           "cumAccbyMurks" ,"trialN"  )]
  
  df$partNum<-j+100
  
  simAll<- rbind(simAll, df)
  
}

# now actual part
# sel files
cd<-getwd()
setwd("data_files")
files2<-selPart(2)
setwd(cd)

# delete the double files (3)
numb3<-files2[substr(files2, 1,2)=="01"][1:2]

files2<-files2[!files2 %in% numb3]

AccDay2task<-vector()

#participants<-estimates$PartNum
for (j in 1 : (length(files2))){
  tryCatch({
    day2<-read.csv(paste("data_files/", files2[j], sep=""))
    
    # select all but the first block, which is for refreshing contingencies
    task2<-day2[day2$switch_cond!=1,]
    
    # exclude NAs
    task2<-task2[!is.na(task2$switch_cond),]
    task2$SubNum<-rep(as.numeric(substr(files2[j], 1,2)), times=nrow(task2))
    
    # report the character
    task2$character<-NA
    for (t in 1: nrow(task2)){
      
      task2$character[t]<-substr(task2$cuedCharacter[t], 9, 
                                 (nchar(as.character(task2$cuedCharacter[t]))-4))
    }
    
    # get optimal choice
    task2<-getOpt(task2)
    
    task2<-getCatresp(task2)
    
    task2<-isOpt(task2)
    
    optchoice<-perOpt(task2)
    
    # check if there are elements that are not present in either variables
    missingel<-setdiff(names(AccDay2task), names(optchoice))
    
    # create the missing elements in order to being able to bind the two
    for (elements in missingel){
      optchoice[[elements]]<-NA
    }
    
  
    # subset
    df<-optchoice[,c("first_character", "second_character","switch_cond", "optimalChoice" ,"respCat" ,"isoptimal" ,             
              "perOptimalChoice" , "cumAcc", "trialNbychar" , "perOptimalChoiceByMurks",
              "cumAccbyMurks" ,"trialN"  )]
    
    # add partNum
    df$partNum<-optchoice$participant

    # bind to the previous participant
    AccDay2task<-rbind(AccDay2task, df)
    
    
    
  },  error=function(e){cat("ERROR :",conditionMessage(e), "\n", "participant=", j)}) 
  
}

# we need to bind the two

simAll$type<-rep("simulated", nrow(simAll))
AccDay2task$type<-rep("actual", nrow(AccDay2task))

# bind the two
dataAll<-rbind(simAll, AccDay2task)

# create before and after
SimVsEmp<-dataAll  %>%
  group_by(partNum, switch_cond, type)  %>%
  dplyr::mutate(befAft=c(rep("afterCP", times=24), rep("beforeCP", times=24)))

# get within participant SE
library(Rmisc)
dat_summary <- summarySEwithin(SimVsEmp,
                               measurevar = "perOptimalChoice",
                               withinvars = c("befAft"),
                               betweenvars = c("type"),
                               idvar = "partNum")



ggplot(SimVsEmp, aes(x= befAft, y = perOptimalChoice, color= type))+
  #stat_summary(fun.data = "mean_cl_boot", size = 0.8, geom="errorbar", width=0.2 )+
  geom_errorbar(aes(y = perOptimalChoice, ymin = perOptimalChoice - ci, ymax = perOptimalChoice + ci),
                 width = 0.10, data=dat_summary)+
  geom_point(aes(y = perOptimalChoice),data=dat_summary)+
  #geom_line()+
  #facet_grid(.~type)+  
  theme_bw()
  #ylim(0,1)

#AccDay2task$switch_cond<-as.factor(AccDay2task$switch_cond)





# plot the graph with adding the data
ggplot(data = simPerfAgg, aes(x = alpha, y = beta, fill = perOptim))+
  geom_tile()+
  scale_fill_gradient(low="black", high="white",name ="p(correct \nchoice)") +
  geom_point(data = estimates, aes(x=alpha, y = beta, fill=NULL), color="red")+
  
  theme_bw()

  ggplot(data = estimates, aes(x=alpha, y = beta))+
  geom_point()
#
    