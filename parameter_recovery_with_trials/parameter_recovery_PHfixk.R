#------------------------------------------------------------------------------#
# simulating trials switch points and character - Pearce Hall
# here we are fixing the k
# created: "Tue Oct 26 14:26:25 2021"
#------------------------------------------------------------------------------#

setwd(dirname(getwd()))
setwd(dirname(getwd()))

#soruce the functions
source("simulate_data/taskSim2.R")
source("computational_model/simulation_functions/simulate_PearceHall.R")
source(("computational_model/softmax.R"))
source(("computational_model/chooseMultinom.R"))
source(("computational_model/BICcompute.R"))
source("computational_model/likelihood_functions/lik_PearceHall.R")
source("computational_model/fitting_functions/fit_PearceHallfixk.R")
source("helper_functions/getCat.R")
source("helper_functions/preSim.R")
source("helper_functions/getcorrCat.R")

# libraries
library(doParallel)
library(foreach)
#------------------------------------------------------------------------------#
# parameters
#------------------------------------------------------------------------------#
Pcong<-0.75
Ntrial<-24
Ncharacter<-1
switchN<-3
categN<-4
#------------------------------------------------------------------------------#
lengthparam<-5

# the simulations. this script is going to take as many cores as there are 
# scripts
sims<-3

alphaseq<-seq(0,1, length.out =lengthparam)
betaseq<-seq(1,20, length.out=lengthparam)

kseq<-seq(0,1, length.out=lengthparam)
etaseq<-seq(0,1, length.out=lengthparam)

data<-matrix(NA, nrow=1,ncol = 7)

df<-data.frame(data)

names(df)<-c("simAlpha", "fitAlpha","simBeta", "fitBeta",
             "simEta", "fitEta", "BIC")

model<-simulate_PearceHall

fit<-fit_PearceHallfixk

modname<-as.character("PHfixk")

name<- paste("output_files/parameterRecovery", modname, ".Ntrial=",Ntrial, 
             ".initialQ=", 0.25 , sep="")

write.csv(df, paste0(name, ".csv"), row.names = F)

# detect cores for runnning in parallel
cores=detectCores()

cl <- makeCluster(sims, outfile="") # to not overload your computer
registerDoParallel(cl)


# progress bar
pb<-txtProgressBar(min=0, max=(lengthparam^3)*sims, style =3)

#for(sims in 1:sims){

dat<-foreach (j=1:sims, .combine=rbind,.packages=c('pracma'))  %dopar% {
  
  # initiate a counter
  count<-1
  
  for (a in 1:length(alphaseq)){
    for (b in 1:length(betaseq)){
      # for (k in 1:length(kseq)){
      for (eta in 1:length(etaseq)){
        # generate the task
        Data<-taskSim(Pcong, Ntrial, Ncharacter, switchN, categN)
        
        # prepare data
        Data$cuedCharacter<-Data$character
        
        for (n in 1:nrow(Data)){
          Data[n,1:4]<-paste0("stimuli/",Data[n,1:4], ".png")
        }
        
        if (!is.null(formals(model)$k)){
          
          # simulate data with the RW obs
          sim<-model(Data=Data,alpha=alphaseq[a], beta =betaseq[b], 
                     k=1, eta = etaseq[eta],
                     initialQ = 0.25)
          
        } else {
          
          # simulate data with the RW obs
          sim<-model(Data=Data,alpha=alphaseq[a], beta =betaseq[b],
                     initialQ = 0.25)
        }
        
        # calculate percentage optimal choice
        # rename variables
        sim$task_resp.keys<-sim$response
        sim$character<-sim$cuedCharacter
        
        # create response category
        categ<-as.character(Data[1,1:4])
        
        sim$respCat<-substr(categ[sim$response], 9, nchar(categ[sim$response])-4)
        
        # now fit the data
        est<-fit(data=sim, alphaBound = c(0,1),  betaBound = c(1,10),
                 etaBound = c(0,1),
                 initialQ =0.25)
        
        estAlpha<-est$alphabetaPAR[1]
        
        estBeta<-est$alphabetaPAR[2]
        
        #estK<-est$alphabetaPAR[3]
        
        estEta<-est$alphabetaPAR[3]
        
        
        temp<-read.csv( paste0(name, ".csv"))
        
        
        #append the data
        temp[nrow(temp)+1, ]<-c(alphaseq[a], estAlpha, betaseq[b], estBeta,
                                etaseq[eta], estEta, est$BIC)
        
        
        #write it
        write.csv(temp, paste0(name, ".csv"), row.names = F)
        
        count <-count+1
        
        setTxtProgressBar(pb, count) 
        
      }
    }
  }
}

stopCluster(cl)

