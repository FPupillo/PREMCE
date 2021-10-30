simulate_dualLRwN<-function (Data,alphafast, alphaslow, Beta,N, initialQ){
  # This simulates data
  #incorporating the belief about the change point, according to reduced bayes model
  # inspired by nassar , Wilson et al (2010)
  #
  # Input
  #   Data: data containing the structure of the task
  #   alphafast<-fast learning rate
  #   alphaslow<- slow learning rate
  #   beta:  beta parameter
  #   initialQ: value of the initial Q
  #   N<-number of trials whose PE is aggregated to estimate CPP
  #
  # Output:
  #   dastaframe with $response and $object_cat
  # -------------
  
  # convert the object category into numeric variable
  categ<-levels(as.factor(Data$obj_category))
  
  murks<-levels(as.factor(Data$cuedCharacter))
  
  Data$cuedCharacter<-as.character(Data$cuedCharacter)
  
  for (n in 1:4){
    
    # Initialize variables: Qs, the expected values
    Data[[paste("Q", n, "fast", sep="")]]<-NA
    Data[[paste("Q", n, "slow", sep="")]]<-NA
    Data[[paste("P", "slow", sep="")]]<-NA
    
    
    # Ps (probabilities for each category's choice)
    Data[[paste("P", n, sep="")]]<-NA
    
  }
  
  # probability for the choice that participants' made on a trial
  Data$Prob<-NA
  
  # uncertainty as the 1/variance of the probability
  Data$uncertainty<-NA
  
  Data$variance<-NA
  
  # change point probability
  Data$CPP<-NA
  
  xindex<-c("x1", "x2", "x3", "x4")  # this is the observation: x=1 indicates the x for the character that is presented on each trials. The other two have x = 0. 
  Pindex<-c("P1", "P2", "P3", "P4") 
  
  # index variables for Q, P, Delta, and N
  Qindexfast<-c("Q1fast", "Q2fast", "Q3fast", "Q4fast")
  Qindexslow<-c("Q1slow", "Q2slow", "Q3slow", "Q4slow")
  
  Deltaindexfast<-c("Delta1fast", "Delta2fast", "Delta3fast", "Delta4fast")
  Deltaindexslow<-c("Delta1slow", "Delta2slow", "Delta3slow", "Delta4slow")

  # Counter for indicating which murk has to be updated
  count<-rep(0, 2)
  
  count2<-1
  
  # initialise choice probability and counter for the choiceprobability
  prob<-NA
  
  for (t in 1: nrow(Data)){
    
    # update the counter
    Murkcounter<-which(murks==Data$cuedCharacter[t])
    
    # The following loop retrieves the Q values of the murk that corresponds to the current trial (time t).
    if (count[Murkcounter]==0){ # if it is the first time that participants are seeing that murk
      
      Qfast<-rep(0.25, 4)#+(alpha/(alpha+beta+alpha+beta)) # initialize the Qs at 0.25
      Qslow<-rep(0.25, 4)#+(alpha/(alpha+beta+alpha+beta)) # initialize the Qs at 0.25
      alpha_k<-c(1, 1, 1,1) # we are initialising them at 1 (uniform distribution)
   
    } else{ # if this is not the first time participants are seeing that murk
      
      # retrieve the Qs and Ns from the last time participants saw the same murk
      Qfast<-Data[Data$cuedCharacter==Data$cuedCharacter[t],][count[Murkcounter],Qindexfast]
      Qslow<-Data[Data$cuedCharacter==Data$cuedCharacter[t],][count[Murkcounter],Qindexslow]

      #alpha_k<-base::colSums(Data[Data$cuedCharacter==Data$cuedCharacter[t], xindex], na.rm=T)+c(1,1,1,1)
      
    }
    
    count[Murkcounter]<-count[Murkcounter]+1 # update the counter
    
    # average of the distribution
    mean_k<-alpha_k/sum(alpha_k)
    
    alpha_0 = sum(alpha_k)
    
    var_k = (alpha_k*(alpha_0-alpha_k))/ (alpha_0^2 * (alpha_0+1))
    
    # change point pp
    if (t<=N){
      CPP<-0.5/t
    } else {
      CPP<-sum(abs(Data$Delta[(t-N):(t-1)]))/N
      
    }
    
    # update choice probabilities using the softmax distribution
    # index for the mixture
    m<-as.numeric((CPP*Qfast)+((1-CPP)*Qslow) )
    
    p<-softmax(m, Beta)
    
    Data[t, Pindex]<-p
    
    # make choice according to choice probabilities
    Data$response[t] <- chooseMultinom(p)
    
    Data$respCat[t]<-as.character(categ[Data$response[t]])
    
    #which cat is the corr ans?
    corr_resp<-Data$obj_category[t]
    
    # get accuracy
    if (Data$respCat[t]==corr_resp){
      Data$accuracy[t]<-1
    }else{
      Data$accuracy[t]<-0
    }
      
    # change point probability
    Data$CPP[t]<- CPP
    
    # get the observation as 1 if that category is present, and 0 if it is not
    if (Data$accuracy[t]==1){
      r<-1 
    } else {
      r<-0
    }
    
    # prediction error
    delta <- r - m[Data$response[t]]
    
    # update Q
    Qfast[Data$response[t]]<-Qfast[Data$response[t]]+alphafast*delta
    
    Qslow[Data$response[t]]<-Qslow[Data$response[t]]+alphaslow*delta
    
    # assign it to the dataset
    Data$Delta[t]<-delta
    
      # probability only for the response made by participant
      prob[count2]<-unlist(  p[Data$response[t]])
      
      # assign it to the dataset
      Data$Prob[t]<- prob[count2]
      
      # update the counter 
      count2<-count2+1

    # # get the observation as 1 if that category is present, and 0 if it is not
    # if (Data$obj_category[t]==categ[1]){
    #   obs<-c(1,0,0,0)
    # } else if (Data$obj_category[t]==categ[2]){
    #   obs<-c(0,1,0,0)
    # } else if (Data$obj_category[t]==categ[3]){
    #   obs<-c(0,0,1,0)
    # }else if (Data$obj_category[t]==categ[4]){
    #   obs<-c(0,0,0,1)
    # }
    
    # # Data[t, xindex]<-obs
    # 
    # # prediction error
    # Data[t, Deltaindexfast]<- obs - Qfast
    # Data[t, Deltaindexslow]<- obs - Qslow
    # 
    # Data$Delta[t]<-as.numeric(1-m[which(obs==1)])
    
    #Data[t, Qindexfast]<-as.numeric(Qfast+alphafast*Data[t, Deltaindexfast])
    
      Data[t, Qindexfast]<-Qfast
      
    #Data[t, Qindexslow]<-as.numeric(Qslow+alphaslow*Data[t, Deltaindexslow])
   
      Data[t, Qindexslow]<-Qslow
      
    # convert the choice into the order of the categories
    Data$response[t]<-which(unlist(Data[t,c("left_categ","centleft_categ" , 
                                            "centright_categ","right_categ") ])== 
                              paste0("stimuli/", Data$respCat[t],  ".png"))
     
  }
  
 
  # we could take the probability only for the congruent trials, but for now we are taking all the probabilities
  NegLL<--sum(log(Data$Prob), na.rm=T)

  return(Data)
}

