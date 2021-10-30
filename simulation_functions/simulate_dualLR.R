simulate_dualLR<-function (Data, Beta,sigmoidBeta, initialQ){
  # This simulates data
  #incorporating the belief about the change point, according to reduced bayes model
  # inspired by nassar , Wilson et al (2010)
  #
  # Input
  #   Data: data containing the structure of the task
  #   beta:  beta parameter
  #   initialQ: value of the initial Q
  #   signmoidBeta<-beta value for the sigmoid dfunction
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
  
  # change point probability
  Data$CPP<-NA
  
  # probability of change point not occurring
  Data$CPnot<-NA
  
  # fast vs slow learning rate
  Data$posVSneg<-NA
  
  #true probability of a change point - 3 changepoints divided by the number of the trials
  CPtrue<-3/nrow(Data)
  
  # we could leave it as a free parameter
  #CPtrue<-hazardRate
  
  # learning rate
  Data$lr<-NA
  
  xindex<-c("x1", "x2", "x3", "x4")  # this is the observation: x=1 indicates the x for the character that is presented on each trials. The other two have x = 0. 
  
  Pindex<-c("P1", "P2", "P3", "P4") 
  
  # index variables for Q, P, Delta, and N
  Qindexfast<-c("Q1fast", "Q2fast", "Q3fast", "Q4fast")
  Qindexslow<-c("Q1slow", "Q2slow", "Q3slow", "Q4slow")
  
  Deltaindexfast<-c("Delta1fast", "Delta2fast", "Delta3fast", "Delta4fast")
  Deltaindexslow<-c("Delta1slow", "Delta2slow", "Delta3slow", "Delta4slow")
  
  # expected runlength
  Data$expRLength<-NA
  
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
      
      Qfast<-rep(initialQ, 4)#+(alpha/(alpha+beta+alpha+beta)) # initialize the Qs at 0.25
      Qslow<-rep(initialQ, 4)#+(alpha/(alpha+beta+alpha+beta)) # initialize the Qs at 0.25
      
      
      alpha_k<-c(1, 1, 1,1) # we are initialising them at 1 (uniform distribution)
      
      Data$expRLength[t]<-1
      
      # change point probability is ground truth
      CPP<-3/192
      
    } else{ # if this is not the first time participants are seeing that murk
      
      # retrieve the Qs and Ns from the last time participants saw the same murk
      Qfast<-Data[Data$cuedCharacter==Data$cuedCharacter[t],][count[Murkcounter],Qindexfast]
      
      Qslow<-Data[Data$cuedCharacter==Data$cuedCharacter[t],][count[Murkcounter],Qindexslow]
      
      
      # CPP is the CPP at the last trial
      CPP<-Data[t-1, "CPP"]
      # Data[t, N]<-unlist(Data[Data$cuedCharacter==Data$cuedCharacter[t],][count[Data$cuedCharacter[t]], N])
      #
      alpha_k<-base::colSums(Data[Data$cuedCharacter==Data$cuedCharacter[t], xindex], na.rm=T)
      
    }
    
    count[Murkcounter]<-count[Murkcounter]+1 # update the counter
    
    # average of the distribution
    mean_k<-alpha_k/sum(alpha_k)
    
    alpha_0 = sum(alpha_k)
    
    var_k = (alpha_k*(alpha_0-alpha_k))/ (alpha_0^2 * (alpha_0+1))
    
    # update choice probabilities using the softmax distribution
    # index for the mixture
    m<-as.numeric((CPP*Qfast)+((1-CPP)*Qslow) )
    
    p<-softmax(m, Beta)
    
    Data[t, Pindex]<-p
    
    # make choice according to choice probabilities
    Data$response[t] <- chooseMultinom(p)
    
    
    # now map the response into the current trial categories
    Data$response[t]<-which(as.character(unlist(Data[t, c("left_categ", "centleft_categ", 
                                                          "centright_categ", "right_categ")]) )
                            == paste0("stimuli/",categ[Data$response[t]], ".png"))
    
    # compute Q, delta, and choice probability for actual choice, only if a choice was made
    if (Data$response[t]!=0 & !is.na(Data$response[t]) ){
      
      # probability only for the response made by participant
      prob[count2]<-unlist(  p[Data$response[t]])
      
      # assign it to the dataset
      Data$Prob[t]<- prob[count2]
      
      # update the counter 
      count2<-count2+1
      
      # compute the accuracy
      if (categ[Data$response[t]] == Data$obj_category[t]){
        Data$accuracy[t]<-1
      } else {
        Data$accuracy[t]<-0
      }
      
      # change point probability
      if (Data$accuracy[t]==1){ # if the accuracy was correct
        
        # probability of response =1 giving the change point (1-0.75 = 0.25. 0.25/3)
        CPeq1<-(Data$Prob[t]*0.083)+(1-Data$Prob[t])*(1/3*0.75+2/3 *0.083)
        
        # what is the probability of CP not occurring given that feedback is 1 - see "BayesianModelPresentation.odp"
        Data$CPnot[t]<-(Data$Prob[t]*(1-CPtrue))/(Data$Prob[t]*(1-CPtrue)+CPeq1*CPtrue)
        
      } else {
        # probability of response =1 giving the change point
        CPeq1<-((Data$Prob[t])*0.083)+(1-Data$Prob[t])*(1/3*0.75+2/3 *0.083)
        
        # what is the probability of CP not occurring given that feedback is 0
        Data$CPnot[t]<-((1-Data$Prob[t])*(1-CPtrue))/(Data$Prob[t]*(1-CPtrue)+CPeq1*CPtrue)
        
      }
      
    } else { # if a choice was not made
      
      # retrieve the choice probability from the last trial of that murk
      Data$Prob[t]<-Data[Data$cuedCharacter==Data$cuedCharacter[t],][count[Data$cuedCharacter[t]]-1,"Prob"]
      
      # calculate the probability of CP not occurring given that the CP = 0 and is considered incorrect
      CPeq1<-((1-Data$Prob[t])*0.083)+Data$Prob[t]*(1/3*0.75+2/3 *0.083)
      
      # what is the probability of CP not occurring given that feedback is 0
      Data$CPnot[t]<-((1-Data$Prob[t])*(1-CPtrue))/((1-Data$Prob[t])*(1-CPtrue)+CPeq1)
      
      
    }
    
    # update the expected run length
    if (t<nrow(Data)){
      Data$expRLength[t+1]<-(Data$expRLength[t]+1)*( Data$CPnot[t])+  1-Data$CPnot[t]
    }
    
    # change point probability
    Data$CPP[t]<- 1-Data$CPnot[t]
    
    # get the observation as 1 if that category is present, and 0 if it is not
    if (Data$obj_category[t]==categ[1]){
      obs<-c(1,0,0,0)
    } else if (Data$obj_category[t]==categ[2]){
      obs<-c(0,1,0,0)
    } else if (Data$obj_category[t]==categ[3]){
      obs<-c(0,0,1,0)
    }else if (Data$obj_category[t]==categ[4]){
      obs<-c(0,0,0,1)
    }
    
    Data[t, xindex]<-obs
    
    # prediction error
    Data[t, Deltaindexfast]<- obs - Qfast
    Data[t, Deltaindexslow]<- obs - Qslow
    
    # the learning rate depends on the sigmoid
    #sigmoid<-1/(1+(CPP/(1-CPP))^(-sigmoidBeta))
    
    # sample choice
    #Data$posVSneg[t]<-sample(c("fast","slow"), 1, prob = c(sigmoid, 1-sigmoid))
    
    # select the models
    # if (Data$posVSneg[t] == "fast"){
    
    Data[t, Qindexfast]<-as.numeric(Qfast+0.3*Data[t, Deltaindexfast])
    
    #} else{
    
    Data[t, Qindexslow]<-as.numeric(Qslow+0.3*Data[t, Deltaindexslow])
    
  }
  
  #   # compute uncertainty as the 1/variance of the probability
  #   uncertainty<-1/(var(unlist(Data[t, Qupindex]))+0.01)
  #   
  #   # uncertainty as the -log(sum prob * log prob)
  #   uncertainty2<- -sum(  unlist(Data[t,Qupindex]) *log(unlist(Data[t,Qupindex])))
  #   
  #  if (any(count==0) ){ # while one of the two butterlfy has not been shown
  #     
  #     Data$uncertainty[t]<-uncertainty
  #     Data$uncertainty2[t]<-uncertainty2
  #     #Data$CPP[t]<-probobs
  #     # uncertainty as the -log(sum prob * log prob)
  #     
  #   } else{
  #       
  #     # retrieve uncertainty on the last trial of the other butterfly
  #     unctmin1<-Data[Data$cuedCharacter!=Data$cuedCharacter[t],][count[which(murks!=Data$cuedCharacter[t])], "uncertainty"]
  #     
  #     unct2tmin1<-Data[Data$cuedCharacter!=Data$cuedCharacter[t],][count[which(murks!=Data$cuedCharacter[t])], "uncertainty2"]
  #     
  #     # retrieve probability of the observation on the previous trial for the same butterfly
  #     #probobstminus1<-Data[Data$butterfly==Data$butterfly[t],][count[Data$butterfly[t]]-1,paste("P", Data$FlowNum[t], sep="") ]
  #     
  #      #average the two "uncertainties"
  #     Data$uncertainty[t]<-mean(uncertainty,unctmin1)
  #     Data$uncertainty2[t]<-mean(uncertainty2,unct2tmin1)
  #     # Data$CPP[t]<-probobs*probobstminus1
  #     
  #     
  #  }
  # }
  # 
  # we could take the probability only for the congruent trials, but for now we are taking all the probabilities
  NegLL<--sum(log(Data$Prob), na.rm=T)
  
  # if (print ==1){
  #   return(NegLL)
  # }else if ( print==2){
  #   return (list("Negative LogLikel"=NegLL, "Q1"= Data$Q1,"Q2"= Data$Q2,"Q3"= Data$Q3,"Q4"= Data$Q4,
  #                "Delta1"= Data$Delta1, "Delta2"= Data$Delta2,"Delta3"= Data$Delta3, "Delta4"= Data$Delta4
  #   ))
  # } else if(print==3){
  #   return(Data)}
  
  return(Data)
}

