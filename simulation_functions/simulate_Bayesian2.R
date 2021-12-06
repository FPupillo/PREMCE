simulate_Bayesian2<-function (Data, Beta,hazardRate, initialQ){
  # This simulates data
  #incorporating the belief about the change point, according to reduced bayes model
  # inspired by nassar , Wilson et al (2010)
  #
  # Input
  #   Data: data containing the structure of the task
  #   beta:  beta parameter
  #   initialQ: value of the initial Q
  #   hazardRate: belief about how many change points there are
  #
  # Output:
  #   dastaframe with $response and $object_cat
  # -------------
  
  # convert the object category into numeric variable
  categ<-levels(as.factor(Data$obj_category))
  
  murks<-levels(as.factor(Data$cuedCharacter))
  
  Data$cuedCharacter<-as.character(Data$cuedCharacter)
  
  # Initialize variables: Qs, the expected values
  Data$Q1<-NA; Data$Q2<-NA; Data$Q3<-NA ; Data$Q4<-NA 
  
  # Ps (probabilities for each category's choice)
  Data$Qup1<-NA; Data$Qup2<-NA; Data$Qup3<-NA ; Data$Qup4<-NA
  
  Data$N1<-NA; Data$N2<-NA; Data$N3<-NA ; Data$N4<-NA
  
  Data$Delta1<-NA; Data$Delta2<-NA; Data$Delta3<-NA; Data$Delta4<-NA;
  
  Data$x1<-NA; Data$x2<-NA; Data$x3<-NA; Data$x4<-NA;
  
  #alphaindex<-c("alpha1", "alpha2", "alpha3") # the alphas here represent the pseudocounts for the categories at trial t. 
  
  # probability for the choice that participants' made on a trial
  Data$Prob<-NA
  
  # Delta, prediction error
  Delta<-c("Delta1", "Delta2", "Delta3", "Delta4")
  
  # uncertainty as the 1/variance of the probability
  Data$uncertainty<-NA
  
  # change point probability
  Data$CPP<-NA
  
  # probability of change point not occurring
  Data$CPnot<-NA
  
  # true probability of a change point - 3 changepoints divided by the number of the trials
  #CPtrue<-3/nrow(Data)
  
  # we could leave it as a free parameter
  CPtrue<-hazardRate
  
  # learning rate
  Data$lr<-NA
  
  # initialize alpha and beta
  alpha<-1
  beta<-1
  
  # number of categories
  #K<-length(unique(Data$catNum))
  
  # index variables for Q, P, Delta, and N
  Qindex<-c("Q1", "Q2", "Q3", "Q4")
  Qupindex<-c("Qup1", "Qup2", "Qup3", "Qup4") 
  Deltaindex<-c("Delta1", "Delta2", "Delta3", "Delta4")
  xindex<-c("x1", "x2", "x3", "x4")  # this is the observation: x=1 indicates the x for the character that is presented on each trials. The other two have x = 0. 
  
  # N is the sum about CP occurring when the same option was chosen (same Chategory)
  #N<-c("N1", "N2", "N3", "N4")
  
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
      
      #Data[t, Qindex]<-rep(0, 4)+(alpha/(alpha+beta+alpha+beta)) # initialize the Qs at 0.25
      
      Data[t, Qindex]<-rep(0.25, 4)#+(alpha/(alpha+beta+alpha+beta)) # initialize the Qs at 0.25
      
      #Data[t,N]<-rep(alpha+beta, 4) # initialize the Ns at 2
      
      alpha_k<-c(1, 1, 1,1) # we are initialising them at 1 (uniform distribution)
      
      Data$expRLength[t]<-1
      
      
    } else{ # if this is not the first time participants are seeing that murk
      
      # retrieve the Qs and Ns from the last time participants saw the same murk
      Data[t, Qindex]<-Data[Data$cuedCharacter==Data$cuedCharacter[t],][count[Murkcounter],Qupindex]
      
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
    p<-softmax(Data[t, Qindex], Beta)
    
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
        CPeq1<-((1-Data$Prob[t])*0.083)+Data$Prob[t]*(1/3*0.75+2/3 *0.083)
        
        # what is the probability of CP not occurring given that feedback is 0
        Data$CPnot[t]<-((1-Data$Prob[t])*(1-CPtrue))/((1-Data$Prob[t])*(1-CPtrue)+CPeq1*CPtrue)
        
      }
      
      # N is the belief that there wasn't a change point up to this trial for that choice
      # we need to take the n referred to the choice
      #Nchoice<-paste("N", Data$catNum[t], sep="")      
      
    } else { # if a choice was not made
      
      # retrieve the choice probability from the last trial of that murk
      Data$Prob[t]<-Data[Data$cuedCharacter==Data$cuedCharacter[t],][count[Data$cuedCharacter[t]]-1,"Prob"]
      
      # calculate the probability of CP not occurring given that the CP = 0 and is considered incorrect
      CPeq1<-((1-Data$Prob[t])*0.083)+Data$Prob[t]*(1/3*0.75+2/3 *0.083)
      # what is the probability of CP not occurring given that feedback is 0
      Data$CPnot[t]<-((1-Data$Prob[t])*(1-CPtrue))/((1-Data$Prob[t])*(1-CPtrue)+CPeq1)
      
      # also, if no response has been made, the N is based on the category shown
      #Nchoice<-paste("N", Data$corrCat[t], sep="")      
      
    }
    
    # update the expected run length
    if (t<nrow(Data)){
    Data$expRLength[t+1]<-(Data$expRLength[t]+1)*( Data$CPnot[t])+  1-Data$CPnot[t]
    }
    # learning rate
    #Data$lr[t]<-Data$CPnot[t]/(Data[t, Nchoice]+alpha+beta)
    
    Data$lr[t]<-(1+ (1-Data$CPnot[t]) *Data$expRLength[t])/(Data$expRLength[t]+1)
    
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
    Data[t, Delta]<- obs - Data[t, Qindex]
    
    # update - rescorla wagner formula multiplied by probability of change point
    #Data[t, Qupindex]<-as.numeric(Data[t, Qindex]+Data$lr[t]*Data[t, Delta]*Data$CPnot[t])
    
    Data[t, Qupindex]<-as.numeric(Data[t, Qindex]+Data$lr[t]*Data[t, Delta])
    
    
    # update N
   # Data[t, Nchoice]<-Data[t, Nchoice]+Data$CPnot[t]
    
    # compute uncertainty as the 1/variance of the probability
    uncertainty<-1/(var(unlist(Data[t, Qupindex]))+0.01)
    
    # uncertainty as the -log(sum prob * log prob)
    uncertainty2<- -sum(  unlist(Data[t,Qupindex]) *log(unlist(Data[t,Qupindex])))
    
    # change point probability
    Data$CPP[t]<- 1-Data$CPnot[t]
    
    # probability of that observations
    # probobs<-Data[t, paste("P", Data$FlowNum[t], sep="")]
    
   if (any(count==0) ){ # while one of the two butterlfy has not been shown
      
      Data$uncertainty[t]<-uncertainty
      Data$uncertainty2[t]<-uncertainty2
      #Data$CPP[t]<-probobs
      # uncertainty as the -log(sum prob * log prob)
      
    } else{
        
      # retrieve uncertainty on the last trial of the other butterfly
      unctmin1<-Data[Data$cuedCharacter!=Data$cuedCharacter[t],][count[which(murks!=Data$cuedCharacter[t])], "uncertainty"]
      
      unct2tmin1<-Data[Data$cuedCharacter!=Data$cuedCharacter[t],][count[which(murks!=Data$cuedCharacter[t])], "uncertainty2"]
      
      # retrieve probability of the observation on the previous trial for the same butterfly
      #probobstminus1<-Data[Data$butterfly==Data$butterfly[t],][count[Data$butterfly[t]]-1,paste("P", Data$FlowNum[t], sep="") ]
      
       #average the two "uncertainties"
      Data$uncertainty[t]<-mean(uncertainty,unctmin1)
      Data$uncertainty2[t]<-mean(uncertainty2,unct2tmin1)
      # Data$CPP[t]<-probobs*probobstminus1
      
      
   }
  }
  
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

