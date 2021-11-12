simulate_RescorlaWagner_EC<-function ( Data,alpha, beta,  initialV){
  #----------------------------------------------------------------------------#
  # This function simulate participants' choices using a RW eva;iative-
  # counterfactual model
  #
  # Input
  #   Data: data containing the structure of the task
  
  #   alpha: alpha parameter 
  #   beta:  beta parameter
  #   initialV: value of the inital Q
  #
  # Output:
  #   dataframe with $response and $object_cat
  #----------------------------------------------------------------------------#
  
  # convert the object category into numeric variable
  categ<-levels(as.factor(Data$obj_category))
  
  murks<-levels(as.factor(Data$cuedCharacter))
  
  Data$cuedCharacter<-as.character(Data$cuedCharacter)
  
  for (n in 1:4){
    
    # Initialize variables: Qs, the expected values
    Data[[paste("P", n, sep="")]]<-NA
    
    # Ps (probabilities for each category's choice)
    Data[[paste("V", n, sep="")]]<-NA
    
  }
  
  # probability for the choice that participants' made on a trial
  Data$Prob<-NA
  
  # Delta, prediction error
  Data$Delta<-NA
  
  # participants' response
  Data$response<-NA

  # change point probability
  Data$CPP<-NA
  
  # index variables for Q, P, and Delta
  Vindex<-c("V1", "V2", "V3", "V4")
  Pindex<-c("P1", "P2", "P3", "P4") 
  Deltaindex<-c("Delta1", "Delta2", "Delta3", "Delta4")
  
  # Counter for indicating which character has to be updated
  count<-rep(0, 2)
  
  # initialise choice probability and counter for the choiceprobability
  prob<-NA
  
  # loop over trials
  for (t in 1: nrow(Data)){
    
    # update the counter
    Murkcounter<-which(murks==Data$cuedCharacter[t])
    
    # The following loop retrieves the Q values of the butterfly that 
    # corresponds to the current trial (time t).
    if (count[Murkcounter]==0){
      V<-rep(initialV, 4) # if it is the first time that butterfly is shown,
      # the Vs are at their initial value
    } else{
      V<-Data[Data$cuedCharacter==Data$cuedCharacter[t],][count[Murkcounter],
                                                          Vindex] # if it is not
      # the first time that butterfly is shown, retrieve the Qs of the last
      # trial of that butterfly
    }
    
    count[Murkcounter]<-count[Murkcounter]+1 # update the counter
    
    # update choice probabilities using the softmax distribution
    p<-softmax(V, beta)
    
    # make choice according to choice probabilities
    Data$response[t] <- chooseMultinom(p)
    
    # now map the response into the current trial categories
    Data$respCat[t]<-as.character(categ[Data$response[t]])
    
    #which cat is the corr ans?
    corr_resp<-as.character(Data$obj_category[t])
    
    # get accuracy
    if (Data$respCat[t]==corr_resp){
      Data$accuracy[t]<-1
    }else{
      Data$accuracy[t]<-0
    }
    
    # get the r
    if (Data$accuracy[t] == 1){
    if (Data$obj_category[t]==categ[1]){
      r<-c(1,0,0,0)
    } else if (Data$obj_category[t]==categ[2]){
      r<-c(0,1,0,0)
    } else if (Data$obj_category[t]==categ[3]){
      r<-c(0,0,1,0)
    } else if (Data$obj_category[t]==categ[4]){
      r<-c(0,0,0,1)
    }
    } else {
      r<-c(0,0,0,0)
    }
    
    # update values
    updateVal<-update_RW(r = r, V = V, alpha = alpha)
    
    # prediction error
    delta <- updateVal$delta
    
    # assign it to the dataset
    Data[t, Deltaindex]<-delta
    
    # update V
    V<-updateVal$V  
    
    # assign values to the dataset
    Data[t, Vindex]<-V
    Data[t, Pindex]<-p
    
  }
  
  return(Data)
  
}

