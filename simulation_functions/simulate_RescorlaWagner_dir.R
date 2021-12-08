simulate_RescorlaWagner_dir<-function ( Data,alpha, beta,  initialV){
  #----------------------------------------------------------------------------#
  # This function simulate participants' choices using dirichlet
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
    Data[[paste("Q", n, sep="")]]<-NA
    
    # Ps (probabilities for each category's choice)
    Data[[paste("P", n, sep="")]]<-NA
    
    Data[[paste("variance", n, sep="")]]<-NA
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
  xindex<-c("x1", "x2", "x3", "x4")  # this is the observation: x=1 indicates the x for the category that is presented on each trials. The other two have x = 0. 
  alphaindex<-c("alpha1", "alpha2", "alpha3", "alpha4") # the alphas here represent the pseudocounts for the categories at trial t. 
  Varianceindex<-c("variance1","variance2", "variance3", "variance4" ) # variance for each trial, for each category
  # Counter for indicating which character has to be updated
  count<-rep(0, 2)
  
  Data$uncertainty<-NA
  
  Data$variance<-NA
  
  # initialise choice probability and counter for the choiceprobability
  prob<-NA
  
  # loop over trials
  for (t in 1: nrow(Data)){
    
    # update the counter
    Murkcounter<-which(murks==Data$cuedCharacter[t])
    
    # calculate the variance
    # number of categories
    K<-length(unique(Data$obj_category))
    
    # pseudocounts up to that point, for that scene (add initial pseudocount)
    if (count[Murkcounter] ==0) { # if this is the first trial
      alpha_k<-c(1, 1, 1,1) # we are initialising them at 1
    } else {
      alpha_k<-base::colSums(Data[Data$cuedCharacter==Data$cuedCharacter[t], xindex ], na.rm=T)+c(1,1,1,1)# this is the initial alpha
    }
    
    # getting the variance (from Murphy, Machine Learning). We use the pseudocounts
    mean_k<-alpha_k/sum(alpha_k)
    alpha_0 = sum(alpha_k)
    var_k = (alpha_k*(alpha_0-alpha_k))/ (alpha_0^2 * (alpha_0+1))
    
    Data[t, alphaindex]<-alpha_k
    
    Data[t, Varianceindex]<-var_k
    
    Data$variance[t]<-sum(var_k)
    # The following loop retrieves the Q values of the butterfly that corresponds to the current trial (time t).
    if (count[Murkcounter]==0){
      V<-rep(initialV, 4) # if it is the first time that butterfly is shown, the Qs are at their initial value
    } else{
      V<-Data[Data$cuedCharacter==Data$cuedCharacter[t],][count[Murkcounter],Vindex] # if it is not the first time that butterfly is shown, retrieve the Qs of the last trial of that butterfly
    }
    
    count[Murkcounter]<-count[Murkcounter]+1 # update the counter
    
    # update choice probabilities using the softmax distribution
    p<-softmax(V, beta)
    
    # make choice according to choice probabilities
    Data$response[t] <- chooseMultinom(p)
    
    # now map the response into the current trial categories
    Data$response[t]<-which(as.character(unlist(Data[t, c("left_categ", "centleft_categ", 
                                                          "centright_categ", "right_categ")]) )
                            == paste0("stimuli/",categ[Data$response[t]], ".png"))
    
    # get the observation as 1 if that category is present, and 0 if it is not
    
    if (Data$obj_category[t]==categ[1]){
      Data[t,xindex]<-c(1,0,0,0)
    } else if (Data$obj_category[t]==categ[2]){
      Data[t,xindex]<-c(0,1,0,0)
    } else if (Data$obj_category[t]==categ[3]){
      Data[t,xindex]<-c(0,0,1,0)
    } else if (Data$obj_category[t]==categ[4]){
      Data[t,xindex]<-c(0,0,0,1)
    }
    
    # update the alpha index 
    Data[t,alphaindex]<-alpha_k+ Data[t,xindex]
    
    # uncertainty
    Data$uncertainty[t]<-as.numeric(1/Data[t, Varianceindex][which(Data[t,xindex]==1)])
    
    # learning rate throuhg sigmoid
    Data$lr[t]<-   1 / (1 + exp(- Data$uncertainty[t]))
    
    # update values
    updateVal<-update_RW(r = Data[t,xindex], V = V, alpha =  Data$lr[t])
    
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

