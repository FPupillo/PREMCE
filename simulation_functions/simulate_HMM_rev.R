simulate_HMM_rev<-function ( Data,c,gamma, initialPs){
  # --------------------------------------------------------------------------#
  # adapted after revision from Lei Zhang
  # This function computes the likelihood of the participants'
  # choices conditional on Hidden Markov model
  #
  # Input
  #   Data: data containing the structure of the task
  #   beta:  beta parameter
  #   c : probability that the reward indicated that one state is the true state
  #   gamma: transition probability
  #   initialPs: value of the inital Probabilities
  #
  # Output:
  #   dastaframe with $response and $object_cat
  # -------------
  
  # convert the object category into numeric variable
  categ<-c("Outdoor activity & sport item", "Kitchen & utensil",
           "Electronic device & accessory","Hand labour tool & accessory")
  
  murks<-levels(as.factor(Data$cuedCharacter))
  
  Data$cuedCharacter<-as.character(Data$cuedCharacter)
  
  for (n in 1:4){
    
    # P probability of the state, given the observation (reward)
    Data[[paste("P_O_S", n, sep="")]]<-NA
    
  }
  
  # probability of the states, prior and posterior
  PS_pre<-rep(initialPs, 4)
  PS_post<-rep(initialPs, 4)
  
  # probability for the choice that participants' made on a trial
  Data$Prob<-NA
  
  # participants' response
  Data$response<-NA
  
  # participants' response categor
  Data$respCat<-NA
  
  # accuracy
  Data$accuracy<-NA
  
  # learning rate
  Data$alpha<-NA
  
  # surprise
  Data$surprise<-NA
  
  # entropy
  Data$entropy<-NA
  
  # index variables for StatesPre, P, and Delta
  PSpreindex<-c("PS_pre1", "PS_pre2", "PS_pre3", "PS_pre4")
  PSpostindex<-c("PS_post1", "PS_post2", "PS_post3", "PS_post4") 
  
  # Counter for indicating which character has to be updated
  count<-rep(0, 2)
  
  # initialise choice probability and counter for the choiceprobability
  prob<-NA
  
  # loop over trials
  for (t in 1: nrow(Data) ){ 
    
    # update the counter
    Murkcounter<-which(murks==Data$cuedCharacter[t])
    
    # The following loop retrieves the Q values of the butterfly that corresponds to the current trial (time t).
    if (count[Murkcounter]==0){
      
      PS_pre<-rep(0.25,4)

    } else {

      PS_post<-unlist(Data[Data$cuedCharacter==Data$cuedCharacter[t],][count[Murkcounter],PSpostindex])
      
      PS_pre[1]<-PS_post[1]*(1-(3*gamma))+PS_post[2]*(gamma)+PS_post[3]*(gamma)+PS_post[4]*(gamma)
      PS_pre[2]<-PS_post[1]*(gamma)+PS_post[2]*(1-3*gamma)+PS_post[3]*(gamma)+PS_post[4]*(gamma)
      PS_pre[3]<-PS_post[1]*(gamma)+PS_post[2]*(gamma)+PS_post[3]*(1-3*gamma)+PS_post[4]*(gamma)
      PS_pre[4]<-PS_post[1]*(gamma)+PS_post[2]*(gamma)+PS_post[3]*(gamma)+PS_post[4]*(1-3*gamma)
    
    }

    
    
    count[Murkcounter]<-count[Murkcounter]+1 # update the counter
    
    # update choice probabilities using the softmax distribution
    #p<-softmax(PS_pre, beta)
    
    # make choice according to choice probabilities
    Data$response[t] <- chooseMultinom(PS_pre)
    
    # map response onto the keys
    # which category was the response?
    # estract the order of the categories at trial t
    Data$respCat[t]<-as.character(categ[Data$response[t]])
    
    # which cat is the corr ans?
    corr_resp<-as.character(Data$obj_category[t])
    
    # get accuracy
    if (Data$respCat[t]==corr_resp){
      Data$accuracy[t]<-1
    }else{
      Data$accuracy[t]<-0
    }
    
    # get the observation as 1 if that category is present, and 0 if it is not
    if (Data$accuracy[t]==1){
      r<-1 
    } else {
      r<-0
    }
    
    # now renew the probability: p(O|S1) p(O|S2) p(O|S4) p(O|S5)
    # O is a pair between A(ction) and R(eward)
    # --> the probability of actually observing this outcome
    
    if (r == 1) {
      P_O_S1 =  0.25 * ( ifelse(Data$response[t] == 1, c, (1-c)/3))
      P_O_S2 =   0.25 *( ifelse(Data$response[t] == 2, c, (1-c)/3))
      P_O_S3 = 0.25 *( ifelse(Data$response[t] == 3, c, (1-c)/3))
      P_O_S4 =   0.25 *( ifelse(Data$response[t] == 4, c, (1-c)/3))
      
    } else if (r== 0) {
      P_O_S1 = 0.25 * ( ifelse(Data$response[t] == 1, 1-c, c/3))
      P_O_S2 =  0.25 *( ifelse(Data$response[t] == 2, 1-c, c/3))
      P_O_S3 =  0.25 * ( ifelse(Data$response[t] == 3, 1-c, c/3))
      P_O_S4 =  0.25 * ( ifelse(Data$response[t] == 4, 1-c, c/3))
    }
    
    # State belief update using Bayesian rule, after observing the outcome
    PS_post[1] = (P_O_S1 * PS_pre[1]) / ( P_O_S1 * PS_pre[1] + P_O_S2 * PS_pre[2] + P_O_S3 * PS_pre[3] + P_O_S4 * PS_pre[4])
    PS_post[2] = (P_O_S2 * PS_pre[2]) / ( P_O_S1 * PS_pre[1] + P_O_S2 * PS_pre[2] + P_O_S3 * PS_pre[3] + P_O_S4 * PS_pre[4])
    PS_post[3] = (P_O_S3 * PS_pre[3]) / ( P_O_S1 * PS_pre[1] + P_O_S2 * PS_pre[2] + P_O_S3 * PS_pre[3] + P_O_S4 * PS_pre[4])
    PS_post[4] = (P_O_S4 * PS_pre[4]) / ( P_O_S1 * PS_pre[1] + P_O_S2 * PS_pre[2] + P_O_S3 * PS_pre[3] + P_O_S4 * PS_pre[4])
    
    # / compute entropy
    # if (PS_post[1] == 0 | PS_post[1] == 1){
    #  Data$entropy[t] = 0
    #} else {
    Data$entropy[t] = -((PS_post[1]*log(PS_post[1]) + PS_post[2]*log(PS_post[2]) + PS_post[3]*log(PS_post[3]) + PS_post[4]*log(PS_post[4]) ))
    #}
    
    # compute bayesian surprise
    # else  
    Data$surprise[t] = PS_post[1] * log(PS_post[1]/PS_pre[1]) + PS_post[2] * log(PS_post[2]/PS_pre[2]) +
      PS_post[3] * log(PS_post[3]/PS_pre[3]) + PS_post[4] * log(PS_post[4]/PS_pre[4])  ; 
    
    # assign other values to the dataset
    Data[t, PSpreindex]<-PS_pre
    Data[t, PSpostindex]<-PS_post
    
    # convert the choice into the order of the categories
    Data$response[t]<-which(unlist(Data[t,c("left_categ","centleft_categ" , 
                                            "centright_categ","right_categ") ])== 
                              paste0("stimuli/", Data$respCat[t],  ".png"))
    
  }
  
  return(Data)
  
}

