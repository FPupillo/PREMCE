simulate_HMM_dual<-function ( Data,c,gamma, initialPs){
  # ---------------------------------------------------------------------------#
  # This function simulates participants' choice
  # through HMM agent, in a task where there are only two choices
  #
  # Input
  #   Data: data containing the structure of the task
  #   c : probability that the reward indicated that one state is the true state
  #   gamma: transition probability
  #   initialPs: value of the inital Probabilities
  #
  # Output:
  #   dataframe with $response and $object_cat
  #
  # created by: Francesco Pupillo, Goethe University Frankfurt
  # date: "Wed Dec  8 18:23:59 2021"
  # ---------------------------------------------------------------------------#
  
  # categories that the characters choose
  categ<-c("Outdoor activity & sport item", "Kitchen & utensil")
  
  # characters (called "murkse")
  murks<-levels(as.factor(Data$cuedCharacter))
  
  Data$cuedCharacter<-as.character(Data$cuedCharacter)
  
  # initialize variables
  for (n in 1:2){
    
    # P probability of the state, given the observation (reward)
    Data[[paste("P_O_S", n, sep="")]]<-NA
    
  }
  
  # probability of the states, prior and posterior
  PS_pre<-rep(initialPs, 2)
  PS_post<-rep(initialPs, 2)
  
  # probability for the choice that participants' made on a trial
  Data$Prob<-NA
  
  # participants' response
  Data$response<-NA
  
  # participants' response category
  Data$respCat<-NA
  
  # accuracy
  Data$accuracy<-NA
  
  # surprise
  Data$surprise<-NA
  
  # entropy
  Data$entropy<-NA
  
  # index variables for States Pre, Post
  PSpreindex<-c("PS_pre1", "PS_pre2")
  PSpostindex<-c("PS_post1", "PS_post2") 
  
  # Counter for indicating which character has to be updated
  count<-rep(0, 2)
  
  # initialise choice probability and counter for the choice probability
  prob<-NA
  
  # loop over trials
  for (t in 1: nrow(Data) ){ 
    
    # update the counter
    Murkcounter<-which(murks==Data$cuedCharacter[t])
    
    if (count[Murkcounter]==0){ # if it is the first trial
      
      PS_pre<-rep(0.50,2)

    } else { # if not, retrieve the posterior for that character

      PS_post<-unlist(Data[Data$cuedCharacter==Data$cuedCharacter[t],][count[Murkcounter],PSpostindex])
      
      PS_pre[1]<-PS_post[1]*(1-gamma)+PS_post[2]*(gamma)
      PS_pre[2]<-1 - PS_pre[1]
      
    }

    count[Murkcounter]<-count[Murkcounter]+1 # update the counter
    
    # make choice according to choice probabilities
    if (categN==2){
      Data$response[t] <- chooseBinomial(PS_pre)
    }else{
    Data$response[t] <- chooseMultinom(PS_pre)
    }
    
    # map response onto the keys
    # which category was the response?
    # extract the order of the categories at trial t
    Data$respCat[t]<-as.character(categ[Data$response[t]])
    
    # which cat is the corr ans?
    corr_resp<-as.character(Data$obj_category[t])
    
    # get accuracy
    if (Data$respCat[t]==corr_resp){
      Data$accuracy[t]<-1
      r<-1
    }else{
      Data$accuracy[t]<-0
      r<-0
    }
    
    # now renew the probability: p(O|S1) p(O|S2) 
    # O is a pair between A(ction) and R(eward)
    # --> the probability of actually observing this outcome
    
    if (r == 1) {
      P_O_S1 = 0.50 * ( ifelse(Data$response[t] == 1, c, 1-c))
      P_O_S2 = 0.50 * ( ifelse(Data$response[t] == 2, c, 1-c))

      
    } else if (r== 0) {
      P_O_S1 = 0.50 * ( ifelse(Data$response[t] == 1, 1-c, c))
      P_O_S2 = 0.50 * ( ifelse(Data$response[t] == 2, 1-c, c))
    }
    
    # State belief update using Bayesian rule, after observing the outcome
    PS_post[1] = (P_O_S1 * PS_pre[1]) / ( P_O_S1 * PS_pre[1] + P_O_S2 * PS_pre[2]) 
    PS_post[2] = 1-PS_post[1]

    Data$entropy[t] = -(PS_post[1]*log(PS_post[1]) + PS_post[2]*log(PS_post[2]))
    
    # compute bayesian surprise
    # else  
    Data$surprise[t] = PS_post[1] * log(PS_post[1]/PS_pre[1]) + PS_post[2] *
      log(PS_post[2]/PS_pre[2]) 
    
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

