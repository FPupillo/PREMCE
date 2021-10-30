
taskSim<-function(
#------------------------------------------------------------------------------#
# function that simulates task
# created: Tue Oct 26 14:15:02 2021"
#  INPUT: Pcong = contingency for the most probably choice
#  Output: a dataset with the task structure 
#------------------------------------------------------------------------------#
# -----------------------------------------------------------------------------#
#                       Parameters
# -----------------------------------------------------------------------------#
  
  # set the reward probabilities for the contingencies
  Pcong=0.75 ,   # 75 for the conguent ones, the preferred category
  
  # Ntrial per block per character
  Ntrial=24,
  
  # N character
  Ncharacter=1,
  
  # switch point
  switchN=3,
  
  # how many categories? 
  categN=4
  
  
# -----------------------------------------------------------------------------#
){
  
  
  Pincong=(1-Pcong)/(categN-1) 
  
  characters<-c("stimuli/m2.png", "stimuli/m5.png")
  
  # select the categories 
  selCat<-c( "Outdoor activity & sport item", "Kitchen & utensil",
             "Electronic device & accessory", "Hand labour tool & accessory")
  
  categ<-selCat
  
  # create dataframe
  df<-as.data.frame(matrix(NA, ncol = 11, nrow = Ntrial*(switchN+1)))
  
  names(df)<-c("left_categ", "centleft_categ", "centright_categ", "right_categ", 
               "character", "switch_cond", "trial_cond", "trialN","opt_choice",
               "corr_ans", "obj_category")
  
  # put the categories
  for (n in 1:nrow(df)){
    df[n,1:4]<-categ
  }
  
  # create the switch condition
  df$switch_cond<-rep(c(2:(switchN+2)), each = Ntrial)
  
  # bind two characters if necessary
  if (Ncharacter==2){
    df<-rbind(df, df)
    df$character<-rep(c(characters) , each = Ntrial*(switchN+1))
  } else{
    df$character<-rep(characters[1], each = Ntrial*(switchN+1))
  }
  
  
  # create correct answer for them
  # the answer is drawn from the categories with the predefinite prob
  # correct category for one character
  corr_cat1<-seq(1:(switchN+1))
  # correct category for the other character
  corr_cat2<-rev(corr_cat1)
  
  # optimal choice for each switch num
  df$opt_choice<-NA
  
  for (n in 1: length(Ncharacter)){
    if (n==1){
      df$opt_choice<-rep(corr_cat1, each = Ntrial)
    } else if (n==2){
      df$opt_choice<-rep(corr_cat2, each = switchN)
    }
  }
  
  # create opt_choice_cat
  df$opt_choice_cat<-categ[df$opt_choice]
  
  # now for each switch sample the most probable category with certain frequency
  df$obj_category<-NA
  for (n in 1:nrow(df)){
    df$obj_category[n]<-sample(c(categ[df$opt_choice[n]], categ[-df$opt_choice[n]]), 1,
                               prob = c(Pcong, rep(Pincong, 3) ))
    if (df$obj_category[n]==df$opt_choice_cat[n]){
      df$trial_cond[n]<-1 }else{ df$trial_cond[n]<-0}
  }
  
  # get corr_ans
  for (n in 1:nrow(df)){
    df$corr_ans[n]<-which(as.character(df[n,1:4])==df$obj_category[n])
  }
  
  # trial Number
  df$trialN<-1:nrow(df)
  
  return(df)
  
}
