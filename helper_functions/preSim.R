preSim<-function(Data){
  # ---------------------------------------------------------------------------#
  # function that preprocess the simulated data
  # INPUT : Data = simulated data
  # OUTPUT: Data with the preprocessed variables
  # ---------------------------------------------------------------------------#
 
  # first, get the category that relates to participants' responses
  Data$respCat<-NA
  for (n in 1:nrow(Data)){
    
  selCat<-getCat(Data$response[n])
  Data$respCat[n]<-substr(Data[n,selCat], 9, nchar(as.character(Data[n,selCat]))-4)
  
  
  }
  
  # turn it into factor
  Data$respCat<-as.factor(Data$respCat)
  
  # transform the category variables from factors to characters
  catVar<-c("left_categ", "centleft_categ","centright_categ", "right_categ" )
  
  for (c in catVar){
    Data[[c]]<-as.character(Data[[c]])
  }
  
  # get the correct category
  Data<-getcorrCat(Data)
  
  # rename it
  Data$corrCat<-Data$corr_category ; Data$corr_category<-NULL
  
  # turn it into a factor
  Data$corrCat<-as.factor(Data$corrCat)
  
  # substring datacharacter
  Data$character<-substr(Data$cuedCharacter, 9,10)
  
  # create trialNum
  Data$trialNum<-1:nrow(Data)
  
  return(Data)
}