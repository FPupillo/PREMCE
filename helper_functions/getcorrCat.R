

getcorrCat<-function(data){
  # this function takes the data, look for the correct category on each trail
  # and returns the dataset with a new column represeting the correct category
  data$corr_category<-NA
  
  for (n in 1:nrow(data)){
    
    if (data$corr_ans[n]=="1"){
      data$corr_category[n]<- data$left_categ[n]
      # substring it
      data$corr_category[n]<-substr(data$corr_category[n], 9, nchar(data$corr_category[n])-4)
      
    }else if (data$corr_ans[n]=="2") {
      data$corr_category[n]<- data$centleft_categ[n]
      # substring it
      data$corr_category[n]<-substr(data$corr_category[n], 9, nchar(data$corr_category[n])-4)
      
    } else if (data$corr_ans[n]=="3") {
      
      data$corr_category[n]<- data$centright_categ[n]
      # substring it
      data$corr_category[n]<-substr(data$corr_category[n], 9, nchar(data$corr_category[n])-4)
      
    } else if (data$corr_ans[n]=="4") {
      
      data$corr_category[n]<- data$right_categ[n]
      # substring it
      data$corr_category[n]<-substr(data$corr_category[n], 9, nchar(data$corr_category[n])-4)
      
    }
    
  }
  
  return(data)
}
