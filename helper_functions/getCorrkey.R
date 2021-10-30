getCorrkey<-function(data){
  #------------------------------------------------------------------------------#
  # function that returns the correct response
  # on each trial
  #------------------------------------------------------------------------------#
  data$corr_ans<-NA
  for (n in 1:nrow(data)){
    # subset the categories on those trial
    cat<-c(  "left_categ","centleft_categ",         
             "centright_categ", "right_categ")
    data$corr_ans[n]<-which(data[n, cat] == data$corrCat[n] )
  }
  
  return(data)
}