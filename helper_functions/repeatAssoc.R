repeatAssoc<-function(Data){
# -------------------------------------------------------------------------------------------------#
# Function that repeats the name of the image in recognition trials
# where the answer was "left" so that participants were asked to do the association task
# in this task, the image is represented in the row immediately after, related to the association
# task
#
# -------------------------------------------------------------------------------------------------#

for (n in 1:nrow(Data)){
 if (Data$recog_resp.keys[n]=="left"){
   # get the image name on the task immediately after
   imname<-Data$images[n+1]
   Data$images[n]<-as.character(imname)
   # now the correct answer
   Data$corr_ans[n]<-Data$corr_ans[n+1]
   # now the type
   Data$type[n]<-Data$type[n+1]
   # now the order
   Data$order[n]<-Data$order[n+1]
   
 }
}
  
return(Data)
}