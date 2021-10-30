# function that returns a matrix calle "x" with the response dummy coded
# it is used in the rescorla wagner formulas to obtain the feedback (observation)

getobs<-function(Data){
  
obs<-matrix(NA, nrow=nrow(Data), ncol=4)

for (t in 1:nrow(Data)){
  
if (Data$corrCat[t]==1){
  obs[t,]<-c(1,0,0,0)
} else if (Data$corrCat[t]==2){
  obs[t,]<-c(0,1,0,0)
} else if (Data$corrCat[t]==3){
  obs[t,]<-c(0,0,1,0)
}else if (Data$corrCat[t]==4){
  obs[t,]<-c(0,0,0,1)
}
  
}

return(obs)
}
