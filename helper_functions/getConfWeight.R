#---------------------------------------------------------------------------------------------#
# function that computes confidence-weighted scores as in Jang et al. (2018)
# created by Francesco Pupillo
# Goethe University Frankfurt
# "Wed Jun  9 14:37:41 2021"
#---------------------------------------------------------------------------------------------#

getConfWeight<-function(df){
  #
  # INPUT : df = dataframe
  # 
  # OUTPUT: dataframe with confidence weight for 
  #         each trial
  # --------------------------------------------

df$confWrec<-NA
# loop through the trials
for (n in 1: nrow(df)){
  if (!is.na(df$recogAcc[n])){
  # select only the old ones
  #if (df$OvsN[n] == 1){
    # if participants select "old" in "old" trials
    if (df$recogAcc[n]==1){
      # their score is proportional to confidence, starting
      # from 5
      df$confWrec[n] <- 4+df$confidence[n]
      # if participant select "new"
    } else{
      # we subtract confidence from 5s
      df$confWrec[n] <- 5-df$confidence[n]
    }
  }
}

return(df)

}