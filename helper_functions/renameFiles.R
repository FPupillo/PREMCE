# -------------------------------------------------------------------------------------------------#
# script that rename the files for participants whose number is less than 10
# and copy all the files in the "data_files" folder 
# -------------------------------------------------------------------------------------------------#

# set the working directory
cd<-getwd()
setwd("raw_data")

# set the destination folder
destfd<-("/data_files/")
files<-list.files(pattern=".csv$")

for (i in 1:length(files)){
  # check if the participant number is less than 10
  partic<-sub("\\_.*", "", files[i])
  if (as.numeric(partic) <10){
    #filename<-paste(cd, destfd, files[i], sep="")
    file.copy(files[i], paste(cd, destfd, "0", partic,
                              substr(files[i], 2, nchar(files[i])) , sep=""), overwrite = F)
  } else{
    file.copy(files[i], paste(cd, destfd,  partic,
                              substr(files[i], 3, nchar(files[i])) , sep=""), overwrite = F)
    
  }
}
