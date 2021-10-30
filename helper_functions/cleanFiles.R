# -------------------------------------------------------------------------------------------------#
# function that clean the files and select only the variables of interest
#--------------------------------------------------------------------------------------------------#

# clear the workspace first
rm(list=ls())

# retrieve the function to select the files
source("helper_functions/selPart.R")

# retrieve one sample dataset for each part
# sel files
cd<-getwd()
setwd("data_files")
files<-selPart(1)
setwd(cd)

# second participant as sample
part1<-read.csv(paste("data_files/", files[2], sep=""))

# check the names of the variables
variables<-names(part1) 

# select the variables of Interest (VoI)
VoI<-c("participant","pract_resp.keys","pract_resp.corr" , "pract_resp.rt",                    
"first_character" ,"second_character", "Cue",  "left_categ",       
 "centleft_categ","centright_categ" ,"right_categ" , "corr_ans" ,           
    "image",    "trial_cond"  ,"cuedCharacter", "task_resp.keys","task_resp.corr", 
"task_resp.rt"  )

# now clean the datafiles and put them in the new folder (data_files_clean)
for (j in 1: length(files)){
  tryCatch({
  # get file name
  filename<-paste( files[j], sep="")
  part1<-read.csv(paste("data_files/", filename, sep=""))
  part1<-part1[,VoI]
  write.csv(part1, paste("data_files_clean/",filename, sep=""), row.names = F)
  }, error = function(e) { print(paste("problem with number", j))}) 
}

# now do for part 2
# sel files
cd<-getwd()
setwd("data_files")
files<-selPart(2)
setwd(cd)

# first participant as sample
part1<-read.csv(paste("data_files/", files[1], sep=""))

# check the names of the variables
variables<-names(part1) 

VoI<-c("participant" , "first_character" , "second_character"  ,"warmup_resp.keys",
       "warmup_resp.corr" ,"warmup_resp.rt" , "Cue" ,  "left_categ" ,  "centleft_categ",
       "centright_categ" ,"right_categ" ,  "corr_ans", "obj_category", "image" , 
       "trial_cond" , "switch_cond")

for (j in 1: length(files)){
  # get file name
  filename<-paste( files[j], sep="")
  part1<-read.csv(paste("data_files/", filename, sep=""))
  part1<-part1[,VoI]
  write.csv(part1, paste("data_files_clean/",filename, sep=""), row.names = F)
}

# now do for part 3
# sel files
cd<-getwd()
setwd("data_files")
files<-selPart(3)
setwd(cd)

# first participant as sample
part1<-read.csv(paste("data_files/", files[1], sep=""))

# check the names of the variables
variables<-names(part1) 

VoI<-c("participant"    , "recog_resp.keys"         ,    
 "recog_resp.rt" ,  "conf_resp.keys"  , "conf_resp.rt" , 
 "resp_assoc.keys", "resp_assoc.rt",
  "images",   "corr_ans","type"   )

for (j in 1: length(files)){
  # get file name
  filename<-paste( files[j], sep="")
  part1<-read.csv(paste("data_files/", filename, sep=""))
  part1<-part1[,VoI]
  write.csv(part1, paste("data_files_clean/",filename, sep=""), row.names = F)
}
