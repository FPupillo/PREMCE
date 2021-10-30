# data quality check
rm(list=ls())
library(dplyr)

# get the script directory
#path<-rstudioapi::getSourceEditorContext()$path

# delete the file name
#path<-substr(path, 1, nchar(path)-18)

# set wd 
#setwd(path)

# source functions
source("selPart.R")

# select the participant
part<-as.numeric(readline(prompt = "What participant"))

# subset only files for the participant
files<-list.files(pattern= glob2rx(paste(part, "*.csv", sep="")))

# select part 1
file<-files[grepl( "ses-01_part-01", files)]

day1<-read.csv(file[1])

# select practice 
practice<-day1[day1$switch_cond==0,]
# exclude NAs
practice<-practice[!is.na(practice$myownaccuracy),]
# how many trials?
PracticeNr<-nrow(practice)

print( paste("Participant ", part, " needed ", PracticeNr, " practice trials", sep=""))

# now cumulative accuracy 
# select task1
task1<-day1[day1$switch_cond==1,]

# exclude NAs
task1<-task1[!is.na(task1$task_resp.corr),]

task1$myownaccuracy<-as.numeric(as.character(task1$myownaccuracy))
accuracy1<-task1$myownaccuracy[-20]

#get cumulative accuracy task 1
cumAcc1<-mean(accuracy1)

print(paste("participant got ", cumAcc1*100, "% right in the first block", sep =""))

# now day two : contingencies
# select part 2
file2<-files[grepl( "ses-02_part-01", files)]
day2<-read.csv(file2)

# warmup file
warmup<-day2[day2$switch_cond==99,]
warmup<-warmup[!is.na(warmup$switch_cond),]

warmup$myownaccuracy<-as.numeric(as.character(warmup$myownaccuracy))
accuracyWarm<-mean(warmup$myownaccuracy)

print(paste("participant got ", round(accuracyWarm*100,2), "% right in the warmup block", sep =""))

# task 2
task2<-day2[day2$switch_cond<99,]
task2$myownaccuracy<-as.numeric(as.character(task2$myownaccuracy))
accuracyTask2<-mean(task2$myownaccuracy, na.rm=T)

print(paste("participant got ", round(accuracyTask2*100,2), "% right in the subsequent blocks", sep =""))

# recognition now
fileRecog<-files[grepl( "ses-02_part-02", files)]

recog<-read.csv(fileRecog)
recog <- select(recog,recog_resp.keys,recog_resp.rt,conf_resp.keys,conf_resp.rt,images,corr_ans, images)

# delete Na
recog<-recog[!is.na(recog$recog_resp.rt),]

# compute accuracy
recog$acc<-NA
for (n in 1 :nrow(recog)){
  if (recog$recog_resp.keys[n]==recog$corr_ans[n]){
    recog$acc[n]<-1
  }else{recog$acc[n]<-0} 
}

# calculate hit, miss, rej, and FA
for ( i in 1:nrow(recog)){
  if (recog$corr_ans[i]=="left"& recog$recog_resp.keys[i]=="left" ){
    recog$type[i]<-"HIT"
  } else if (recog$corr_ans[i]=="left"& recog$recog_resp.keys[i]=="right" ){
    recog$type[i]<-"Miss"
  } else if(recog$corr_ans[i]=="right"& recog$recog_resp.keys[i]=="right" ){
    recog$type[i]<-"CorrRej"
  } else if (recog$corr_ans[i]=="right"& recog$recog_resp.keys[i]=="left" ){
    recog$type[i]<-"FA"
  }
}

# wide dataset
wideData<-table(recog[,"type"])
# 
# # convert it to a data.frame
# wideData<-as.data.frame.matrix(wideData)
# 
# # get rid of the rownames
# rownames(wideData) <- c()

# compute percentage HIT
HITrate<-wideData[3]/(wideData[3]+wideData[4])

# percentage false alarm
FArate<-wideData[2]/(wideData[2]+wideData[1])

print(paste("participant scored", HITrate, "% Hit rate and", FArate, "% False Alarm rate"))
