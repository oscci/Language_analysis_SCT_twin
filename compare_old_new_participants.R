#Script to check which participants took part in 2011 study.
#NB Original data stored securely on network drive.
# Temporarily moved to computer for this comparison, but must be deleted immediately.

sct.old <- read.csv("participant info 2011.csv",stringsAsFactors=FALSE)
sct.new <- read.csv("SCT response forms new.csv",stringsAsFactors=FALSE)
sct.new$Family.Code[71]<-'001' #2 kids in family, initially 001A and B, recoded to 0 and 1


redcap.dir <-"~/Dropbox/ERCadvanced/project SCT analysis/Data from Redcap/"
#Use latest version
sct.redcap <- 'SCTDATA_DATA_2018-08-06_0957.csv'
sct.data <- read.csv(paste0(redcap.dir,sct.redcap),stringsAsFactors = FALSE)

sct.cases<-sct.data$record_id

#First find the name and dob for the current cases

newlist<-as.numeric(sct.new$Family.Code)

tempcheck<-data.frame(sct.data$record_id)
tempcheck$row<-NA
#Do these one at a time in a loop. Inefficient but easier to check

for (i in 1:length(sct.cases)){
  m<-match(sct.cases[i],newlist)
  parent<-sct.new$Parent.surname[m]
  child<-sct.new$Child.name[m]
  dob<-sct.new$Date.of.birth[m]
  
  #may be mistyping etc, or multiple entries, so look for any of these
  p<-match(parent,sct.old$Parent.surname)
  c<-match(child,sct.old$Child.name)
  d<-match(dob,sct.old$Date.of.birth)
  

  try1<-intersect(p,c)
  try2<-intersect(p,d)
  try3<-intersect(c,d)
  if(length(try1)>0){tempcheck$row[i]<-try1}
  if(length(try2)>0){tempcheck$row[i]<-try2}
  if(length(try3)>0){tempcheck$row[i]<-try3}
  }
  
  
