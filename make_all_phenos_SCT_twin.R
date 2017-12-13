#Make all three phenotypes for both twin and SCT samples.

#DVM Bishop 12 Dec 2017

#install.packages(c("data.table","scales","lavaan"))
library(devtools) # to run script from gist and bring in data
library(lavaan)
library(semPlot)
library(knitr)
library(ggplot2)
library(reshape2)
library(psych)
require(tidyverse)
library(reshape2)

#Load all data from redcap
mydir <- "~/Dropbox/ERCadvanced/project SCT analysis/data from redcap/"
sctfile <- 'SCTData_DATA_2017-12-13_1155.csv'
twinfile<-'TwinsData_DATA_2017-12-13_0852.csv'

#read in the SCT data, and remove one case
sct.data<-read.csv(paste0(mydir,sctfile))
w<-which(sct.data$trisomy>8) #isochromosome case to be removed
sct.data<-sct.data[-w,] #remove one record
#------------------------------------------------------------------------------------------
#read in the twin data, and categorise the PAIR according to parent language concerns
#------------------------------------------------------------------------------------------
twin.data<-read.csv(paste0(mydir,twinfile))
twin.data$why_tested<-0 #default is control: neither twin meets criteria
#Loop is very clunky way to do this, but brain defeated
for (i in 1:length(twin.data)){
  myf<-twin.data$fam_id[i]
  w<-which(twin.data$fam_id==myf)
  ww<-which(twin.data$splang_conc[w]>0)
  if(length(ww)>0){
    twin.data$why_tested[i]<-1 #1 if either twin in family meets criteria
  }
}
#NB WHy tested has different significance for twins and SCTs.
#For twins it indicates if they were recruited because of potential language problem in one or both twins
#For scts, it indicates whether the sct was discovered because of neurodev/behav problems

#----------------------------------------------------------------------------------------
#Create partial testing variable for twins (this has now been stored on Redcap so we read it in)
# This part of script preserved to show how it is made
#----------------------------------------------------------------------------------------
#this is 3 digit code. 1st digit is 1 for all battery done, 2 for partial, 9 for none
#digit 2 is 1 if SRS done and 0 otherwise; digit 3 is 1 if DAWBA done and 0 otherwise
# for (i in 1:nrow(short.twin)){
#   dig1<-1
#   w<-which(short.twin[i,3:16]>997)
#   if(length(w)>0){dig1<-2} #not all tests done
#   if(length(w)>13){dig1<-9} #no tests done
#   dig2<-1
#   if(is.na(short.twin$srs_t_score[i])){dig2<-0} #srs not done
#   dig3<-1
#   if(is.na(short.twin$asd_dsm_r1[i])){dig3<-0} #dawba not done
#   short.twin$partial_testing[i]<-100*dig1+10*dig2+dig3
# }
# mybit<-select(short.twin,record_id,partial_testing)
# write.table(mybit, 'parttest.csv', sep=",",row.names=FALSE) 
 
#----------------------------------------------------------------------------------------
# Create big file with sct and twin groups and relevant data columns
#------------------------------------------------------------------------------------------
short.sct<-dplyr::select(sct.data,record_id,age_at_test,wasi_matrices_ss,wasi_block_design_ss,wasi_vocab_ss,
                         wdck_jhsn_ss,sent_rep_ss,nonword_rep_ss,oromotor_ss,nara_acc_ss,nara_comp_ss,nara_rate_ss,
                         towre_words_ss,towre_nonwords_ss,phab_pic_ss,phab_digit_ss,gcc,why_tested,
                         srs_t_score,slt,schooling,piq,asd_dsm_r1,adhd_comb_dsm_r1,
                         adhd_hyp_dsm_r1,adhd_inatt_dsm_r1,conduct_dsm_r1,splang_conc,partial_testing)
short.twin<-dplyr::select(twin.data,record_id,age_at_test,wasi_matrices_ss,wasi_block_design_ss,wasi_vocab_ss,
                          wdck_jhsn_ss,sent_rep_ss,nonword_rep_ss,oromotor_ss,nara_acc_ss,nara_comp_ss,nara_rate_ss,
                          towre_words_ss,towre_nonwords_ss,phab_pic_ss,phab_digit_ss,gcc,why_tested,
                          srs_t_score,slt,schooling,piq,asd_dsm_r1,adhd_comb_dsm_r1,
                          adhd_hyp_dsm_r1,adhd_inatt_dsm_r1,conduct_dsm_r1,splang_conc,partial_testing)
short.sct$source<-'sct'
short.sct$allgroup<-3 #default is no-bias SCT group,code 3
w<-c(which(short.sct$why_tested==3),which(short.sct$why_tested==2))
short.sct$allgroup[w]<-4 #cases tested because of behaviour or neurodev concerns

short.twin$source<-'twin'
short.twin$allgroup<-1+short.twin$why_tested #allgroup: control twin=1; DLD (parent report) twin =2

names(short.sct)[1]<-"record_id"

all.data<-rbind(short.sct,short.twin)
all.data$source<-as.factor(all.data$source)
all.data$record_id<-as.factor(all.data$record_id)
#NB asd_dsm code is 2 for ASD but 1 for 'uncertain' and 4 for 'other'. Need to recode 4 to 1
w<-which(all.data$asd_dsm_r1==4)
all.data$asd_dsm_r1[w]<-1

sctrange<-1:nrow(sct.data)
nrows<-nrow(all.data)
twinrange<-(nrow(sct.data)+1):nrows
#----------------------------------------------------------------------------------------------
# Create measure of global disability that considers all available data
#----------------------------------------------------------------------------------------------
#Will try now to get global score by adding as follows:
# History of speech problems = 1
# Current help in mainstream school (suport or special class or SLT) =1
# Special school = 2
# Dyslexia (testing, unless no data , in which case report from parent interview) = 1
# DLD (testing, unless no data , in which case report from parent interview) = 1
# ADHD (report or DAWBA) = 1
# behaviour problems (DAWBA or clear description on interview) = 1
# Autistic features: report from interview of definite diagnosis, SRS =90,  DAWBA = 2
# low IQ (PIQ < 70 or refusal/inability to do battery - with exception of reading tests) = 1

# NB coding for slt is:
#0, never ; 1, preschool only; 2, beyond 4 yr; 3, ongoing; 8, assessed only; 9, no information

# Neurodev diagnosis codes:
# 0 none; others coded as all applicable from list:
#  1 ADHD, 2 APD, 3 ASD, 4 behav, 5 dyscalc, 6 dyslexia, 7 dyspraxia, 8  DLD/SLI/LD, 9 ID/GDD

#Lang dis from test scores; computed below
#Dyslexia from test scors : computed below

#lang_concerns from parental report:
#0, never; 1, past; 2, continuing mild; 3, continuing severe; 9, unclear

#SLT from parent report: 0, never; 1, preschool only; 2, beyond 4 yr; 3, ongoing; 8, assessed only; 9, no information

# School code
#1, mainstream no help; 2, mainstream with help; 3, special class/unit; 4, special school; 5, home schooled; 8, other; 9, d

#--------------------------------------------------------------------------------------------------
# First create DLD and dyslexia categories on basis of language tests
#--------------------------------------------------------------------------------------------------
all.data$Nlowlang<-0 #Initialise counters to zero
all.data$Nlowread<-0 
all.data$langdone<-8 #assume all done but then subtract if not
all.data$readdone<-5

langtests<-c('wasi_vocab_ss','wdck_jhsn_ss','sent_rep_ss','nonword_rep_ss','oromotor_ss',
             'phab_pic_ss','phab_digit_ss','gcc')
langcuts<-c(40,85,7,7,7,85,85,55)
readtests<-c('towre_words_ss','towre_nonwords_ss','nara_acc_ss','nara_comp_ss','nara_rate_ss')
readcuts<-c(85,85,85,85,85)
#Count how many language tests done and how many below cutoff
# NB missing data codes: 999 general, 998 maladminstration, 997 refusal, 996 too young/old for norms
for (i in 1:length(langtests)){
  thiscol<-which(colnames(all.data)==langtests[i])
  myplus<-c(which(all.data[,thiscol]<langcuts[i]),which(all.data[,thiscol]==997))
  all.data$Nlowlang[myplus]<-all.data$Nlowlang[myplus]+1
  mymiss<-c(which(is.na(all.data[,thiscol])),which(all.data[,thiscol]>997))
  all.data$langdone[mymiss]<-all.data$langdone[mymiss]-1
}
#Count how many reading tests done and how many below cutoff
for (i in 1:length(readtests)){
  thiscol<-which(colnames(all.data)==readtests[i])
  myplus<-c(which(all.data[,thiscol]<readcuts[i]),which(all.data[,thiscol]==997))
  all.data$Nlowread[myplus]<-all.data$Nlowread[myplus]+1
  mymiss<-c(which(is.na(all.data[,thiscol])),which(all.data[,thiscol]>997))
  #NB for reading tests treat refusal as failure!
  all.data$readdone[mymiss]<-all.data$readdone[mymiss]-1
}

#Create DLD category ;
# For Yes (2) need at least 2 language tests more than 1 SD below age level, including WJ (ignores reading tests) and IQ > 70
# For subthreshold (1) same but does not require low WJ
all.data$lang_disorder<-0 #Initialise lang probs to zero
all.data$lang_disorder[all.data$Nlowlang>1]<-1
lowlangs<-which(all.data$lang_disorder==1)
langcases<-intersect(lowlangs,which(all.data$wdck_jhsn_ss<85))
all.data$lang_disorder[langcases]<-2
all.data$lang_disorder[all.data$langdone<3]<-9
#all.data$lang_disorder[all.data$piq<70]<-8

# Create dyslexia category
# 1 = Below 1 SD on two reading measures 
# Treat unable to attempt test as fail
all.data$dyslexia<-0 #Initialise read probs to zero
all.data$dyslexia[all.data$Nlowread>1]<-1
all.data$dyslexia[all.data$readdone<3]<-9
#all.data$dyslexia[all.data$piq<70]<-8 #include this if you want to categorise low IQ separately

#--------------------------------------------------------------------------------------------------
# Now build up global_neurodev measure
#--------------------------------------------------------------------------------------------------

all.data$global_neurodev<-0 #Initialise to zero
w<-which(is.na(all.data$srs_t_score)) #Recode NA to 999 for SRS
all.data$srs_t_score[w]<-999

temp<-all.data$global_neurodev
w<-unique(which(all.data$slt==1),which(all.data$slt==8)) #Cases with preschool SLT or assessed by SLT
all.data$global_neurodev[w]<-all.data$global_neurodev[w]+1

#Now code so can add one point for help in mainstream or ongoing SLT or in language unit
w1<-c(which(all.data$schooling==2),which(all.data$schooling==3)) #help in mainstream/lang unit

all.data$global_neurodev[w1]<-all.data$global_neurodev[w1]+1

#add 2 points if attending special school
w<-which(all.data$schooling==4)
all.data$global_neurodev[w]<-all.data$global_neurodev[w]+1

#add 1 point if PIQ < 70 or not completed
w<-which(all.data$piq<70)
w1<-which(all.data$piq>996)
w2<-which(all.data$partial_testing>199) #failed to complete test battery (not because of age)
allw<-unique(w,w1,w2)
all.data$global_neurodev[allw]<-all.data$global_neurodev[allw]+1

## Next bits done in a loop because criteria not captured in a single code

for (i in 1:nrows){
  
  # add 1 to code if meets language test criteria for dyslexia OR (if no data) has diagnosis of this 
  # reported on parent interview
  wd<-NA
  temp<-all.data$dyslexia[i] #coding according to test battery, 1 if dyslexic
  if(temp==9){
    wd<-unlist(gregexpr(pattern ='6',toString(all.data$neurodev_diag[i]))) #dyslexia code is 6
    #wd is one if 6 is included in neurodev_diag
  }
  if(length(wd)<1){temp=0}
  if (temp>0){all.data$global_neurodev[i]<-all.data$global_neurodev[i]+1}
  
  #Add 1 to code if evidence of ADHD on parental interview or DAWBA
  w2<-unlist(gregexpr(pattern ='1',toString(all.data$neurodev_diag[i]))) #ADHD code is 1
  if(w2==0){
    w2<-max(all.data$adhd_comb_dsm_r1[i],all.data$adhd_hyp_dsm_r1[i],all.data$adhd_inatt_dsm_r1[i])
  }
  if (w2>0){all.data$global_neurodev[i]<-all.data$global_neurodev[i]+1}
  
  # add 1 to code if meets language test criteria for lang_disorder OR (if no data) has diagnosis of this 
  # reported on parent interview
  
  temp<-all.data$lang_disorder[i] #coding according to test battery, 2 if with poor comp, 1 otherwise
  w1<-temp
  if(w1==2){w1<-1} #just one point added regardless of whether lang_dis code is 1 or 2
  if(temp==9){ #no data on language tests so use parent interview
    w1<-unlist(gregexpr(pattern ='8',toString(all.data$neurodev_diag[i]))) #DLD code is 8
    if(all.data$slt[i]==3){w1<-1} #regardless of diagnosis, ongoing SLT counts as DLD
    if(all.data$splang_conc[i]==3){w1<-1}#also serious language concerns count as DLD
  } 
  if (w1>0){all.data$global_neurodev[i]<-all.data$global_neurodev[i]+w1}
  #NB more severe language problems with poor comprehension get addition of 2 points
  
  # add 1 to code if significant behaviour problems on interview or DAWBA
  
  w1<-unlist(gregexpr(pattern ='4',toString(all.data$neurodev_diag[i]))) #behav problems code is 4
  w2<-all.data$conduct_dsm_r1[i]
  if (is.na(w2)){w2<-0}
  if (max(w1,w2)>0){all.data$global_neurodev[i]<-all.data$global_neurodev[i]+1}
  
  # add 2 to code if ASD on interview or DAWBA or SRS is 90 or more
  
  w1<-unlist(gregexpr(pattern ='3',toString(all.data$neurodev_diag[i]))) #ASD code is 3

  w2<-all.data$asd_dsm_r1[i]
  if(is.na(w2)){w2<-0}
  w3<-0
  
  if(all.data$srs_t_score[i]>89) {w3<-1} #SRS t score 90
  
  if(all.data$srs_t_score[i]>900){w3<-0} 
  if (max(w1,w2,w3)>0){all.data$global_neurodev[i]<-all.data$global_neurodev[i]+2}
  all.data$global_jittered[i]<-all.data$global_neurodev[i]+.5*runif(1)-.25
}

#----------------------------------------------------------------------------------------
#Derive language factor: based on Paul Thompson lavaan factor models script
# First : Deal with missing data; recode to NA
#------------------------------------------------------------------------------------------

for(i in 2:16) #just recode ones used in SEM for now
{
  all.data[,i]<-car::recode(all.data[,i],"996=NA;997=NA;998=NA;999=NA")
}
all.data$missing<-vector(mode="numeric",length=dim(all.data)[1])

for(i in 1:length(all.data[,1]))
{
  all.data$missing[i]<-ifelse(any(is.na(all.data[i,4:15]))==TRUE,1,0)
}
#amount of missing data in all cells, not just rows
#sct missing
sum(is.na(all.data[sctrange,5:16]))/(12*nrow(sct.data))

#twin missing
sum(is.na(all.data[twinrange,5:16]))/(12*nrow(twin.data))

################################################################################
# SEM
################################################################################
#Select data for model
data.f1<-all.data[,3:16]

#Single factor model
names(data.f1)<-c("matrices_ss", "blockD_ss","Vocab_ss", "WJ_ss", "Sent_Rep_ss",
                  "Nword_Rep_ss", "Oro_ss", "Nara_acc", "Nara_comp", "Nara_rate", "Words_ss", 
                  "Nwords_ss", "Pics_ss", "Digit_ss") 

data.f1$Oro_ss_f<-as.factor(data.f1$Oro_ss)
#exclude 2 SCT cases 101 (row 24) and 245 (row 67) - language too poor to attempt tests
data.f2<-data.f1[-c(24,67),]

#----------------------------------------------------------------------------------------------
model.f1 <- ' f1 =~ Vocab_ss + WJ_ss + Sent_Rep_ss + Oro_ss_f + Nword_Rep_ss
f2 =~ Nara_acc + Nara_comp + Nara_rate + Words_ss + Nwords_ss + Pics_ss + Digit_ss
f3 =~ matrices_ss + blockD_ss'
fit.mod.A <- sem(model.f1, data = data.f2,ordered=c("Oro_ss_f"),missing="pairwise")
summary(fit.mod.A, fit.measures = TRUE, standardized=TRUE, rsq=TRUE)

semPaths(fit.mod.A, "std", title = FALSE, 
         nCharNodes=0, edge.label.cex=0.6,esize=0.5) #This line draws diagram but doesn't work for mac!

fs1<-data.frame(record_id=all.data$record_id[-c(24,67)],lavPredict(fit.mod.A))
lowscore<-min(fs1[,2])#the two omitted cases will be assigned lowest score as they had missing data due to poor language
#create dimension for factor score with omitted cases estimated as lowscore
all.data$langfac<-c(fs1[1:23,2],lowscore,fs1[24:65,2],lowscore,fs1[66:528,2])

all.data$allgroup<-as.factor(all.data$allgroup)
levels(all.data$allgroup)<-c('Twin TD','Twin LD','SCT no-bias','SCT bias')
#show means
aggregate(all.data$langfac, list(all.data$allgroup), mean,na.rm=TRUE)
aggregate(all.data$global_neurodev, list(all.data$allgroup), mean,na.rm=TRUE)
plot(all.data$langfac,all.data$global_jittered)
cor(all.data[,c(8,38,41)],use="complete.obs")


