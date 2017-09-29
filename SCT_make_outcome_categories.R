#Create outcome categories for genetic analysis
#DVM Bishop 23rd Sept 2017

#NB found some missing data when doing this; case 336
# case 344 altered school code from 2 to 4 on basis of DAWBA notes
# Need to rerun the lang and dyslexia coding with full data before finalising
# This has been done and file updated to 1109 version on 26 Sept

# Initial goal was to use all available data to create 5 point scale
# 0 = no impairment
# 1 = history of impairment but no current problems (usually speech)
# 2 = current problem in one area (dyslexia/DLD/ADHD) as evidenced by parent report and/or language test scores with PIQ>69
# 3 = current ASD as evidenced by diagnosis, SRS 85+, DAWBA ASD OR current problems in 2+ areas (eg ADHD+DLD)
# 4 = ASD plus other problems, e.g. ADHD, major behavioural problems OR ID (IQ<70) with other problems
# 5 = global impairment: includes those unable to attempt test battery, requiring special schooling with lack of independence or severely problematic behaviour

# But first just checking approach of adding up risk scores - see below

library(tidyverse)
readdir<-"/Users/dorothybishop/Dropbox/ERCAdvanced/Project SCT analysis/data from redcap/"
writedir<-"/Users/dorothybishop/Dropbox/ERCAdvanced/Project SCT analysis/SCT_ASD_analysis/Project_files/Data/"

#load redcap data 
alldat<-read.csv(paste0(readdir,'SCTData_DATA_2017-09-26_1335.csv'))

nrows<-nrow(alldat)

######################################################################################

#nudat was just used to visually check records, ordered by PIQ, when deciding what vars are needed for coding
# nudat<-select(alldat,record_id,neurodev_diag,piq,lang_disorder,dyslexia,srs_t_score,gcc,autism_icd_r1,
#               hyperkinetic_icd_r1,lang_concerns,slt,schooling,cgas_r1,comments,dawba_comments_r1)
# nudat<-nudat[order(nudat$piq),]
# 
# writebit<-paste0(writedir,"allneurodev.csv")
# write.table(nudat, writebit, sep=",",row.names=TRUE) 
######################################################################################



#First create updated code for partial testing
#currently 3 digit code (Doppler can be added as 4th if needed)
#1st digit: 2 all tests done, 1 some done, 9 none done
#2nd digit: 1 SRS and CCC, 0 neither done
#3rd digit: 1 DAWBA done, 0 not done

#-------------------------------------------------------------------------
#Find cases without complete test data for checking.
#-------------------------------------------------------------------------
rawdat<-select(alldat,record_id,age_at_test,wasi_block_design_total_raw,
               wasi_matrices_raw_total,
                    wasi_vocab_total_raw,
                    nepsy_sent_rep_raw,
                    nepsy_nonword_rep_raw,
                    nepsy_oromotor_seq_raw,
                    nara_acc,nara_comp,
                    nara_rate,towre_words_raw,
                    towre_nonwords_raw,
                    phab_pics_raw,phab_digits_raw,
                    wdck_jhsn_ss,comments,dawba_comments_r1)

for (i in 1:nrows){
  w<-which(rawdat[i,3:14]<997)
  rawdat$testdone[i]<-length(w)
  
}
www<-select(rawdat,record_id,testdone,comments,dawba_comments_r1)
ww<-filter(www,testdone<12)
writebit<-paste0(writedir,"missingdata.csv")
write.table(ww, writebit, sep=",",row.names=TRUE) 
#-------------------------------------------------------------------------
# Create global impairment measure
#-------------------------------------------------------------------------

#Will try now to get global score by adding as follows:
# History of speech problems = 1
# Current help in mainstream school (suport or special class or SLT) =1
# Special school = 2
# Dyslexia (testing, unless no data , in which case report) = 1
# DLD (testing, unless no data , in which case report) = 1
# ADHD (report or DAWBA) = 1
# behaviour problems (DAWBA or clear description on interview) = 1
# Autistic features: report from interview, SRS =90,  DAWBA = 2
# low IQ (PIQ < 70 or refusal/inability to do battery - with exception of reading tests) = 1

# NB coding for slt is:
#0, never ; 1, preschool only; 2, beyond 4 yr; 3, ongoing; 8, assessed only; 9, no information

# Neurodev diagnosis codes:
# 0 none; others coded as all applicable from list:
#  1 ADHD, 2 APD, 3 ASD, 4 behav, 5 dyscalc, 6 dyslexia, 7 dyspraxia, 8  DLD/SLI/LD, 9 ID/GDD

#Lang dis from test scores; 0, no; 1, subclinical; 2, yes; 8, iq< 70; 9, no test results
#Dyslexia from test scors :0, no; 1, yes; 8, piq< 70; 9, no test results

#lang_concerns from parental report:
#0, never; 1, past; 2, continuing mild; 3, continuing severe; 9, unclear

#SLT from parent report: 0, never; 1, preschool only; 2, beyond 4 yr; 3, ongoing; 8, assessed only; 9, no information

# School code
#1, mainstream no help; 2, mainstream with help; 3, special class/unit; 4, special school; 5, home schooled; 8, other; 9, d

alldat$global_neurodev_rating<-0 #Initialise to zero
w<-which(is.na(alldat$srs_t_score)) #Recode NA to 999 for SRS
alldat$srs_t_score[w]<-999

temp<-alldat$global_neurodev_rating
w<-unique(which(alldat$slt==1),which(alldat$slt==8)) #Cases with preschool SLT or assessed by SLT
alldat$global_neurodev_rating[w]<-alldat$global_neurodev_rating[w]+1

#Now code so can add one point for help in mainstream or ongoing SLT or in language unit
w1<-c(which(alldat$schooling==2),which(alldat$schooling==3)) #help in mainstream/lang unit

alldat$global_neurodev_rating[w1]<-alldat$global_neurodev_rating[w1]+1

#add 2 points if attending special school
w<-which(alldat$schooling==4)
alldat$global_neurodev_rating[w]<-alldat$global_neurodev_rating[w]+1

#add 1 point if PIQ < 70 or not completed
w<-which(alldat$piq<70)
w1<-which(alldat$piq>996)
allw<-c(w,w1)
alldat$global_neurodev_rating[allw]<-alldat$global_neurodev_rating[allw]+1

## Next bits done in a loop because criteria not captured in a single code

for (i in 1:nrows){
  
  # add 1 to code if meets language test criteria for dyslexia OR (if no data) has diagnosis of this 
  # reported on parent interview
  wd<-NA
  temp<-alldat$dyslexia[i] #coding according to test battery, 1 if dyslexic
  if(temp==9){
    wd<-unlist(gregexpr(pattern ='6',toString(alldat$neurodev_diag[i]))) #dyslexia code is 6
    #wd is one if 6 is included in neurodev_diag
  }
  if(length(wd)<1){temp=0}
  if (temp>0){alldat$global_neurodev_rating[i]<-alldat$global_neurodev_rating[i]+1}

#Add 1 to code if evidence of ADHD on parental interview or DAWBA
  w2<-unlist(gregexpr(pattern ='1',toString(alldat$neurodev_diag[i]))) #ADHD code is 1
  if(w2==0){
  w2<-max(alldat$hyperkinetic_icd_r1[i],alldat$adhd_comb_dsm_r1[i],alldat$adhd_hyp_dsm_r1[i],alldat$adhd_inatt_dsm_r1[i])
  }
if (w2>0){alldat$global_neurodev_rating[i]<-alldat$global_neurodev_rating[i]+1}
  
# add 1 or 2 to code if meets language test criteria for lang_disorder OR (if no data) has diagnosis of this 
# reported on parent interview

temp<-alldat$lang_disorder[i] #coding according to test battery, 2 if with poor comp, 1 otherwise
w1<-temp
if(temp==9){ #no data on language tests so use parent interview
  w1<-unlist(gregexpr(pattern ='8',toString(alldat$neurodev_diag[i]))) #DLD code is 8
  if(alldat$slt[i]==3){w1<-1} #regardless of diagnosis, ongoing SLT counts as DLD
  if(alldat$lang_concerns[i]==3){w1<-1}#also serious language concerns count as DLD
} 
if (w1>0){alldat$global_neurodev_rating[i]<-alldat$global_neurodev_rating[i]+w1}
#NB more severe language problems with poor comprehension get addition of 2 points

# add 1 to code if significant behaviour problems on interview or DAWBA

  w1<-unlist(gregexpr(pattern ='4',toString(alldat$neurodev_diag[i]))) #behav problems code is 4
  w2<-alldat$conduct_dsm_r1[i]
if (is.na(w2)){w2<-0}
  if (max(w1,w2)>0){alldat$global_neurodev_rating[i]<-alldat$global_neurodev_rating[i]+1}
  
# add 2 to code if ASD on interview or DAWBA or SRS is 90 or more

w1<-unlist(gregexpr(pattern ='3',toString(alldat$neurodev_diag[i]))) #ASD code is 3
w2<-max(alldat$asd_dsm_r1[i],alldat$autism_icd_r1[i])
if(is.na(w2)){w2<-0}
w3<-0

if(alldat$srs_t_score[i]>89) {w3<-1} #SRS t score 90

     if(alldat$srs_t_score[i]>900){w3<-0} 
if (max(w1,w2,w3)>0){alldat$global_neurodev_rating[i]<-alldat$global_neurodev_rating[i]+2}

}

#Now write to spreadsheet to check if it all looks OK
shortdat<-select(alldat, record_id, age_at_test,slt,lang_concerns,schooling,piq,neurodev_diag,lang_disorder,dyslexia,
                 hyperkinetic_icd_r1,adhd_comb_dsm_r1,adhd_hyp_dsm_r1,adhd_inatt_dsm_r1,
                 conduct_dsm_r1,asd_dsm_r1,autism_icd_r1,srs_t_score,global_neurodev_rating)
writebit<-paste0(writedir,"global_coded_data.csv")
write.table(shortdat, writebit, sep=",",row.names=TRUE) 


#png(file="mygraphic2.png",width=400,height=350)
pirateplot(formula = alldat$global_neurodev_rating~ Trisomy + Diagnosis,
           point.o = .5,
           bar.o=.0,
           inf.o=.2,
           bean.o=.5,
           jitter=.2,
           data = mydata,
           ylab='Global rating',
           ylim=c(0,10),
           main="Global rating")
#dev.off()
