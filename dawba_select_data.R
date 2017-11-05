#dawba_select_data by DVM Bishop
#section of code that only runs when accessed from Rmd (bcs needs mydata in memory)
#based on SCT_plot_script_flowchart.R

library(tidyverse)
#Create categories for plotting
#Exclude those without DAWBA and divide the rest (a) by karyotype and (b) by reason for testing
#i.e. 6 subgroups

#Within each subgroup identify whether 1) Social Anxiety on DAWBA, 2) ASD on DAWBA or 3) ASD on parent report
#Not mutually exclusive, but 2 trumps 3. So each case needs coding as 10, 2, 3, 12,13. 
#Then plot SRS scores - for whole score as well as subscales, for 6 subgroups, with symbols denoting psychiat code



my.dawba1<-filter(mydata,dawba_diagnoses_rater_1_complete>0)
my.dawba<-dplyr::select(my.dawba1,record_id,trisomy,why_tested,asd,srs_t_score,socaw_ss,soccog_ss,
                 soccomm_ss,socmot_ss,autfeat_ss,asd_dsm_r1,autism_icd_r1,social_anx_dsm_r1,
                 social_anx_icd_r1,asd_dsm_r2,autism_icd_r2,social_anx_dsm_r2,
                 social_anx_icd_r2)
my.dawba$subgp<-1
w<-c(which(my.dawba$why_tested==2),which(my.dawba$why_tested==3))
my.dawba$subgp[w]<-2
my.dawba$allsubgp<-10*my.dawba$subgp+my.dawba$trisomy #create 2factor code
my.dawba$allsubgp<-as.factor(my.dawba$allsubgp)
levels(my.dawba$allsubgp)<-c('XXX','XXY','XYY','XXX*','XXY*','XYY*') #* denotes asc bias group

#recode DAWBA. for the moment code as positive if either rater codes as >0
my.dawba$socanx<-0 #initialise
w<-unique(c(which(my.dawba$social_anx_dsm_r1>0),which(my.dawba$social_anx_dsm_r2>0),
            which(my.dawba$social_anx_icd_r1>0),which(my.dawba$social_anx_icd_r2>0)))
my.dawba$socanx[w]<-1
my.dawba$aut<-0 #initialise
w<-unique(c(which(my.dawba$asd_dsm_r1>0),which(my.dawba$asd_dsm_r2>0),
            which(my.dawba$asd_icd_r1>0),which(my.dawba$asd_icd_r2>0)))
my.dawba$aut[w]<-1
my.dawba$parasd<-my.dawba$asd
w<-which(my.dawba$parasd>1)
my.dawba$parasd[w]<-1
my.dawba$tricode<-0
w<-which(my.dawba$parasd==1)
my.dawba$tricode[w]<-3
w<-which(my.dawba$aut==1)
my.dawba$tricode[w]<-2
w<-which(my.dawba$socanx==1)
my.dawba$tricode[w]<-my.dawba$tricode[w]+10
my.dawba$tricode<-as.factor(my.dawba$tricode)
levels(my.dawba$tricode)  <- c('None','ASD','ASD.par','SocAnx','SocAnx.ASD')                                                      

mytab<-table(my.dawba$tricode,my.dawba$allsubgp) 
tab.lobias<-mytab[,1:3]
tab.socanx<-rbind(colSums(tab.lobias[1:3,]),colSums(tab.lobias[4:5,]))

#---------------------------------------------------------------------
# Create long form file for SRS subscales, Low Bias only
#---------------------------------------------------------------------
lowbias<-filter(my.dawba,allsubgp=='XXX'|allsubgp=='XXY'|allsubgp=='XYY')
table(lowbias$trisomy)
lowbias.long<-dplyr::select(lowbias,record_id,trisomy)
lowbias.long$subscale<-NA
lowbias.long$score<-NA
myrow<-nrow(lowbias)
thisrow=0
for (i in 6:10){
 lowbias.long[(thisrow+1):(thisrow+myrow),3:4]<-cbind(i-5,lowbias[,i])
 lowbias.long[(thisrow+1):(thisrow+myrow),1:2]<-lowbias.long[1:51,1:2]
 thisrow<-thisrow+myrow
}
lowbias.long$trisomy<-as.factor(lowbias.long$trisomy)
levels(lowbias.long$trisomy)<-c('XXX','XXY','XYY')

w<-which(lowbias.long$score>90) #remove jitter that was added for beeswarm
lowbias.long$score[w]<-90

library(lme4)

fit1a <- lmer(score ~ trisomy + subscale + trisomy*subscale + (1 | record_id),  
              data = lowbias.long)
#model 1 output
summary(fit1a)



