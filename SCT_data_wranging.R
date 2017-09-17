#Wrangling SCT data
#DVM Bishop 17th Sept 2017

library(tidyverse)
library(yarrr)
readdir<-"/Users/dorothybishop/Dropbox/ERCAdvanced/Project SCT analysis/data from redcap/"

#load redcap data 
alldat<-read.csv(paste0(readdir,'SCTData_DATA_2017-09-17_1615.csv'))
#NB one error in scaled score transform found from data inspection
#case 276 had wrong ss for TOWRE nonwords - now corrected on Redcap

nrows<-nrow(alldat)

alldat<-filter(alldat,trisomy<9) #remove isochromosome case

alldat$trisomy<-as.factor(alldat$trisomy)
levels(alldat$trisomy)<-c('XXX','XXY','XYY')
#Explore age by karyotype
pirateplot(formula = age_at_test ~ trisomy,
           data = alldat,
           main = "Age by karyotype")

#Select relevant variables
smalldat<-select(alldat,record_id,age_at_test,trisomy,pre_postnatal_diag,piq,
                               wasi_matrices_raw_total, wasi_matrices_ss,
                               wasi_block_design_total_raw,wasi_block_design_ss,
                               wasi_vocab_total_raw,wasi_vocab_ss,
                               nepsy_sent_rep_raw,sent_rep_ss,
                               nepsy_nonword_rep_raw,nonword_rep_ss,
                               nepsy_oromotor_seq_raw,oromotor_ss,
                               nara_acc,nara_acc_ss,nara_comp,nara_comp_ss,
                               nara_rate,nara_rate_ss,towre_words_raw,towre_words_ss,
                               towre_nonwords_raw,towre_nonwords_ss,
                               phab_pics_raw,phab_pic_ss,phab_digits_raw,phab_digit_ss,
                               wdck_jhsn_ss)
#Woodcock Johnson at the end as no raw score (computed by auto scorer)

smalldat$ageyr<-as.integer(smalldat$age_at_test/12)
#All missing data as NA
for (i in 1:length(smalldat)){
  w<-which(smalldat[,i]>900)
  smalldat[w,i]<-NA
}

#Check all raw score vs scaled score plots to detect errors

x=4
for (i in 1:13){
  x=x+2
  y=x+1
mytitle<-colnames(smalldat)[y]
yhi<-max(which(smalldat[,y]<900))
ylo<-min(which(smalldat[,y]>0))
xhi<-max(which(smalldat[,x]<900))
xlo<-min(which(smalldat[,x]>0))
#First plot shows raw vs scaled score by ageyr and Trisomy
plot(smalldat[,y] ~ smalldat[,x],type='n',
     xlab="Raw score", ylab="Scaled score",
     main=mytitle)     
text(smalldat[,x],smalldat[,y],
     label=smalldat$ageyr,cex=.7,col=as.integer(smalldat$trisomy))
legend("topleft", inset=.05, title="Trisomy",
       c("XXX","XXY","XYY"), fill=c(1:3),cex=.75)

plot(smalldat[,y] ~ smalldat$age_at_test, cex=.7,
     col=as.integer(smalldat$trisomy),
     xlab="Age (mo)", ylab="Scaled score",
     main=mytitle)     
legend("topright", inset=.05, title="Trisomy",
       c("XXX","XXY","XYY"), fill=c(1:3),cex=.75)
}

