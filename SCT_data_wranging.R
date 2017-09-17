#Wrangling SCT data
#DVM Bishop 17th Sept 2017

library(tidyverse)
library(yarrr)
readdir<-"/Users/dorothybishop/Dropbox/ERCAdvanced/Project SCT analysis/data from redcap/"

#load redcap data 
alldat<-read.csv(paste0(readdir,'SCTData_DATA_2017-09-17_1441.csv'))
nrows<-nrow(alldat)

alldat<-filter(alldat,trisomy<9) #remove isochromosome case

alldat$trisomy<-as.factor(alldat$trisomy)
levels(alldat$trisomy)<-c('XXX','XXY','XYY')
#Explore age by karyotype
pirateplot(formula = age_at_test ~ trisomy,
           data = alldat,
           main = "Age by karyotype")

#Select relevant variables
smalldat<-select(alldat,age_at_test,trisomy,pre_postnatal_diag,piq,
                               wasi_matrices_raw_total, wasi_matrices_ss,
                               wasi_block_design_total_raw,wasi_block_design_ss,
                               wasi_vocab_total_raw,wasi_vocab_ss,
                               wdck_jhsn_ss,nepsy_sent_rep_raw,sent_rep_ss,
                               nepsy_nonword_rep_raw,nonword_rep_ss,
                               nepsy_oromotor_seq_raw,oromotor_ss,
                               nara_acc,nara_acc_ss,nara_comp,nara_comp_ss,
                               nara_rate,nara_rate_ss,towre_words_raw,towre_words_ss,
                               towre_nonwords_raw,towre_nonwords_ss,
                               phab_pics_raw,phab_pic_ss,phab_digits_raw,phab_digit_ss
                               )
smalldat$ageyr<-as.integer(smalldat$age_at_test/12)
#Check all raw score vs scaled score plots to detect errors

plot(wasi_matrices_ss ~ wasi_matrices_raw_total, data=smalldat,type='n',
     ylim=c(15,80),
     xlab="Raw score", ylab="Scaled score",
     main='WASI Matrices')     
text(smalldat$wasi_matrices_raw_total,smalldat$wasi_matrices_ss,
     label=smalldat$ageyr,cex=.7,col=as.integer(smalldat$trisomy))

legend("topleft", inset=.05, title="Trisomy",
       c("XXX","XXY","XYY"), fill=c(1:3),cex=.75)

plot(wasi_matrices_ss ~ age_at_test, data=smalldat,cex=.7,
     col=as.integer(smalldat$trisomy),
     ylim=c(15,80),
     xlab="Age (mo)", ylab="Scaled score",
     main='WASI Matrices')     
legend("topright", inset=.05, title="Trisomy",
       c("XXX","XXY","XYY"), fill=c(1:3),cex=.75)


