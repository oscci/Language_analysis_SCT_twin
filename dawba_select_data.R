#dawba_select_data by DVM Bishop
#section of code that only runs when accessed from Rmd (bcs needs mydata in memory)
#based on SCT_plot_script_flowchart.R

library(tidyverse)
library(Hmisc)
library(doBy)
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
                 social_anx_icd_r2,emotional,conduct,hyperactivity,peer,prosocial,
                 sepanx_concern,socanx_concerns,soc_vs_sep,socfear_level)
#last 5 are SDQ scales, added Nov 2017; also social anxiety and sep anxiety added

my.dawba$subgp<-1
w<-c(which(my.dawba$why_tested==2),which(my.dawba$why_tested==3))
my.dawba$subgp[w]<-2
my.dawba$allsubgp<-10*my.dawba$subgp+my.dawba$trisomy #create 2factor code
my.dawba$allsubgp<-as.factor(my.dawba$allsubgp)
levels(my.dawba$allsubgp)<-c('XXX','XXY','XYY','XXX*','XXY*','XYY*') #* denotes asc bias group

#########################################################################
#Look at CGAS
#########################################################################
#First check rater agreement
myr<-rcorr(my.dawba1$cgas_r1,my.dawba1$cgas_r2)
my.dawba$CGAS<-as.integer(my.dawba1$cgas_r1/10)
#my.dawba$cgas<-mean(my.dawba1$cgas_r1,my.dawba1$cgas_r2)

library(ggridges) #for joyplot

cgast<-table(my.dawba$CGAS,my.dawba$allsubgp)
mysum<-summaryBy(CGAS ~ allsubgp, data=my.dawba,
                 FUN = function(x) { c(m = mean(x))})
mymean<-rep(NA,6)
for (i in 1:6){
  mymean[i]<-paste0('Mean = ',round(mysum[i,2],1))
}     

pngname1<-'joyplot_cgas.png'
png(pngname1,width=300,height=400)

ggplot(my.dawba, aes(x = CGAS, y = allsubgp,fill=allsubgp)) + 
  geom_density_ridges(rel_min_height = 0.01)+
  ylab('Low Bias             High Bias           ')+
  theme(axis.text.y= element_text( color="black", size=18))+
  theme(axis.text.x= element_text( color="black", size=14))+
  theme(axis.title= element_text( color="black", size=18))+
  scale_x_continuous(breaks=seq(2,10,2))+
  scale_fill_cyclical(values = c("deeppink","darkorchid1", "deepskyblue"))+
  annotate("text", x=10,y = seq(1.1,6.1,1), label =mymean, size=6)

dev.off()
#file.show(pngname1) 
#########################################################################

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
lowbias.long$SRS.subscale<-NA
lowbias.long$SRS.score<-NA
lowbias.long$SDQ.subscale<-NA
lowbias.long$SDQ.score<-NA
myrow<-nrow(lowbias)
thisrow=0
starti<-which(colnames(lowbias)=='socaw_ss')
endi<-starti+4
for (i in starti:endi){
 lowbias.long[(thisrow+1):(thisrow+myrow),3:4]<-cbind(i-5,lowbias[,i])
 lowbias.long[(thisrow+1):(thisrow+myrow),1:2]<-lowbias.long[1:51,1:2]
 thisrow<-thisrow+myrow
}
#add 2 cols for SDQ
starti<-which(colnames(lowbias)=='emotional')
endi<-starti+4
offset<-starti-1
thisrow=0
for (i in starti:endi){
  lowbias.long[(thisrow+1):(thisrow+myrow),5:6]<-cbind(i-offset,lowbias[,i])
  thisrow<-thisrow+myrow
}

lowbias.long$trisomy<-as.factor(lowbias.long$trisomy)
levels(lowbias.long$trisomy)<-c('XXX','XXY','XYY')

w<-which(lowbias.long$SRS.score>90) #remove jitter that was added for beeswarm
lowbias.long$SRS.score[w]<-90

#---------------------------------------------------------------------
# Test for impact of trisomy on the 5 SRS subscales
#---------------------------------------------------------------------

library(lme4)
library(lmerTest) #Adding this extra package provides p-values (if you want them) for your fixed effects.
myfit <- lmer(SRS.score ~ trisomy + SRS.subscale + trisomy*SRS.subscale + (1 | record_id),  
              data = lowbias.long)
summary(myfit)
plot(myfit) # residuals check - look ok 
qqnorm(residuals(myfit)) #check residuals - look fine.
#Test of unequal variance in trisomy groups.
library(car)
leveneTest(residuals(myfit) ~ lowbias.long$trisomy) # not significant so equality of variance in trisomy groups.


#Look at summary stats for score data by trisomy group
require(psych)
socaw<-describeBy(lowbias$socaw_ss,lowbias$trisomy) # some summary statistics to confirm.
soccog<-describeBy(lowbias$soccog_ss,lowbias$trisomy)
soccomm<-describeBy(lowbias$soccomm_ss,lowbias$trisomy)
socmot<-describeBy(lowbias$socmot_ss,lowbias$trisomy)
autfeat<-describeBy(lowbias$autfeat_ss,lowbias$trisomy)

mysrs.summary<-data.frame(matrix(rep(NA,33),nrow=3))
rownames(mysrs.summary)<-c('XXX','XXY','XYY')
colnames(mysrs.summary)<-c('N','Soc Aware','(SD)','Soc Cog','(SD)','Soc Comm','(SD)','Soc Motiv','(SD)','Aut feat','(SD)')
#write.csv(lowbias.long, "lowbias.long.csv",row.names=FALSE) 
for (i in 1:3){
 mysrs.summary[i,]<-c(unlist(socaw[i])[2],unlist(socaw[i])[3],unlist(socaw[i])[4],
                      unlist(soccog[i])[3],unlist(soccog[i])[4],
                      unlist(soccomm[i])[3],unlist(soccomm[i])[4],
                      unlist(socmot[i])[3],unlist(socmot[i])[4],
                      unlist(autfeat[i])[3],unlist(autfeat[i])[4])
  
}
mysrs.summary<-round(mysrs.summary,2)
file.loc<-"~/Dropbox/ERCadvanced/Project SCT analysis/SCT_ASD_analysis/Project_Files/data/"
write.csv(mysrs.summary, file = paste0(file.loc,"nicetab_srs.csv"))

write.csv(my.dawba,file=paste0(file.loc,'my_dawba_short.csv'))

#---------------------------------------------------------------------
# Test for impact of trisomy on the 5 SDQ subscales
#---------------------------------------------------------------------
myfit2 <- lmer(SDQ.score ~ trisomy + SDQ.subscale + trisomy*SDQ.subscale + (1 | record_id),  
              data = lowbias.long)
summary(myfit2)
plot(myfit2) # residuals check - look ok 
qqnorm(residuals(myfit2)) #check residuals - look fine.
#Test of unequal variance in trisomy groups.
leveneTest(residuals(myfit2) ~ lowbias.long$trisomy) # not significant so equality of variance in trisomy groups.

#---------------------------------------------------------------------
# Means on the 5 SDQ subscales
#---------------------------------------------------------------------

emotional<-describeBy(lowbias$emotional,lowbias$trisomy) # some summary statistics to confirm.
conduct<-describeBy(lowbias$conduct,lowbias$trisomy)
hyperactive<-describeBy(lowbias$hyperactivity,lowbias$trisomy)
peer<-describeBy(lowbias$peer,lowbias$trisomy)
prosocial<-describeBy(lowbias$prosocial,lowbias$trisomy)

mysdq.summary<-data.frame(matrix(rep(NA,33),nrow=3))
rownames(mysdq.summary)<-c('XXX','XXY','XYY')
colnames(mysdq.summary)<-c('N','Emotional','(SD)','Conduct','(SD)','Hyperactive','(SD)','Peer','(SD)','Prosocial','(SD)')
for (i in 1:3){
  mysdq.summary[i,]<-c(unlist(emotional[i])[2],unlist(emotional[i])[3],unlist(emotional[i])[4],
                       unlist(conduct[i])[3],unlist(conduct[i])[4],
                       unlist(hyperactive[i])[3],unlist(hyperactive[i])[4],
                       unlist(peer[i])[3],unlist(peer[i])[4],
                       unlist(prosocial[i])[3],unlist(prosocial[i])[4])
  
}
mysdq.summary<-round(mysdq.summary,2)
file.loc<-"~/Dropbox/ERCadvanced/Project SCT analysis/SCT_ASD_analysis/Project_Files/data/"
write.csv(mysdq.summary, file = paste0(file.loc,"nicetab_sdq.csv"))



socialanx<-describeBy(lowbias$socanx_concerns,lowbias$trisomy)

#social anxiety symptoms
table(lowbias$socanx_concerns,lowbias$trisomy)
table(lowbias$sepanx_concern,lowbias$trisomy)
table(lowbias$soc_vs_sep,lowbias$trisomy)
table(lowbias$socfear_level,lowbias$trisomy)
lowbias$sep_or_soc<-lowbias$socanx_concerns
w<-which(lowbias$sepanx_concern==1)
lowbias$sep_or_soc[w]<-1
sepsoctable<-table(lowbias$sep_or_soc,lowbias$trisomy)

#social anxiety symptoms whole sample
table(my.dawba1$socanx_concerns,my.dawba1$trisomy)
table(my.dawba1$sepanx_concern,my.dawba1$trisomy)
table(my.dawba1$soc_vs_sep,my.dawba1$trisomy)
table(my.dawba1$socfear_level,my.dawba1$trisomy)
my.dawba1$sep_or_soc<-my.dawba1$socanx_concerns
w<-which(my.dawba1$sepanx_concern==1)
my.dawba1$sep_or_soc[w]<-1
sepsoctable<-table(my.dawba1$sep_or_soc,my.dawba1$trisomy)



