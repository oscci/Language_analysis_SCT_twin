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

#analysis of SRS items - this is my own exploring - not v conclusive, ignore
#add SRS items to my.dawba
# w<-which(colnames(my.dawba1)=='srs_item1') #find col number for first SRS item
# w2<-w+64
# srsitems<-my.dawba1[,w:w2]
# my.dawba<-cbind(my.dawba,srsitems)
# 
# #Just exploring data to see if any oddities in relation to +ve vs -ve items
# anxpos<-c(3,11,43) #This is my personal classification of items
# anxneg<-c(6,9,23,27,64)
# asdpos<-c(12,15,17,21,22,26,40,45,48,52,55)
# asdneg<-c(2,4,5,7,8,10,13,16,18,19,20,24,25,28,29:31,33,34:39,42,46,47,50,51,53,54,56,58,60:63,65)
# other<-c(1,14,32,41,44,49,57,59)
# 
# w<-which(colnames(my.dawba)=='srs_item1') #find col number for first SRS item
# w=w-1
# 
# anxcol1<-anxpos+w
# anxcol2<-anxneg+w
# asdcol1<-asdpos+w
# asdcol2<-asdneg+w
# othcol<-other+w
# 
# my.dawba$anxposmean<-rowMeans(my.dawba[,anxcol1],na.rm=TRUE)
# my.dawba$anxnegmean<-rowMeans(my.dawba[,anxcol2],na.rm=TRUE)
# my.dawba$asdposmean<-rowMeans(my.dawba[,asdcol1],na.rm=TRUE)
# my.dawba$asdnegmean<-rowMeans(my.dawba[,asdcol2],na.rm=TRUE)
# my.dawba$othmean<-rowMeans(my.dawba[,othcol],na.rm=TRUE)
# 
# 
# #Now do the  plots on one pdf
# mysrscats<-c('Anxpos','Anxneg','ASDpos','ASDneg','Other')
# mycols<- c("grey66", "red", "hotpink1","blue","purple")
# dir<
#  pdfname<-paste0(dir,'beeswarm_dbscales.pdf')
#  pdf(pdfname,width=8,height=5)
# par(mfrow=c(2,3))
# for (i in 1:5){
#   boxplot(my.dawba[,(89+i)]~allsubgp , data = my.dawba)
#   beeswarm(my.dawba[,(89+i)]~allsubgp , data = my.dawba, xlab='Trisomy',ylab=mysrscats[i],
#                horizontal=FALSE,col = 5,cex.axis=.65, pch = 16,
#            pwcol = mycols[as.numeric(tricode)],add=TRUE)
# }
# dev.off()
