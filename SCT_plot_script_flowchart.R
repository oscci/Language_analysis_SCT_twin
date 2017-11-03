#http://rich-iannone.github.io/DiagrammeR/graphviz_and_mermaid.html

# Based on version done for twins by Alex Wilson
# 1st nov 2017 - now with plot of SRS by group by diagnosis

#install.packages('DiagrammeR')
library(DiagrammeR)
library(tidyverse)
library(stringr)
#Function to detect if OS is PC or Macintosh and set path accordingly
get_os <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}
os1=get_os()
dir<-"/Users/dorothybishop/Dropbox/ERCadvanced/project SCT analysis/SCT_ASD_analysis/Project_files/Data/"
if(os1=="windows"){
  dir<-"C:\\Users\\wilsona\\Dropbox\\project SCT analysis\\SCT_ASD_analysis\\Project_files\\Data\\"}

#Sorry had to add this bit as the link didn't work for me. i.e. manual down load and redirect link.
#dir.PT<-"c:/Users/pthompson/Desktop/"

main.data <- read.csv(paste0(dir,"SCTData_DATA_2017-11-01_1815.csv"))
w<-which(main.data$trisomy>8) #isochromosome case to be removed
main.data<-main.data[-w,] #remove one record
names(main.data)[1]<-"record_id"

#Referral source
nhscases<-c(70, 212, 213, 214, 215, 218, 221, 222, 223, 225, 229, 231,
            233, 234, 237, 239, 240, 241, 247, 251, 252, 254, 255, 257,
            258, 264, 265, 268, 270, 273, 274, 276, 283, 285, 297, 304,
            306, 307, 310, 318, 319, 321, 322, 323, 327, 328, 329, 331,
            334, 335, 336, 337, 340, 342, 346, 348, 351, 352, 355, 357,
            358, 360, 362, 365, 367, 368, 369, 370)
nhsset<-filter(main.data,record_id %in% nhscases)
othset<-setdiff(main.data,nhsset)

y<-table(nhsset$pre_postnatal_diag)
z<-table(othset$pre_postnatal_diag)
y1=y[1]
y2=y[2]
z1=z[1]
z2=z[2]



for (i in 1:1){ #change to 2 to see rates by reason for testing
  #Count for diagnosis pre or postnatal
  prenatals<-subset(main.data,pre_postnatal_diag==0)
  postnatals<-subset(main.data,pre_postnatal_diag==1)
  label1<-'Prenatal'
  label2<-'Postnatal'
  label3<-'When SCT identified'

  n.A<-dim(postnatals)[1]
  n.B<-dim(prenatals)[1]
  
    #   reason for testing 2 = Behaviour and 3 =Neurodev.
    biasgrp<-filter(main.data,why_tested==3|why_tested==2)
  nobiasgrp<-filter(main.data,why_tested<2|why_tested>3)
  
  #subset by karyotype
  xxx1<-subset(nobiasgrp,trisomy==1)
  xxy1<-subset(nobiasgrp,trisomy==2)
  xyy1<-subset(nobiasgrp,trisomy==3)
  xxx2<-subset(biasgrp,trisomy==1)
  xxy2<-subset(biasgrp,trisomy==2)
  xyy2<-subset(biasgrp,trisomy==3)

  
  n.C <-nrow(nobiasgrp)
  n.D <-nrow(biasgrp) 
  
  
  n.E<-nrow(xxx1)
  n.F<-nrow(xxy1)
  n.G<-nrow(xyy1)
  n.H<-nrow(xxx2)
  n.I<-nrow(xxy2)
  n.J<-nrow(xyy2)
  
  #now check DAWBA
  n.K=length(which(xxx1$dawba_diagnoses_rater_1_complete>0))
  n.L=length(which(xxy1$dawba_diagnoses_rater_1_complete>0))
  n.M=length(which(xyy1$dawba_diagnoses_rater_1_complete>0))
  n.N=length(which(xxx2$dawba_diagnoses_rater_1_complete>0))
  n.O=length(which(xxy2$dawba_diagnoses_rater_1_complete>0))
  n.P=length(which(xyy2$dawba_diagnoses_rater_1_complete>0))
  

  }
  #now create flow chart ; TB denotes top to bottom
  #Need to add labels along the side: top row 'Reason for testing or Time of testing'
  #Then trisomy, then with DAWBA data
  
  print(grViz("
digraph a_nice_graph {
        
        # node definitions with substituted label text
        node [shape = plaintext, fontname = Helvetica]
        X[label='@@1']
        Y[label= 'Bias']
        Z[label= 'Trisomy']
        ZZ[label = 'With DAWBA']
        
        node [shape=square]
        A[label='@@2']
        B[label='@@3']
        
        C[label='@@4']
        D[label='@@5']
        E[label='@@6']
        F[label='@@7']
        G[label='@@8']
        H[label='@@9']
        I[label='@@10']
        J[label='@@11']
        K[label='@@12']
        L[label='@@13']
        M[label='@@14']
        N[label='@@15']
        O[label='@@16']
        P[label='@@17']
        # edge definitions with the node IDs
        A -> C
        B -> {C D}
        C -> {E F G}
        D -> {H I J}
        E -> K
        F -> L
        G -> M
        H -> N
        I-> O
        J -> P
    
        X -> Y [alpha=0,color='white']
        Y -> Z [alpha=0,color='white']
       Z -> ZZ [alpha=0,color='white']
}

[1]: paste0(label3, ':\\n ',' Recruited from')
[2]: paste0(label1,':\\n', 'NHS: N = ',y1,':\\n', 'Other: N = ',y2)
[3]: paste0(label2,':\\n', 'NHS: N = ',z1,':\\n' ,'Other: N = ',z2)
[4]: paste0('Low',':\\n', 'N = ',n.C)
[5]: paste0('High',':\\n', 'N = ',n.D)
[6]: paste0('XXX',':\\n', 'N = ',n.E)
[7]: paste0('XXY',':\\n', 'N = ',n.F)
[8]: paste0('XYY',':\\n', 'N = ',n.G)
[9]: paste0('XXX',':\\n', 'N = ',n.H)
[10]: paste0('XXY',':\\n', 'N = ',n.I)
[11]: paste0('XYY',':\\n', 'N = ',n.J)
[12]: paste0('N = ',n.K)
[13]: paste0('N = ',n.L)
[14]: paste0('N = ',n.M)
[15]: paste0('N = ',n.N)
[16]: paste0('N = ',n.O)
[17]: paste0('N = ',n.P)
"))
  
  #Create categories for plotting
  #Exclude those without DAWBA and divide the rest (a) by karyotype and (b) by reason for testing
  #i.e. 6 subgroups
  
  #Within each subgroup identify whether 1) Social Anxiety on DAWBA, 2) ASD on DAWBA or 3) ASD on parent report
  #Not mutually exclusive, but 2 trumps 3. So each case needs coding as 10, 2, 3, 12,13. 
  #Then plot SRS scores - for whole score as well as subscales, for 6 subgroups, with symbols denoting psychiat code
  
  my.dawba<-filter(main.data,dawba_diagnoses_rater_1_complete>0)
  my.dawba<-select(my.dawba,record_id,trisomy,why_tested,asd,srs_t_score,socaw_ss,soccog_ss,
                   soccomm_ss,socmot_ss,autfeat_ss,asd_dsm_r1,autism_icd_r1,social_anx_dsm_r1,
                   social_anx_icd_r1,asd_dsm_r2,autism_icd_r2,social_anx_dsm_r2,
                   social_anx_icd_r2)
  my.dawba$subgp<-1
  w<-c(which(my.dawba$why_tested==2),which(my.dawba$why_tested==3))
  my.dawba$subgp[w]<-2
  my.dawba$allsubgp<-10*my.dawba$subgp+my.dawba$trisomy
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
  
 table(my.dawba$tricode,my.dawba$allsubgp) 
 

 # Initial try as dotplot but commented out as beeswarm is better
#  p<-ggplot(my.dawba, aes(x=allsubgp, y=srs_t_score, fill=tricode)) +
#    geom_dotplot(binaxis='y', stackdir='center')
# 
#  p+scale_fill_manual(values=c("white", "red", "pink","blue","purple"))
# 
# for (i in 1:5){
#   p<-ggplot(my.dawba, aes(x=allsubgp, y=my.dawba[,(i+5)], fill=tricode)) +
#     geom_dotplot(binaxis='y', stackdir='center')
#   
#   p1<-p+scale_fill_manual(values=c("white", "red", "pink","blue","purple"))
#   p1+ylab(colnames(my.dawba[i+5]))
# }

 #--------------------------------------------------------------------------
 #Try a beeswarm plot 
 #--------------------------------------------------------------------------
 library(beeswarm) #particular plot type
 #first need to jitter those with SRS of 90 - too many to plot
 #add a random number to those with scores of 90
 for (i in 5:10){
 w<-which(my.dawba[,i]>89)
 myr<-runif(w,0,4)
 my.dawba[w,i]<-90+myr
 }
 #--------------------------------------------------------------------------
#We want a gap between the two blocks of groups.
 #Try doing this by creating a datapoint from a fake group
 myn<-nrow(my.dawba)
 dummyrow<-my.dawba[myn,]
my.dawba<-rbind(my.dawba,dummyrow)
my.dawba$record_id[myn+1]<-'dummy'
my.dawba$allsubgp<-as.numeric(my.dawba$allsubgp)
my.dawba$allsubgp[myn+1]<-3.5
my.dawba$allsubgp<-as.factor(my.dawba$allsubgp)
levels(my.dawba$allsubgp)<-c('XXX','XXY','XYY',' ','XXX*','XXY*','XYY*') #* denotes asc bias group
my.dawba$tricode[myn+1]<-0 #creates NA so won't plot
mycols<- c("grey66", "red", "hotpink1","blue","purple")
mysrsnames<-c('T-score','Social Awareness','Social Cognition','Social Communication',
              'Social Motivation','Autistic Features')
#quartz()
pdf('beeswarm_srsT.pdf',width=5,height=4)
beeswarm(my.dawba[,5]~allsubgp , data = my.dawba, xlab='Trisomy',ylab=mysrsnames[1],
             horizontal=FALSE,ylim=c(35,95),col = 5,cex.axis=.85, pch = 16,
         pwcol = mycols[as.numeric(tricode)])
par(xpd=FALSE) #confine to plot area
abline(a=60,b=0,col='darkgray',lty=2)
abline(a=75,b=0,col='darkgray',lty=2)
text(4,65,'Mild')
text(4,80,'Severe')
par(xpd=NA) #write outside plot area
text(2,14,'Low bias')
text(6,14,'High bias')
#legend("top", legend = c("None","ASD","ASD report","Soc Anx","Both"),
# title='Soc Anx/ASD Diagnosis')
dev.off()


#Now do the remaining plots on one pdf
pdf('beeswarm_subscales.pdf',width=8,height=5)
par(mfrow=c(2,3))
for (i in 6:10){

 p<- beeswarm(my.dawba[,i]~allsubgp , data = my.dawba, xlab='Trisomy',ylab=mysrsnames[i-4],
           horizontal=FALSE,ylim=c(35,95),col = 5,cex.axis=.65, pch = 16,pwcol = mycols[as.numeric(tricode)]
        )
 par(xpd=FALSE) #confine to plot area
 abline(a=60,b=0,col='darkgray',lty=2)
 abline(a=75,b=0,col='darkgray',lty=2)
 text(4,65,'Mild')
 text(4,80,'Severe')
 par(xpd=NA) #write outside plot area
 text(2,14,'Low bias')
 text(6,14,'High bias')

}
dev.off()

#Need to add a legend for the colour codes
