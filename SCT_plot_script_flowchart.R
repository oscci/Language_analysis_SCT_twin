#http://rich-iannone.github.io/DiagrammeR/graphviz_and_mermaid.html

# Based on version done for twins by Alex Wilson
#

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

main.data <- read.csv(paste0(dir,"SCTData_DATA_2017-10-30_0855.csv"))

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
  if (i==2){
    ascbias<-c(which(main.data$why_tested==2),which(main.data$why_tested==3))
    postnatals<-main.data[ascbias,] #these are no longer pre-post but divided
    #according to whether tested because of behav/neuro concerns
    prenatals<-main.data[-ascbias,] #tested bcs other med concerns
    label1<-'Medical'
    label2<-'Neurodev.'
    label3<-'Reason for testing'
  }
  n.A<-dim(postnatals)[1]
  n.B<-dim(prenatals)[1]
  
  #subset by karyotype
  xxx2<-subset(postnatals,trisomy==1)
  xxy2<-subset(postnatals,trisomy==2)
  xyy2<-subset(postnatals,trisomy==3)
  xxx1<-subset(prenatals,trisomy==1)
  xxy1<-subset(prenatals,trisomy==2)
  xyy1<-subset(prenatals,trisomy==3)
  
  n.C<-dim(xxx1)[1]
  n.D<-dim(xxy1)[1]
  n.E<-dim(xyy1)[1]
  n.F<-dim(xxx2)[1]
  n.G<-dim(xxy2)[1]
  n.H<-dim(xyy2)[1]
  
  #now check DAWBA
  n.I=length(which(xxx1$dawba_diagnoses_rater_1_complete>0))
  n.J=length(which(xxy1$dawba_diagnoses_rater_1_complete>0))
  n.K=length(which(xyy1$dawba_diagnoses_rater_1_complete>0))
  n.L=length(which(xxx2$dawba_diagnoses_rater_1_complete>0))
  n.M=length(which(xxy2$dawba_diagnoses_rater_1_complete>0))
  n.N=length(which(xyy2$dawba_diagnoses_rater_1_complete>0))
  
  
  #now create flow chart ; TB denotes top to bottom
  #Need to add labels along the side: top row 'Reason for testing or Time of testing'
  #Then trisomy, then with DAWBA data
  
  print(grViz("
digraph a_nice_graph {
        
        # node definitions with substituted label text
        node [shape = plaintext, fontname = Helvetica]
        X[label='@@1']
        Y[label= 'Trisomy']
        Z[label = 'With DAWBA']
        
        node [shape=square]
        A[label='@@2',fillcolor=lightBlue]
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
        
        # edge definitions with the node IDs
        A -> {C D E}
        B -> {F G H}
        C -> I
        D -> J
        E -> K
        F -> L
        G -> M
        H -> N
        
        
        X -> Y [alpha=0,color='white']
        Y -> Z [alpha=0,color='white']
}

[1]: paste0(label3, ':\\n ',' Recruited from')
[2]: paste0(label1,':\\n', 'NHS: N = ',y1,':\\n', 'Other: N = ',y2)
[3]: paste0(label2,':\\n', 'NHS: N = ',z1,':\\n' ,'Other: N = ',z2)
[4]: paste0('XXX',':\\n', 'N = ',n.C)
[5]: paste0('XXY',' :\\n', 'N = ',n.D)
[6]: paste0('XYY',' :\\n', 'N = ',n.E)
[7]: paste0('XXX',' :\\n', 'N = ',n.F)
[8]: paste0('XXY',' :\\n', 'N = ',n.G)
[9]: paste0('XYY',' :\\n', 'N = ',n.H)
[10]: paste0('N = ',n.I)
[11]: paste0('N = ',n.J)
[12]: paste0('N = ',n.K)
[13]: paste0('N = ',n.L)
[14]: paste0('N = ',n.M)
[15]: paste0('N = ',n.N)
"))
  
  
}




