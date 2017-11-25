#SCT_for_beeswarm by DVM Bishop
#section of code that only runs when accessed from Rmd after dawba_select_data.R
#(bcs needs my.dawba in memory - but this created problems so now write my.dawba to file and re-read

#Also be aware that repeatedly running later sections may mess up the plots
#because of addition of dummy row - need to start over 

#based on SCT_plot_script_flowchart.R
#--------------------------------------------------------------------------
#Try a beeswarm plot 
#--------------------------------------------------------------------------
library(beeswarm) #particular plot type
file.loc<-"~/Dropbox/ERCadvanced/Project SCT analysis/SCT_ASD_analysis/Project_Files/data/"
myfile<-paste0(file.loc,'my_dawba_short.csv')
my.dawba<-read.csv(myfile)

#reorder levels of tricode
my.dawba$tricode = factor(my.dawba$tricode,levels(my.dawba$tricode)[c(3,1,2,6,4,5)])
#reorder levels of allsubgp
my.dawba$allsubgp = factor(my.dawba$allsubgp,levels(my.dawba$allsubgp)[c(1,3,5,2,4,6)])

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
my.dawba[(myn+1),5:10]<-NA
my.dawba$allsubgp<-as.numeric(my.dawba$allsubgp)
my.dawba$allsubgp[myn+1]<-3.5
my.dawba$allsubgp<-as.factor(my.dawba$allsubgp)
levels(my.dawba$allsubgp)<-c('XXX','XXY','XYY',' ','XXX*','XXY*','XYY*') #* denotes asc bias group
my.dawba$tricode[myn+1]<-1 #NA so won't plot
mycols<- c("grey66", "red", "hotpink1","purple","dodgerblue","blue")
mysrsnames<-c('T-score','Social Awareness','Social Cognition','Social Communication',
              'Social Motivation','Autistic Features')
#quartz()
dir<-"/Users/dorothybishop/Dropbox/ERCadvanced/project SCT analysis/SCT_ASD_analysis/Project_files/Plots/"
png_bees<-paste0(dir,'beeswarm_srsT.png')
png(png_bees,width=1200,height=500)
par(mfrow=c(1,2)) #one row and 2 columns
par(mar=c(5.1,4.1,4.1,0.1))
mysrsnames<-c('T-score','Social Awareness','Social Cognition','Social Communication',
              'Social Motivation','Autistic Features')

beeswarm(srs_t_score~allsubgp , data = my.dawba, xlab='Trisomy',ylab=mysrsnames[1],
         horizontal=FALSE,ylim=c(35,95),col = 5, pch = 16,cex=1.7,cex.axis=1.5,cex.lab=1.5,
         pwcol = mycols[as.numeric(tricode)])
par(xpd=FALSE) #confine to plot area
abline(a=60,b=0,col='darkgray',lty=2)
abline(a=75,b=0,col='darkgray',lty=2)
text(4,65,'Mild',cex=1.5)
text(4,80,'Severe',cex=1.5)
par(xpd=NA) #write outside plot area
text(2,14,'Low bias',cex=1.5)
text(6,14,'High bias',cex=1.5)

#legend as separate plot in panel 2
mylegendcols<- c("grey66","dodgerblue","blue","hotpink1","red", "purple") #different order

mysrsnames<-c('T-score','Social Awareness','Social Cognition','Social Communication',
              'Social Motivation','Autistic Features')
plot(x=NULL, y=NULL , type = "n", axes = F, xlab = "", ylab = "", xlim=c(0,20), ylim=c(0,250)) #set up the plot?
mylabels <- c("None","Social Anxiety (concerns)","Social Phobia","ASD (parent report)","ASD","ASD + Social Phobia")
#draw rectangles with labels
for (i in 1:6){
  x1<-1;x2<-3;y1<-(i-1)*41;y2<-y1+31;
  rect(x1,y1,x2,y2, col = mylegendcols[(7-i)], border = "transparent") 
  text(x2+1,y1+15,mylabels[(7-i)],pos=4,cex=1.2)
}
text(0,y2+18,'Diagnosis',pos=4,offset=0,font=2,cex=1.2)
dev.off()
file.show(png_bees)

#Now do the remaining plots on one pdf

pdfname<-paste0(dir,'beeswarm_subscales.pdf')
pdf(pdfname,width=6,height=8)
par(mfrow=c(3,2))
for (i in 7:11){
 
  p<- beeswarm(my.dawba[,i]~allsubgp , data = my.dawba, xlab='Trisomy',
               horizontal=FALSE,ylim=c(35,95),col = 5,cex.axis=.75, pch = 16,
               ylab=mysrsnames[i-4],pwcol = mycols[as.numeric(tricode)])

  par(xpd=FALSE) #confine to plot area
  abline(a=60,b=0,col='darkgray',lty=2)
  abline(a=75,b=0,col='darkgray',lty=2)
  text(4,65,'Mild')
  text(4,80,'Severe')
  par(xpd=NA) #write outside plot area
  text(2,14,'Low bias')
  text(6,14,'High bias')
}
  #Need to add a legend for the colour codes - options with beeswarm are rubbish

  plot(x=NULL, y=NULL , type = "n", axes = F, xlab = "", ylab = "", xlim=c(0,20), ylim=c(0,250)) #set up the plot?
  mylabels <- c("None","ASD (parent report)","ASD + Social Phobia","ASD","Social Anxiety (concerns)","Social Phobia")
  #draw rectangles with labels
  for (i in 1:5){
    x1<-1;x2<-3;y1<-(i-1)*41;y2<-y1+31;
    rect(x1,y1,x2,y2, col = mylegendcols[i], border = "transparent") 
    text(x2+1,y1+15,mylabels[i],pos=4)
  }
  text(0,y2+18,'Diagnosis',pos=4,offset=0,font=2)
  

dev.off()
file.show(pdfname) 


#Now compute percentages with socanx or asd by subgroup
mytab<-table(my.dawba$tricode,my.dawba$allsubgp)
mytab<-mytab[,-4] #remove the blank column used to separate groups in plot!
tab.socanx<-rbind(colSums(mytab[1:3,]),colSums(mytab[4:5,]))
tab.asd<-rbind(colSums(mytab[c(1,4),]),colSums(mytab[c(2,3,5),]))
rownames(tab.socanx)<-c('None','SocAnx')
rownames(tab.asd)<-c('None','ASD')

lobias_socanx<-chisq.test(tab.socanx[,1:3])
hibias_socanx<-chisq.test(tab.socanx[,4:6])
lobias_asd<-chisq.test(tab.asd[,1:3])
hibias_asd<-chisq.test(tab.asd[,4:6])

mytaball<-cbind(rowSums(mytab[,c(1,4)]),rowSums(mytab[,c(2,5)]),rowSums(mytab[,c(3,6)]))
colnames(mytaball)<-c('XXX','XXY','XYY')

taball.socanx<-rbind(colSums(mytaball[c(1:3),]),colSums(mytaball[4:5,]))
taball.asd<-rbind(colSums(mytaball[c(1,4),]),colSums(mytaball[c(2,3,5),]))

p1.XXX.socanx<-as.integer(100*tab.socanx[2,1]/(tab.socanx[2,1]+tab.socanx[1,1]))
p1.XXY.socanx<-as.integer(100*tab.socanx[2,2]/(tab.socanx[2,2]+tab.socanx[1,2]))
p1.XYY.socanx<-as.integer(100*tab.socanx[2,3]/(tab.socanx[2,3]+tab.socanx[1,3]))
p2.XXX.socanx<-as.integer(100*tab.socanx[2,4]/(tab.socanx[2,4]+tab.socanx[1,4]))
p2.XXY.socanx<-as.integer(100*tab.socanx[2,5]/(tab.socanx[2,5]+tab.socanx[1,5]))
p2.XYY.socanx<-as.integer(100*tab.socanx[2,6]/(tab.socanx[2,6]+tab.socanx[1,6]))

p1.XXX.asd<-as.integer(100*tab.asd[2,1]/(tab.asd[2,1]+tab.asd[1,1]))
p1.XXY.asd<-as.integer(100*tab.asd[2,2]/(tab.asd[2,2]+tab.asd[1,2]))
p1.XYY.asd<-as.integer(100*tab.asd[2,3]/(tab.asd[2,3]+tab.asd[1,3]))
p2.XXX.asd<-as.integer(100*tab.asd[2,4]/(tab.asd[2,4]+tab.asd[1,4]))
p2.XXY.asd<-as.integer(100*tab.asd[2,5]/(tab.asd[2,5]+tab.asd[1,5]))
p2.XYY.asd<-as.integer(100*tab.asd[2,6]/(tab.asd[2,6]+tab.asd[1,6]))

#DAWBA only ASD rates
p1.XYY.asd.d<-as.integer(100*(mytab[2,3]+mytab[5,3])/sum(mytab[,3]))
p2.XYY.asd.d<-as.integer(100*(mytab[2,6]+mytab[5,6])/sum(mytab[,6]))

#Test hypothesis that extra X associated with social anxiety
#Group together all XXX and XXY vs XYY and compare on social anx
extraXtab<-cbind(sum(tab.socanx[1,c(1,2,4,5)]),sum(tab.socanx[1,c(3,6)]))
extraYtab<-cbind(sum(tab.socanx[2,c(1,2,4,5)]),sum(tab.socanx[2,c(3,6)]))
extratab<-rbind(extraXtab,extraYtab)
colnames(extratab)<-c('X','Y')
rownames(extratab)<-c('No','Socanx')

extraXasd<-cbind(sum(tab.asd[1,c(1,2,4,5)]),sum(tab.asd[1,c(3,6)]))
extraYasd<-cbind(sum(tab.asd[2,c(1,2,4,5)]),sum(tab.asd[2,c(3,6)]))
extratabasd<-rbind(extraXasd,extraYasd)
colnames(extratabasd)<-c('X','Y')
rownames(extratabasd)<-c('No','ASD')