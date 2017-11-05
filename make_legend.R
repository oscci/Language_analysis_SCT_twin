#set up png
#png(filename = "tryplot.png", width = 5, height = 15, units = "cm", pointsize = 12, res = 300)


par(usr = c(0, 51, 0, 200)) #make the plot window a certain size?
mycols<- c("grey66","hotpink1","purple", "red", "blue")
plot(x=NULL, y=NULL , type = "n", axes = F, xlab = "", ylab = "", xlim=c(0,20), ylim=c(0,250)) #set up the plot?
mylabels <- c("None","ASD (parent report)","ASD + Social Anxiety","ASD","Social Anxiety")
#draw rectangles with labels
for (i in 1:5){
  x1<-1;x2<-31;y1<-(i-1)*41;y2<-y1+31;
 rect(x1,y1,x2,y2, col = mycols[i], border = "transparent") 
 text(x2+15,y1+15,mylabels[i],pos=4)
}
text(0,y2+18,'Diagnosis',pos=4,offset=0,font=2)
#dev.off()