data <- c(12,15,16,21,24,29,30,31,32,33,45,46,49,50,52,58,60,63,64,65)
stem(data)
stem(data,scale=2)

library(aplpack)
stem.leaf(data)
stem.leaf(data,m=1)


R<-c(7,12,28,3,41)
M<-c("Mar","Apr","May","Jun","Jul")
png(file="barchart_months_revenue.png")
barplot(R,names.arg=M,xlab="Month",ylab="Revenue",col="blue",main="Revenue Chart",border="red")
dev.off()


colors=c("green","orange","brown")
months<-c("Mar","Apr","May","Jun","Jul")
regions<-c("East","West","North")
values<-matrix(c(2,9,3,11,9,4,8,7,3,12,5,2,8,10,11),nrow=3,ncol=5,byrow=TRUE)
png(file="barchart_stacked.png")
barplot(values,names.arg=months,xlab="Month",ylab="Revenue",col=colors,main="Revenue Chart")


barplot(values,names.arg=months,xlab="Month",ylab="Revenue",col=colors,main="Revenue Chart",beside=TRUE)

participationdata =c(425,1667,45,64)
dim(participationdata)=c(2,2)
dimnames(participationdata)=list(Sex=c("Female","Male"),Age=c("Adult","Child"))
mosaicplot(participationdata,col="light yellow",main="Participation Details")

music=c(210,194,190,406,170,110,730,290)
dim(music)=c(2,2,2)
dimnames(music)=list(Age=c("Old","Young"),Listen=c("Yes","No"),Education=c(""))

library(corrplot)
corrplot(cor(mtcars))
