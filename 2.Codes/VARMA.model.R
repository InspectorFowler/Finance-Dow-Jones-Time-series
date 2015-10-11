VARMA.model<-function(cluster.data,pred.data,real.data,pvalue,qvalue,hvalue,dir=TRUE){
    options(warn=-1)
    suppressMessages(library(MTS))
    suppressMessages(library(forecast))
    write.table(real.data,"C:/Users/akhilesh/Desktop/Course Materials/ISEN 613/Final Project/2.Excel Data/VARMA.Predict.csv",
                sep=",",row.names=FALSE)
    write.table(real.data,"C:/Users/akhilesh/Desktop/Course Materials/ISEN 613/Final Project/2.Excel Data/VARMA.Direction.csv",
                sep=",",row.names=FALSE)
    for (i in 2:ncol(cluster.data)){
        pred<-data.frame(matrix(NA,nrow=hvalue,ncol=(ncol(pred.data))))
        pred[,1]<-real.data[,1]
        names(pred)<-names(pred.data)
        names(pred)[1]<-as.character(paste("Cluster-",(i-1),sep=""))
        for (j in 2:ncol(pred.data)){
            model<-VARMA(cbind(cluster.data[,i],pred.data[,j]),p=pvalue,q=qvalue)
            x<-VARMApred(model,h=hvalue)
            pred[,j]<-x$pred[,2]
        }
        direction<-sign(pred[,-1])+sign(real.data[,-1])
        direction<-cbind(real.data[,1],direction)
        names(direction)[1]<-as.character(paste("Cluster-",(i-1),sep=""))
        write.table(pred,"C:/Users/akhilesh/Desktop/Course Materials/ISEN 613/Final Project/2.Excel Data/VARMA.Predict.csv",sep=",",
                    row.names=FALSE,append=TRUE)
        if (dir==TRUE) write.table(direction,
                                   "C:/Users/akhilesh/Desktop/Course Materials/ISEN 613/Final Project/2.Excel Data/VARMA.Direction.csv",sep=",",
                                   row.names=FALSE,append=TRUE)
    }
    options(warn=0)
}