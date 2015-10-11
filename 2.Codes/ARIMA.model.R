ARIMA.model<-function(pred.data,real.data,aheadv,dir=TRUE){
    options(warn=-1)
    write.table(real.data,"C:/Users/akhilesh/Desktop/Course Materials/ISEN 613/Final Project/2.Excel Data/ARIMA.Predict.csv",
                sep=",",row.names=FALSE)
    write.table(real.data,"C:/Users/akhilesh/Desktop/Course Materials/ISEN 613/Final Project/2.Excel Data/ARIMA.Direction.csv",
                sep=",",row.names=FALSE)
    pred<-data.frame(matrix(NA,nrow=aheadv,ncol=(ncol(pred.data))))
    pred[,1]<-real.data[,1]
    names(pred)<-names(pred.data)
    for (i in 2:ncol(pred.data)){
        model<-auto.arima(pred.data[,i])
        x<-tryCatch(predict(model,n.ahead=aheadv),error=function(e) NULL)
        if (length(x)==2){
            pred[1,i]<-x$pred[1]
            pred[2,i]<-x$pred[2]  
        }
    }
    direction<-sign(pred[,-c(1,which(is.na(pred[1,])))])+sign(real.data[,-c(1,which(is.na(pred[1,])))])
    direction<-cbind(real.data[,1],direction)
    if (dir==TRUE) {
        write.table(direction,"C:/Users/akhilesh/Desktop/Course Materials/ISEN 613/Final Project/2.Excel Data/ARIMA.Direction.csv",
                    sep=",",row.names=FALSE,append=TRUE)
    }
    write.table(pred,"C:/Users/akhilesh/Desktop/Course Materials/ISEN 613/Final Project/2.Excel Data/ARIMA.Predict.csv",
                sep=",",row.names=FALSE,append=TRUE)
}