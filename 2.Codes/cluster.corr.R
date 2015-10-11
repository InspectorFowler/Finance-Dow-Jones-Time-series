cluster.corr<-function(stock.data,pred.data,divisor,dist.method,clust.method,clusters,
                       Prcnt=TRUE,Std=FALSE,charts=FALSE,indexseries=FALSE){
    
    # Sourcing the user defined functions
    source('C:/Users/akhilesh/Desktop/Course Materials/ISEN 613/Final Project/3.Codes/hcluster.R')
    source('C:/Users/akhilesh/Desktop/Course Materials/ISEN 613/Final Project/3.Codes/clusterindex.R')
    source('C:/Users/akhilesh/Desktop/Course Materials/ISEN 613/Final Project/3.Codes/correlation.R')
    
    # Creating clusters, mini-indices and finding correlations
    clusters<-hcluster(stock.data,dist.method,clust.method,clusters,Prcnt,Std,FALSE)
    x<-clusterindex(clusters,stock.data,divisor,Prcnt,Std)
    y<-correlation(x,pred.data,charts)
    par(mfrow=c(1,1))
    if (indexseries==TRUE) return(x)
    else return(y)
}