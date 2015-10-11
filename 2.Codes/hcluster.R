hcluster<-function(stock.data,dist.method,clust.method,clusters,
                   Detrend=FALSE,Prcnt=TRUE,Std=FALSE,charts=FALSE){
    
    # Loading the necessary libraries
    suppressMessages(library(sparcl)) # Color dendrogram function
    suppressMessages(library(TSclust)) # Clustering package for Time series
    suppressMessages(library(ggplot2)) # Pretty plots
    suppressMessages(library(reshape2)) # melting the dataframe
    suppressMessages(library(grid)) # ggplot associate for creating grids
    suppressMessages(library(xts)) # Converting to time series data format and back
    suppressMessages(library(pracma))# De-trending the time series
    
    # Ensuring the first column of the dataframe is named 'Date'
    names(stock.data)[1]<-"Date"
    
    #De-trending the stock data
    if(Detrend==TRUE){
      for (i in 2:ncol(stock.data)) stock.data[,i]<-detrend(stock.data[,i])
    }
    
    # Converting to time series, calculating lags and converting back to dataframe
    if (Prcnt==TRUE){
        stock<-stock.data
        stock$Date<-as.Date(as.character(stock$Date),format="%Y-%m-%d")
        stock<-xts(stock[,-1],stock$Date)
        stock<-(diff(stock)/stock[-nrow(stock),]*100)
        stock<-stock[-1,]
        stock<-data.frame(Date=index(stock),coredata(stock))
        if (Std==TRUE){
            
            # Standardizing the dataset if required
            scaled.stock<-scale(stock[,-1])
            scaled.stock<-as.data.frame(scaled.stock)
            scaled.stock<-cbind(stock[,1],scaled.stock)
            stock<-scaled.stock
        }
    }
    else
        stock<-stock.data
    
    # Computing the dissimilarity matrix and creating the hierarchical cluster
    Diss<-diss(stock[,-1],dist.method)
    newclust<-hclust(Diss,method=clust.method)
    x<-cutree(newclust,clusters)
    y<-as.character(x)
    
    # Plotting the time series data as per groups
    if(charts==TRUE){
        vplayout<-function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(ceiling(clusters/2),2)))
        stock.data$Date<-as.Date(as.character(stock.data$Date),format="%Y-%m-%d")
        for (i in 1:clusters){
            dataset<-stock.data[,c(1,(which(y==i)+1))]
            dataset<- melt(dataset,id.vars="Date")
            var<-ggplot(dataset)+geom_line(aes(x = Date, y = value, colour = variable))
            if(i<=ceiling(clusters/2)){
                print(var,vp=vplayout(i,1))
            }
            else
                print(var,vp=vplayout((i-ceiling(clusters/2)),2))
        }
    }
    
    # Creating the cluster dendrogram
    else
        ColorDendrogram(newclust,y=x,labels=paste(as.character(y),names(x)),main=paste("Stock Clusters: Distance Method-",
                                                                dist.method,", Clustering Method-",
                                                                clust.method,", Lagged-",Prcnt,
                                                                ", Standardized-",Std,sep="")
                        ,cex.main=0.9,branchlength=5)
    
    # Return the clusters for further analysis
    return(as.numeric(y))
    
}