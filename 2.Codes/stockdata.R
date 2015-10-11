stockdata<-function(index,from,to,interval,Prcnt=FALSE,Std=FALSE){
    suppressMessages(library(xts))
    
    # Extracting day, month and year in desired format
    from<-as.Date(from)
    to<-as.Date(to)
    a<-as.character(as.numeric(format(from,"%m"))-1)
    b<-as.character(format(from,"%d"))
    c<-as.character(format(from,"%Y"))
    d<-as.character(as.numeric(format(to,"%m"))-1)
    e<-as.character(format(to,"%d"))
    f<-as.character(format(to,"%Y"))
    g<-tolower(substring(interval,1,1))
    
    # Creating an empty numeric vector
    x <- vector(mode="numeric")
    
    # Pulling maximum data available for each stock in the given timeframe
    for (i in 1:nrow(index)){
        temp<-nrow(read.csv(paste("http://real-chart.finance.yahoo.com/table.csv?s=",
                                     as.character(index[i,"Symbol"]),"&a=",a,"&b=",b,"&c=",
                                     c,"&d=",d,"&e=",e,"&f=",f,"&g=",g,"&ignore=.csv",sep=""),
                               colClasses=c(rep("NULL",6),"numeric")))
        x<-c(x,temp)            
    }
    
    # Removing companies which do not have the complete stock history
    if((length(which(x<max(x)))==0)==FALSE){
        index<-index[-c(which(x<max(x))),]
        row.names(index)<- 1:nrow(index)
    }
            
    # Creating the dataframe for storing all the stock data starting with the first entry
    stock.data<-read.csv(paste("http://real-chart.finance.yahoo.com/table.csv?s=",
                            as.character(index[1,"Symbol"]),"&a=",a,"&b=",b,"&c=",
                            c,"&d=",d,"&e=",e,"&f=",f,"&g=",g,"&ignore=.csv",sep=""),
                      colClasses=c("character",rep("NULL",5),"numeric"))
    names(stock.data)<-c("Date",as.character(index[1,"Symbol"]))
    
    # Pulling all the remaining stock data from Yahoo! finance
    for (i in 2:nrow(index)){
        dataset<-read.csv(paste("http://real-chart.finance.yahoo.com/table.csv?s=",
                                as.character(index[i,"Symbol"]),"&a=",a,"&b=",b,"&c=",
                                c,"&d=",d,"&e=",e,"&f=",f,"&g=",g,"&ignore=.csv",sep=""),
                 colClasses=c(rep("NULL",6),"numeric"))
        names(dataset)<-as.character(index[i,"Symbol"])
        stock.data<-cbind(stock.data,dataset)            
    }
    
    #Renaming headers to full company name
    colnames(stock.data)<-c("Date",as.character(paste(index[,1],index[,3],sep=":")))
    
    #Converting the data frame into a time series data
    stock.data$Date<-as.Date(as.character(stock.data$Date),format="%Y-%m-%d")
    stock.data<-xts(stock.data[,-1],stock.data$Date)
    
    #Calculating percentage changes in stock values if TRUE
    if (Prcnt==TRUE){
        stock.data<-(diff(stock.data)/stock.data[-nrow(stock.data),] * 100)
        stock.data<-stock.data[-1,]
    }
    
    #Converting back to dataframe
    stock.data<-data.frame(Date=index(stock.data),coredata(stock.data))
    
    #Standardizing the time series if required
    if (Std==TRUE){
        scaled.stock<-scale(stock.data[,-1])
        scaled.stock<-as.data.frame(scaled.stock)
        scaled.stock<-cbind(stock.data[,1],scaled.stock)
        stock.data<-scaled.stock
        names(stock.data)[1]<-"Date"
    }
    
    #output and writing to system
    write.csv(stock.data,file=
                  "C:/Users/akhilesh/Desktop/Course Materials/ISEN 613/Final Project/2.Excel Data/temp.csv",
              row.names=FALSE)
    stock.data
}