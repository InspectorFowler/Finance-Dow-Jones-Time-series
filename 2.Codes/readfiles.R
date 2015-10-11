readfiles<-function(){
    Dow.list<-read.csv("C:/Users/akhilesh/Desktop/Course Materials/ISEN 613/Final Project/2.Excel Data/1.Dow.list.csv")
    Dow.daily<-read.csv("C:/Users/akhilesh/Desktop/Course Materials/ISEN 613/Final Project/2.Excel Data/2.Dow.daily.csv")
    Dow.daily.prcnt<-read.csv("C:/Users/akhilesh/Desktop/Course Materials/ISEN 613/Final Project/2.Excel Data/3.Dow.daily.prcnt.csv")
    Dow.daily.prcnt.std<-read.csv("C:/Users/akhilesh/Desktop/Course Materials/ISEN 613/Final Project/2.Excel Data/4.Dow.daily.prcnt.std.csv")
    Pred.list<-read.csv("C:/Users/akhilesh/Desktop/Course Materials/ISEN 613/Final Project/2.Excel Data/5.Pred.list.csv")
    Pred.daily<-read.csv("C:/Users/akhilesh/Desktop/Course Materials/ISEN 613/Final Project/2.Excel Data/6.Pred.daily.csv")
    Pred.daily.prcnt<-read.csv("C:/Users/akhilesh/Desktop/Course Materials/ISEN 613/Final Project/2.Excel Data/7.Pred.daily.prcnt.csv")
    Pred.daily.prcnt.std<-read.csv("C:/Users/akhilesh/Desktop/Course Materials/ISEN 613/Final Project/2.Excel Data/8.Pred.daily.prcnt.std.csv")
    Predictions<-read.csv("C:/Users/akhilesh/Desktop/Course Materials/ISEN 613/Final Project/2.Excel Data/10.Predictions.csv")
    Predictions<-tail(Predictions,n=2)
}