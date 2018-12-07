library(WindR)
w.start(showmenu = F)
setwd("D:/Users/admin/Documents/repo")
repo<-function(startdate1,enddate1,startdate2,enddate2){
  startdate1<-as.Date(startdate1)
  enddate1<-as.Date(enddate1)
  startdate2<-as.Date(startdate2)
  enddate2<-as.Date(enddate2)
  Rcodes<-c("131810.SZ","131811.SZ","131800.SZ","131809.SZ","131801.SZ","131802.SZ","131803.SZ","131805.SZ","131806.SZ")
  Rnames<-c("R001","R002","R003","R004","R007","R014","R028","R091","R182")
  result<-data.frame(Rnames,"last","now","increase",stringsAsFactors = F)
  last1<-paste0(format(startdate1,format="%Y-%m"),"加权平均成本")
  now1<-paste0(format(startdate2,format="%Y-%m"),"加权平均成本")
  increase1<-paste0(format(startdate2,format="%Y-%m"),"环比变化幅度")
  colnames(result)<-c("证券代码",last1,now1,increase1)
  for(i in 1:length(Rcodes)){
    x1<-w.wsd(Rcodes[i],"volume,vwap",startdate1,enddate1,"TradingCalendar=SZSE")
    y1<-x1$Data
    z1<-sum(y1[,2]*y1[,3])/sum(y1[,2])
    z1<-round(z1,4)
    result[i,2]<-z1
    x2<-w.wsd(Rcodes[i],"volume,vwap",startdate2,enddate2,"TradingCalendar=SZSE")
    y2<-x2$Data
    z2<-sum(y2[,2]*y2[,3])/sum(y2[,2])
    z2<-round(z2,4)
    result[i,3]<-z2
    z3<-(z2-z1)*100/z1
    z3<-paste0(round(z3,2),"%")
    result[i,4]<-z3
  }
return(result)
}
Wrate<-repo(startdate1="2018-10-01",enddate1="2018-10-31",startdate2="2018-11-01",enddate2="2018-11-30")
write.csv(Wrate,file = "repo.csv")
