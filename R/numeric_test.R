
library(dplyr)# 이거 있어야함

# integer이면 numeric으로 바꾸고 character와 mumeric변수 알아보기

numeric_test<-function(data){
  colname<-colnames(data)
  data_str<-data.frame(변수명=character()
             ,변수형태=character())
  data_str2<-data.frame(변수명=character()
                          ,변수형태=character())
  data_str$변수명<-as.character(data_str$변수명)
  data_str$변수형태<-as.character(data_str$변수형태)
  data_str2$변수명<-as.character(data_str2$변수명)
  data_str2$변수형태<-as.character(data_str2$변수형태)
  
  for(j in 1:ncol(data)){
    if(is.integer(data[,colname[j]])){
      data[,colname[j]]<-as.numeric(data[,colname[j]])
    }
  }
  
  for(i in 1:ncol(data)){
  if(is.numeric(data[,colname[i]])){
    data_str[i,1]<-colname[i] 
    data_str[i,2]<-"numeric"} else{
      data_str2[i,1]<-colname[i] 
      data_str2[i,2]<-"Factor"  
    }
  }
  na.omit(merge(data_str,data_str2,all=T))
}

