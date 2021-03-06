#' Check variable type
#'
#' If it is an integer, change it to numeric and check the character and numeric variables
#' @param data input data
#' @return You can check the type for each variable.
#' @examples
#' numeric_test(datasetname)
#' @export
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
