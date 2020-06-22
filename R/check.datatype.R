#'
#'
#' If it is an integer, change it to numeric and check the character and numeric variables
#' After roading the data, you must name the data as 'data'!!!!
#'
#' @param data input data
#'
#' @return You can check the type for each variable.
#'
#' @examples
#' check.datatype()
#' enter : data
#' enter : colname
#'
#' @export
check.datatype <- function(){
  print("Please enter a data name")
  dataname<-scan( what = 'character',n=1)
  dataset<-get(dataname)
  colname<-colnames(data)
  for(j in 1:ncol(dataset)){
    if(is.integer(dataset[,colname[j]])){
      dataset[,colname[j]]<-as.numeric(dataset[,colname[j]])
    }
  }
  print("Enter the name(or colnumber) of variables")
  colnumber<-scan(what = 'character')
  for(i in colnumber){
    if(typeof(dataset[,i])=="double"){
      type <- "numeric"
      valuename<-colnames(dataset[i])
    }else if(is.factor(dataset[,i])){
      type <- "factor"
      valuename<-colnames(dataset[i])
    }else{
      type <- "charactor"
      valuename<-colnames(dataset[i])
    }
    cat("The" ,valuename ," variable type is",type,"\n")
  }
}

