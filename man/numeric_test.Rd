\name{numeric_test}
\alias{numeric_test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{value type test
}
\description{
%%  ~~ na.omit(merge(data_str,data_str2,all=T)) ~~
}
\usage{
numeric_test(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
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

}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
whale
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
