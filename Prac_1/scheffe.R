# calculated using the formula in http://lycofs01.lycoming.edu/~sprgene/M123/Text/UNIT_28.pdf
scheffe=function(data,y,factor1,alpha){
  anova1way=aov(y~factor1,data=data)
  names=levels(factor1)
  dim.factor1=dim(table(factor1))
  qf.critical=qf((1-alpha),(dim.factor1-1),(dim(data)[1]-dim.factor1))
  MSE=sum(anova1way$residuals^2)/(dim(data)[1]-dim.factor1)
  mean.f1=tapply(y,list(factor1),mean)
  diff1=matrix(0,nrow=dim.factor1,ncol=dim.factor1)
  dimnames(diff1)=list(levels(factor1),levels(factor1))
  scheffevalue=matrix(0,nrow=dim.factor1,ncol=dim.factor1)
  dimnames(scheffevalue)=list(levels(factor1),levels(factor1))
  decision=matrix(0,nrow=dim.factor1,ncol=dim.factor1)
  dimnames(decision)=list(levels(factor1),levels(factor1))
  
  for(i in 1:dim.factor1)
  {
    for(j in 1:dim.factor1)
    {
      diff1[i,j]=abs(mean.f1[i]-mean.f1[j])
      
      scheffevalue[i,j]=sqrt((dim.factor1-1)*qf.critical*MSE*((1/table(factor1)[i])+(1/table(factor1)[j])))
      decision[i,j]=diff1[i,j]-scheffevalue[i,j]
      if(decision[i,j]>0)
      {
        decision[i,j]=c("reject")
      } else {
        decision[i,j]=c("accept")
      }
    }
  }
  result <- list(decision=decision,meandifferences=diff1,Scheffevalues=scheffevalue)  
  return(result)
}

# usage: scheffe(shareprices,shareprice,sector,0.05)
