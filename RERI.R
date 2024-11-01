####RERI####
n$Wscore01<-ifelse(n$Wscore01=="1","0","1")
n$Mscore01<-ifelse(n$Mscore01=="1","0","1")

n$M_W<-ifelse(n$Wscore01=="0" & n$Mscore01=="0","W0M0",
              ifelse(n$Wscore01=="0" & n$Mscore01=="1","W0M1",
                     ifelse(n$Wscore01=="1" & n$Mscore01=="0","W1M0","W1M1")))

n<-within(n,M_W<-relevel(as.factor(M_W), ref = "W0M0"))

model<-with(n,glm(as.numeric (PTB) ~M_W+
                    Wage+diabetes+WHTN+STI+firstgestation+history+location+stress,
                  family = poisson(log)))

interac<-function(Iglm){# OＲand 95% CI#
  x <-summary(Iglm) $ coefficients
  df <-Iglm$df.residual
  left<-exp(x[,1]-x[,2]*qt(1-0.05/2,df))
  right<-exp(x[,1]+x[,2]*qt(1-0.05/2,df))
  OR<-exp(x[,1])
  rowname<-dimnames(x)[[1]]
  colname<-c("Estimate","OR","95LOW","95UPPER","P")
  out<-matrix(c(x[,1],OR,left,right,x[,4]),ncol = 5,dimnames = list(rowname,colname))
  cor<-vcov(Iglm)
  #REＲI and 95% CI#
  RERI<-out[4,2]-out[2,2]-out[3,2]+1
  hr1<-(-out[3,2])
  hr2<-(-out[2,2])
  hr3<-out[4,2]
  SERERI<-sqrt(hr1^2*cor[3,3]+hr2^2*cor[2,2]+hr3^2*cor[4,4]
               +2*hr1*hr2*cor[2,3]+2*hr1*hr3*cor[3,4]+2*hr3*hr2*cor[2,4])
  RERI.low<-RERI-1.96*SERERI
  RERI.up<-RERI+1.96*SERERI
  #AP and  95% CI#
  ha1<-(-(out[3,2]/out[4,2]))
  ha2<-(-(out[2,2]/out[4,2]))
  ha3<-(out[3,2]+out[2,2]-1)/out[4,2]
  SEAP<-sqrt(ha1^2*cor[3,3]+ha2^2*cor[2,2]+ha3^2*cor[4,4]
             +2*ha1*ha2*cor[2,3]+2*ha1*ha3*cor[3,4]+2*ha3*ha2*cor[2,4])
  AP<-RERI/out[4,2]
  AP.low<-AP-1.96*SEAP
  AP.up<-AP+1.96*SEAP
  #SI and  95% CI#
  SI<-(out[4,2]-1)/(out[2,2]+out[3,2]-2)
  hs1<-(-out[3,2]/(out[3,2]+out[2,2]-2))
  hs2<-(-out[2,2]/(out[3,2]+out[2,2]-2))
  hs3<-(out[4,2]/(out[4,2]-1))
  SESI<-sqrt(hs1^2*cor[3,3]+hs2^2*cor[2,2]+hs3^2*cor[4,4]
             +2*hs1*hs2*cor[2,3]+2*hs1*hs3*cor[3,4]+2*hs3*hs2*cor[2,4])
  SI.low<-exp(log(SI)-1.96*SESI)
  SI.up<-exp(log(SI)+1.96*SESI)
  reri<-data.frame(RERI,RERI.low,RERI.up)
  ap<-data.frame(AP,AP.low,AP.up)
  si<-data.frame(SI,SI.low,SI.up)

  bar_d<-matrix(c(1,1,1,1,(out[2,2]-1),0,(out[2,2]-1)
                  ,0,(out[3,2]-1),(out[3,2]-1),0,0,RERI,
                  0,0,0),c(4,4),byrow = T,dimnames = list(c("U","B","A","A&B"),
                                                          c("OR_A1B1","OR_A1B0","OR_A0B1","OR_AOBO")))
  plot<-barplot(bar_d,legend=rownames(bar_d))
  LIST<-list(out,reri,ap,si)
  return(LIST)
  plot()
}


interac(model)
n$M_W01<-ifelse(n$M_W=="W0M0",0,1)
model<-with(n,glm(as.numeric (PTB) ~M_W01+
                    Wage+diabetes+WHTN+STI+firstgestation+history+location+stress,
                  family = poisson(log)))
Coef(model)
fit.std<-stdGlm(fit = model,data=n,X="M_W01",x=c(NA,0))
summary(fit.std)
AF(fit.std$est)
confint(object = fit.std,fun=AF,level = 0.95)