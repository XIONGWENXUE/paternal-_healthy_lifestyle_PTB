
#### gestational age model###
Coefc <- function(Obj){
  coef <- coeftest(Obj)
  coef<-as.data.frame(coef[c(1:3),c(1:3)])
  coef$`exp(B)` <- coef[,1]
  coef$`95%CIlower` <- coef[,1]-1.96*coef[,2]
  coef$`95%CIupper` <- coef[,1]+1.96*coef[,2]
  coef <- round(coef,2)
  coef$OR.CI<-paste(paste(paste(coef$`exp(B)`,
                                coef$`95%CIlower`, sep = ' ('), coef$`95%CIupper`,sep=" to "),
                    ')', sep = '')
  return(coef)
}
model<-with(n,lm(as.numeric (gestationalage) ~as.factor(Mscore)))
Coefc(model)#paternal crude model
model<-with(n,lm(as.numeric (gestationalage) ~as.numeric(Mscore)))
summary(model)#calculate p for trend

model<-with(n,lm(as.numeric (gestationalage) ~as.factor(Wscore)))
Coefc(model)#maternal crude model
model<-with(n,lm(as.numeric (gestationalage) ~as.numeric(Wscore)))
summary(model)#calculate p for trend

#paternal Multivariate model
model<-with(n,lm(as.numeric (gestationalage) ~as.factor(Mscore)+Wscore+
                   Wage+diabetes+WHTN+STI+firstgestation+history+location+stress))
Coefc(model)
model<-with(n,lm(as.numeric (gestationalage) ~as.numeric(Mscore)+Wscore+
                   Wage+diabetes+WHTN+STI+firstgestation+history+location+stress))
summary(model)
model<-with(n,lm(as.numeric (gestationalage) ~as.factor(Mscore)+as.numeric(Wscore)+
                   Wage+diabetes+WHTN+STI+firstgestation+history+location+stress))
summary(model)


#paternal Multivariate model with IPTW
m<-n[,c("Mscore","Wscore","gestationalage",
        "Wage","diabetes","WHTN","firstgestation","history","location","stress","STI")]
m<-na.omit(m)
library(WeightIt)
PS.fmu<-as.factor(Mscore)~Wscore+
  Wage+diabetes+WHTN+STI+firstgestation+history+location+stress
Wtit<-weightit(formula=PS.fmu,data=m,method="ps",link="logit",stabilize=TRUE)
model<-with(m,lm(as.numeric (gestationalage) ~as.factor(Mscore),
                 weights =Wtit$weights))
Coefc(model)

model<-with(m,lm(as.numeric (gestationalage) ~as.factor(Wscore),
                 weights =Wtit$weights))
Coefc(model)