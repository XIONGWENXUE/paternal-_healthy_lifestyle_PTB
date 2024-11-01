###modified Poisson regression models####

library(sandwich)
library(lmtest)
Coef <- function(Obj){
  coef <- coeftest(Obj, vcov = vcovHC(Obj,type="HC1"),sandwich = TRUE)
  coef<-as.data.frame(coef[c(1:3),c(1:3)])
  coef$`exp(B)` <- exp(coef[,1])
  coef$`95%CIlower` <- exp(coef[,1]-1.96*coef[,2])
  coef$`95%CIupper` <- exp(coef[,1]+1.96*coef[,2])
  coef <- round(coef,2)
  coef$OR.CI<-paste(paste(paste(coef$`exp(B)`,
                                coef$`95%CIlower`, sep = ' ('), coef$`95%CIupper`,sep="–"),
                    ')', sep = '')
  return(coef)
}

#PTB
model<-with(n,glm(as.numeric (PTB) ~as.factor(Mscore),
                  family = poisson(log)))
Coef(model)#paternal crude model
model<-with(n,glm(as.numeric (PTB) ~as.numeric(Mscore),
                  family = poisson(log)))
summary(model)#calculate p for trend

model<-with(n,glm(as.numeric (PTB) ~as.factor(Wscore),
                  family = poisson(log)))
Coef(model)#maternal crude model
summary(model)
model<-with(n,glm(as.numeric (PTB) ~as.numeric(Wscore),
                  family = poisson(log)))
summary(model)


model<-with(n,glm(as.numeric (PTB) ~as.factor(Mscore)+Wscore+
                    Wage+diabetes+WHTN+STI+firstgestation+history+location+stress,
                  family = poisson(log))) #paternal Multivariate model
Coef(model)
model<-with(n,glm(as.numeric (PTB) ~as.factor(Mscore):Wscore+
                    Wage+diabetes+WHTN+STI+firstgestation+history+location+stress,
                  family = poisson(log)))#maternal Multivariate model
Coef(model)
summary(model)
library(car)
vif(model)#VIF

model<-with(n,glm(as.numeric (PTB) ~as.numeric(Mscore) +Wscore+
                    Wage+diabetes+WHTN+STI+firstgestation+history+location+stress,
                  family = poisson(log)))#calculate p for trend
summary(model)
model<-with(n,glm(as.numeric (PTB) ~as.factor(Mscore)+as.numeric(Wscore)+
                    Wage+diabetes+WHTN+STI+firstgestation+history+location+stress,
                  family = poisson(log)))#calculate p for trend
summary(model)

##### paternal Multivariate model with IPTW
m<-n[,c("Mscore","Wscore","PTB",
        "Wage","diabetes","WHTN","firstgestation","history","location","stress","STI")]
m<-na.omit(m)
library(WeightIt)
PS.fmu<-as.factor(Mscore)~Wscore+
  Wage+diabetes+WHTN+STI+firstgestation+history+location+stress
Wtit<-weightit(formula=PS.fmu,data=m,method="ps",link="logit",stabilize=TRUE)
model<-with(m,glm(as.numeric (PTB) ~as.factor(Mscore),
                  family = poisson(log),weights =Wtit$weights)) 

Coef(model)
model<-with(m,glm(as.numeric (PTB) ~as.numeric (Mscore),
                  family = poisson(log),weights =Wtit$weights))

summary(model)

##### maternal Multivariate model with IPTW
m<-n[,c("Mscore","Wscore","PTB","gestationalage",
        "Wage","diabetes","WHTN","firstgestation","history","location","stress","STI")]
m<-na.omit(m)
library(WeightIt)
PS.fmu<-as.factor(Wscore)~Mscore+
  Wage+diabetes+WHTN+STI+firstgestation+history+location+stress
Wtit<-weightit(formula=PS.fmu,data=m,method="ps",link="logit",stabilize=TRUE)
model<-with(m,glm(as.numeric (PTB) ~as.factor(Wscore),
                  family = poisson(log),weights =Wtit$weights))

Coef(model)
model<-with(m,glm(as.numeric (PTB) ~as.numeric (Mscore),
                  family = poisson(log),weights =Wtit$weights))

summary(model)
