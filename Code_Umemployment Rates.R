df = read.csv("All_Data.csv")
head(df)
#import library
library(plm)
library(stargazer)
#build BIC function
aicbic_plm <- function(object, criterion) {4
  
  
  # object is "plm", "panelmodel" 
  # Lets panel data has index :index = c("Country", "Time")
  
  sp = summary(object)
  
  if(class(object)[1]=="plm"){
    u.hat <- residuals(sp) # extract residuals
    df <- cbind(as.vector(u.hat), attr(u.hat, "index"))
    names(df) <- c("resid", "Country", "Time")
    c = length(levels(df$Country)) # extract country dimension 
    t = length(levels(df$Time)) # extract time dimension 
    np = length(sp$coefficients[,1]) # number of parameters
    n.N = nrow(sp$model) # number of data
    s.sq <- log( (sum(u.hat^2)/(n.N))) # log sum of squares
    
    # effect = c("individual", "time", "twoways", "nested"),
    # model = c("within", "random", "ht", "between", "pooling", "fd")
    
    
    if (sp$args$model == "within" & sp$args$effect == "individual"){
      n = c
      np = np+n+1 # update number of parameters
    }
    
    if (sp$args$model == "within" & sp$args$effect == "time"){
      T = t
      np = np+T+1 # update number of parameters
    }
    
    if (sp$args$model == "within" & sp$args$effect == "twoways"){
      n = c
      T = t
      np = np+n+T # update number of parameters
    }
    aic <- round( 2*np + n.N * ( log(2*pi) + s.sq + 1 ),1)
    bic <- round(log(n.N)*np + n.N * ( log(2*pi) + s.sq + 1 ),1)
    
    if(criterion=="AIC"){
      names(aic) = "AIC"
      return(aic)
    }
    if(criterion=="BIC"){
      names(bic) = "BIC"
      return(bic)
    }
  }
}
#Compare Ar models
#Ar(1)
ar1 = plm(formula = Unemployment.Rate ~ U_1,data=df,index=c("Country","DATE"))
ar2 = plm(formula = Unemployment.Rate ~ U_1+U_2,data=df,index=c("Country","DATE") )
ar3 = plm(formula = Unemployment.Rate ~U_1+U_2+U_3,data=df,index=c("Country","DATE"))
stargazer(ar1,ar2,ar3,type="text",column.labels = c("AR(1)","AR(2)","AR(3)"),out="armodel.html")
print(c(aicbic_plm(ar1,"BIC"),aicbic_plm(ar2,"BIC"),aicbic_plm(ar3,"BIC")))
#choose Ar(1)
Model1 = plm(formula = Unemployment.Rate ~ U_1+log(Monthly_Cases+1)+Lockdown_Dummy,data=df,
             index=c("Country","DATE") )
Model2 = plm(formula = Unemployment.Rate ~ U_1+log(Monthly_Death+1)+Lockdown_Dummy,data=df,
             index=c("Country","DATE") )
Model3 = plm(formula = Unemployment.Rate ~ U_1+log(Monthly_Cases+1)+log(Monthly_Death+1)+Loc
             kdown_Dummy,data=df,index=c("Country","DATE") )
stargazer(Model1,Model2,Model3,type="text",column.labels = c("Model with Cases","Model with Death","
Model with Both"),out = "compare.html") 
print(c(aicbic_plm(Model1,"BIC"),aicbic_plm(Model2,"BIC"),aicbic_plm(Model3,"BIC")))
print(c(aicbic_plm(ar1,"BIC"),aicbic_plm(ar2,"BIC"),aicbic_plm(ar3,"BIC"))) 
#fix effect
aneweytest(formula = Unemployment.Rate ~ U_1+f(log(Monthly_Cases+1))+Lockdown_Dummy,data=df)
aneweytest(formula = Unemployment.Rate ~ U_1,data=df)
aneweytest(formula = Unemployment.Rate ~ U_1+U_2,data=df)
aneweytest(formula = Unemployment.Rate ~ U_1+U_2+U_3,data=df)
#unit root
purtest(df["Unemployment.Rate"],lag =1)
purtest(df["Unemployment.Rate"],lag =2)
purtest(df["Unemployment.Rate"],lag =3)
purtest(log(df["Monthly_Death"]+1))
#serial correlation
pbgtest(ar1)
pbgtest(ar2)
pbgtest(ar3)