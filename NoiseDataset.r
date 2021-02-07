library(ggpubr)
library(ggplot2)
library(dplyr) #add rows
library(lmridge) #for ridge regression
library(lars) # lasso regression
library(caret) # for cross validation
library(gdata) #to read xls file format
library(rsm) # for response surface

str(Noise)
ns<-Noise
str(ns)
#Forward Selection
fwd_model_noise <- lm(a ~ 1,  data = ns) 
step(fwd_model_noise, direction = "forward", scope = formula(a ~ b + c + d + e + f))

lm(formula = a ~ f + e + c + b + d, data = ns)


#Linear Regression
lin_nos1 <- lm(a ~ f, data = ns)
lin_nos2 <- lm(a ~ f+e, data = ns)
lin_nos3 <- lm(a ~ f+e+c, data = ns)
lin_nos4 <- lm(a ~ f+e+c+b, data = ns)
lin_nos5 <- lm(a ~ f+e+c+b+d, data = ns)


#Cross-Validation
train_control <- trainControl(method = "cv", number = 10)
cv_nsmodel1 <- train(a ~ f, data = ns, trControl = train_control, method = "lm")
cv_nsmodel2 <- train(a ~ f+e, data = ns, trControl = train_control, method = "lm")
cv_nsmodel3 <- train(a ~ f+e+c, data = ns, trControl = train_control, method = "lm")
cv_nsmodel4 <- train(a ~ f+e+c+b, data = ns, trControl = train_control, method = "lm")
cv_nsmodel5 <- train(a ~ f+e+c+b+d, data = ns, trControl = train_control, method = "lm")

#New Data Frames:-
frame_ns<-data.frame("r_sq" = double(0), "adj_r_sq" = double(0), "cv_r_sq" = double(0))
frame_ns<- add_row(frame_ns, r_sq = summary(lin_nos1)$r.squared, adj_r_sq = summary(lin_nos1)$adj.r.squared, cv_r_sq = mean(cv_nsmodel1$resample$Rsquared))
frame_ns<- add_row(frame_ns, r_sq = summary(lin_nos2)$r.squared, adj_r_sq = summary(lin_nos2)$adj.r.squared, cv_r_sq = mean(cv_nsmodel2$resample$Rsquared))
frame_ns<- add_row(frame_ns, r_sq = summary(lin_nos3)$r.squared, adj_r_sq = summary(lin_nos3)$adj.r.squared, cv_r_sq = mean(cv_nsmodel3$resample$Rsquared))
frame_ns<- add_row(frame_ns, r_sq = summary(lin_nos4)$r.squared, adj_r_sq = summary(lin_nos4)$adj.r.squared, cv_r_sq = mean(cv_nsmodel4$resample$Rsquared))
frame_ns<- add_row(frame_ns, r_sq = summary(lin_nos5)$r.squared, adj_r_sq = summary(lin_nos5)$adj.r.squared, cv_r_sq = mean(cv_nsmodel5$resample$Rsquared))

#Plots
plot(frame_ns$r_sq, type = 'l', col = 'red', main = "Linear Plot", ylab = "Errors", ylim = c(0.1,1))
lines(frame_ns$adj_r_sq,  col = 'green' )
lines(frame_ns$cv_r_sq,  col = 'blue')
legend(5,0.8, legend = c("R Squared","Adj R Squared","R Squared CV"), 
       col = c("red","green","blue"), lty = 1:2, cex = 0.8)

#Ridge Regression
ridge_ns1 <- lmridge(a ~ f + e, ns, K = c(0.1, 0.001))
ridge_ns2 <- lmridge(a ~ f + e + c, ns, K = c(0.1, 0.001))
ridge_ns3 <- lmridge(a ~ f + e + c + b, ns, K = c(0.1, 0.001))
ridge_ns4 <- lmridge(a ~ f + e + c + b + d , ns, K = c(0.1, 0.001))

#Cross-Validation On Ridge Regression
cv_ridge_ns1 <- train(a ~ f + e, data = ns, trControl = train_control, method = "ridge")
cv_ridge_ns2 <- train(a ~ f + e + c, data = ns, trControl = train_control, method = "ridge")
cv_ridge_ns3 <- train(a ~ f + e + c + b, data = ns, trControl = train_control, method = "ridge")
cv_ridge_ns4 <- train(a ~ f + e + c + b + d, data = ns, trControl = train_control, method = "ridge")

max(rstats1(ridge_ns1)$R2)

#Data Frame Values 
frame_ns1 <-data.frame("r_sq" = double(0), "adj_r_sq" = double(0), "cv_r_sq" = double(0))
frame_ns1 <- add_row(frame_ns1, r_sq = max(rstats1(ridge_ns1)$R2), adj_r_sq = max(rstats1(ridge_ns1)$adjR2), cv_r_sq = mean(cv_ridge_ns1$resample$Rsquared))
frame_ns1 <- add_row(frame_ns1, r_sq = max(rstats1(ridge_ns2)$R2), adj_r_sq = max(rstats1(ridge_ns2)$adjR2), cv_r_sq = mean(cv_ridge_ns2$resample$Rsquared))
frame_ns1 <- add_row(frame_ns1, r_sq = max(rstats1(ridge_ns3)$R2), adj_r_sq = max(rstats1(ridge_ns3)$adjR2), cv_r_sq = mean(cv_ridge_ns3$resample$Rsquared))
frame_ns1 <- add_row(frame_ns1, r_sq = max(rstats1(ridge_ns4)$R2), adj_r_sq = max(rstats1(ridge_ns4)$adjR2), cv_r_sq = mean(cv_ridge_ns4$resample$Rsquared))

#Plot
plot(frame_ns1$adj_r_sq,type="l",col="red",main = "RidgePlot",ylab="Variation",ylim = c(0,1))
lines(frame_ns1$r_sq,col="green")
lines(frame_ns1$cv_r_sq,col="Blue")
legend(2,1,legend = c("adj_r_square","r_square","cv_r_square"),col=c("red","green","blue"),lty = 1:2,cex = 0.8)

#LassoRegression

x <-(ns$f)
x <-cbind(ns$e,x)
lasso_mod1 <- lars(x, ns$a, type = 'lasso')

x <-(ns$f)
x <-cbind(ns$e,x)
x <-cbind(ns$c,x)
lasso_mod2 <- lars(x, ns$a, type = 'lasso')



x <-(ns$f)
x <-cbind(ns$e,x)
x <-cbind(ns$c,x)
x <-cbind(ns$b,x)
lasso_mod3 <-lars(x,ns$a,type='lasso')

x <-(ns$f)
x <-cbind(ns$e,x)
x <-cbind(ns$c,x)
x <-cbind(ns$b,x)
x <-cbind(ns$d,x)
lasso_mod4 <- lars(x, ns$a, type = 'lasso')

#Cross-Validation
cv_lasso_ns1<-train(a~f+e,ns,method="lasso",trControl=train_control)
cv_lasso_ns2<-train(a~f+e+c,ns,method="lasso",trControl=train_control)
cv_lasso_ns3<-train(a~f+e+c+b,ns,method="lasso",trControl=train_control)
cv_lasso_ns4<-train(a~f+e+c+b+d,ns,method="lasso",trControl=train_control)

library(lmridge)
library(lars)
#DataFrame
frame_ns2 <- data.frame("r_sq" = double(0), "cv_r_sq" = double(0))
frame_ns2 <- add_row(frame_ns2, r_sq =max((lasso_mod1)$R2), cv_r_sq = mean(cv_lasso_ns1$resample$Rsquared))
frame_ns2 <- add_row(frame_ns2, r_sq =max((lasso_mod2)$R2), cv_r_sq = mean(cv_lasso_ns2$resample$Rsquared))
frame_ns2 <- add_row(frame_ns2, r_sq =max((lasso_mod3)$R2), cv_r_sq = mean(cv_lasso_ns3$resample$Rsquared))
frame_ns2 <- add_row(frame_ns2, r_sq =max((lasso_mod4)$R2), cv_r_sq = mean(cv_lasso_ns4$resample$Rsquared))

#Plotting The Values
plot(frame_ns2$r_sq,type="l",col="red",main = "LassoPLOT",ylab="Variation",ylim = c(0,1))
lines(frame_ns2$cv_r_sq,col="Blue")
legend(2,1,legend = c("r_square","cv_r_square"),col=c("red","blue"),lty = 1:2,cex = 0.8)


#Quad Regression

q_nas<-ns

#Getting the Square Of Variables:-
q_nas$f1<-q_nas$f^2
q_nas$e1<-q_nas$e^2
q_nas$c1<-q_nas$c^2
q_nas$b1<-q_nas$b^2
q_nas$d1<-q_nas$d^2

#Linear Regression

Quad1 <- lm(a~f+f1,data=q_nas)
Quad2 <- lm(a~f+f1+e+e1,data=q_nas)
Quad3 <- lm(a~f+f1+e+e1+c+c1,data=q_nas)
Quad4 <- lm(a~f+f1+e+e1+c+c1+b+b1,data=q_nas)
Quad5 <- lm(a~f+f1+e+e1+c+c1+b+b1+d+d1,data=q_nas)


rstats1()
library(lmridge)

#Cross-Validation on Quad Regression
cv_quad_ns1 <- train(a ~ f+f1, data = q_nas, trControl = train_control, method = "lm")
cv_quad_ns2 <- train(a ~ f+f1+e+e1, data = q_nas, trControl = train_control, method = "lm")
cv_quad_ns3 <- train(a ~ f+f1+e+e1+c+c1, data = q_nas, trControl = train_control, method = "lm")
cv_quad_ns4 <- train(a ~ f+f1+e+e1+c+c1+b+b1, data = q_nas, trControl = train_control, method = "lm")
cv_quad_ns5 <- train(a ~ f+f1+e+e1+c+c1+b+b1+d+d1, data = q_nas, trControl = train_control, method = "lm")

#DataFrames
frame_ns3 <- data.frame("adj_r_sq" = double(0),"r_sq" = double(0), "cv_r_sq" = double(0))
frame_ns3<- add_row(frame_ns3, r_sq = summary(Quad1)$r.squared, adj_r_sq = summary(Quad1)$adj.r.squared, cv_r_sq = mean(cv_quad_ns1$resample$Rsquared))
frame_ns3<- add_row(frame_ns3, r_sq = summary(Quad2)$r.squared, adj_r_sq = summary(Quad2)$adj.r.squared, cv_r_sq = mean(cv_quad_ns2$resample$Rsquared))
frame_ns3<- add_row(frame_ns3, r_sq = summary(Quad3)$r.squared, adj_r_sq = summary(Quad3)$adj.r.squared, cv_r_sq = mean(cv_quad_ns3$resample$Rsquared))
frame_ns3<- add_row(frame_ns3, r_sq = summary(Quad4)$r.squared, adj_r_sq = summary(Quad4)$adj.r.squared, cv_r_sq = mean(cv_quad_ns4$resample$Rsquared))

#Plotting The Variables:-

plot(frame_ns3$adj_r_sq,type="l",col="red",main = "QuadPLOT",ylab="Variation",ylim = c(0,1))
lines(frame_ns3$r_sq,col="green")
lines(frame_ns3$cv_r_sq,col="Blue")
legend(2,1,legend = c("adj_r_square","r_square","cv_r_square"),col=c("red","green","blue"),lty = 1:2,cex = 0.8)

#Response Surface
resp_ns<-ns
rs_md1<-rsm(a~SO(f,e),data=resp_ns)
rs_md2<-rsm(a~SO(f,e,c),data=resp_ns)
rs_md3<-rsm(a~SO(f,e,c,b),data=resp_ns)
rs_md4<-rsm(a~SO(f,e,c,b,d),data=resp_ns)


#DataFrame
frame_ns4 <-data.frame("adj_r_square" = double(0), "r_square" = double(0))
frame_ns4 <- add_row(frame_ns4, adj_r_square = summary(rs_md1)$adj.r.squared, r_square = summary(rs_md1)$r.squared)
frame_ns4 <- add_row(frame_ns4, adj_r_square = summary(rs_md2)$adj.r.squared, r_square = summary(rs_md2)$r.squared)
frame_ns4 <- add_row(frame_ns4, adj_r_square = summary(rs_md3)$adj.r.squared, r_square = summary(rs_md3)$r.squared)
frame_ns4 <- add_row(frame_ns4, adj_r_square = summary(rs_md4)$adj.r.squared, r_square = summary(rs_md4)$r.squared)

#Plotting The Variables:-
plot(frame_ns4$adj_r_square,type="l",col="red",main = "ReponseSurfacePLOT",ylab="Variation",ylim = c(0,1))
lines(frame_ns4$r_square,col="green")
legend(2,1,legend = c("adj_r_square","r_square"),col=c("red","green"),lty = 1:2,cex = 0.8)

