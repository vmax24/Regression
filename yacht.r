library(ggpubr)
library(ggplot2)
library(dplyr) #add rows
library(lmridge) #for ridge regression
library(lars) # lasso regression
library(caret) # for cross validation
library(gdata) #to read xls file format
library(rsm) # for response surface

yacht <- read.csv('C:/Users/rsdgr/OneDrive/Desktop/Data Science/Datasets/Yacht/yacht_hydrodynamics1.csv')
str(yacht)

#Forward Selection
fwd_model_yacht <- lm(a ~ 1,  data = yacht) 
step(fwd_model_yacht, direction = "forward", scope = formula(a ~ b + c + d + e + f))

#Linear Regression
lin_ya1  <- lm(a ~ b, data = yacht)
lin_ya2  <- lm(a ~ b+c, data = yacht)
lin_ya3  <- lm(a ~ b+c+d, data = yacht)
lin_ya4  <- lm(a ~ b+c+d+e, data = yacht)
lin_ya5  <- lm(a ~ b+c+d+e+f, data = yacht)

#Cross-Validation
train_control <- trainControl(method = "cv", number = 10)
cv_yamodel1 <- train(a ~ b, data = ns, trControl = train_control, method = "lm")
cv_yamodel2 <- train(a ~ b+c, data = ns, trControl = train_control, method = "lm")
cv_yamodel3 <- train(a ~ b+c+d, data = ns, trControl = train_control, method = "lm")
cv_yamodel4 <- train(a ~ b+c+d+e, data = ns, trControl = train_control, method = "lm")
cv_yamodel5 <- train(a ~ b+c+d+e+f, data = ns, trControl = train_control, method = "lm")

#Dataframe
frame_ya<-data.frame("r_sq" = double(0), "adj_r_sq" = double(0), "cv_r_sq" = double(0))
frame_ya<- add_row(frame_ya, r_sq = summary(lin_ya1)$r.squared, adj_r_sq = summary(lin_ya1)$adj.r.squared, cv_r_sq = mean(cv_yamodel1$resample$Rsquared))
frame_ya<- add_row(frame_ya, r_sq = summary(lin_ya2)$r.squared, adj_r_sq = summary(lin_ya2)$adj.r.squared, cv_r_sq = mean(cv_yamodel2$resample$Rsquared))
frame_ya<- add_row(frame_ya, r_sq = summary(lin_ya3)$r.squared, adj_r_sq = summary(lin_ya3)$adj.r.squared, cv_r_sq = mean(cv_yamodel3$resample$Rsquared))
frame_ya<- add_row(frame_ya, r_sq = summary(lin_ya4)$r.squared, adj_r_sq = summary(lin_ya4)$adj.r.squared, cv_r_sq = mean(cv_yamodel4$resample$Rsquared))
frame_ya<- add_row(frame_ya, r_sq = summary(lin_ya5)$r.squared, adj_r_sq = summary(lin_ya5)$adj.r.squared, cv_r_sq = mean(cv_yamodel5$resample$Rsquared))

#Plotting The Variables:-
plot(frame_ya$r_sq, type = 'l', col = 'red', main = "Linear Plot", ylab = "Errors", ylim = c(0.1,1))
lines(frame_ya$adj_r_sq,  col = 'green' )
lines(frame_ya$cv_r_sq,  col = 'blue')
legend(2,0.8, legend = c("R Squared","Adj R Squared","R Squared CV"), 
       col = c("red","green","blue"), lty = 1:2, cex = 0.8)


#Ridge Regression
ridge_ya1 <- lmridge(a ~ b + c, yacht, K = c(0.1, 0.001))
ridge_ya2 <- lmridge(a ~ b + c + d,yacht, K = c(0.1, 0.001))
ridge_ya3 <- lmridge(a ~ b + c + d + e, yacht, K = c(0.1, 0.001))
ridge_ya4 <- lmridge(a ~ b + c + d + e + f , yacht, K = c(0.1, 0.001))

#Cross-Validation 
cv_ridge_ya1 <- train(a ~ b + c, data = yacht, trControl = train_control, method = "ridge")
cv_ridge_ya2 <- train(a ~ b + c +d, data = yacht, trControl = train_control, method = "ridge")
cv_ridge_ya3 <- train(a ~ b + c+d+e, data = yacht, trControl = train_control, method = "ridge")
cv_ridge_ya4 <- train(a ~ b + c+d+e+f, data = yacht, trControl = train_control, method = "ridge")

#DataFrames:-
frame_ya1 <-data.frame("r_sq" = double(0), "adj_r_sq" = double(0), "cv_r_sq" = double(0))
frame_ya1 <- add_row(frame_ya1, r_sq = max(rstats1(ridge_ya1)$R2), adj_r_sq = max(rstats1(ridge_ya1)$adjR2), cv_r_sq = mean(cv_ridge_ya1$resample$Rsquared))
frame_ya1 <- add_row(frame_ya1, r_sq = max(rstats1(ridge_ya2)$R2), adj_r_sq = max(rstats1(ridge_ya2)$adjR2), cv_r_sq = mean(cv_ridge_ya2$resample$Rsquared))
frame_ya1 <- add_row(frame_ya1, r_sq = max(rstats1(ridge_ya3)$R2), adj_r_sq = max(rstats1(ridge_ya3)$adjR2), cv_r_sq = mean(cv_ridge_ya3$resample$Rsquared))
frame_ya1 <- add_row(frame_ya1, r_sq = max(rstats1(ridge_ya4)$R2), adj_r_sq = max(rstats1(ridge_ya4)$adjR2), cv_r_sq = mean(cv_ridge_ya4$resample$Rsquared))

#Plotting The Values
plot(frame_ya1$adj_r_sq,type="l",col="red",main = "RidgePlot",ylab="Variation",ylim = c(0,1))
lines(frame_ya1$r_sq,col="green")
lines(frame_ya1$cv_r_sq,col="Blue")
legend(2,1,legend = c("adj_r_square","r_square","cv_r_square"),col=c("red","green","blue"),lty = 1:2,cex = 0.8)


#LassoRegression

x <-(yacht$b)
x <-cbind(yacht$c,x)
lasso_mod1 <- lars(x, yacht$a, type = 'lasso')


x <-(yacht$b)
x <-cbind(yacht$c,x)
x <-cbind(yacht$d,x)
lasso_mod2 <- lars(x, yacht$a, type = 'lasso')

x <-(yacht$b)
x <-cbind(yacht$c,x)
x <-cbind(yacht$d,x)
x <-cbind(yacht$e,x)
lasso_mod3 <- lars(x, yacht$a, type = 'lasso')

x <-(yacht$b)
x <-cbind(yacht$c,x)
x <-cbind(yacht$d,x)
x <-cbind(yacht$e,x)
x <-cbind(yacht$f,x)
lasso_mod4 <- lars(x, yacht$a, type = 'lasso')

#Cross-Validation
cv_lasso_ya1<-train(a~b+c,yacht,method="lasso",trControl=train_control)
cv_lasso_ya2<-train(a~b+c+d,yacht,method="lasso",trControl=train_control)
cv_lasso_ya3<-train(a~b+c+d+e,yacht,method="lasso",trControl=train_control)
cv_lasso_ya4<-train(a~b+c+d+e+f,yacht,method="lasso",trControl=train_control)

#DataFrames:-
frame_ya2 <- data.frame("r_sq" = double(0), "cv_r_sq" = double(0))
frame_ya2 <- add_row(frame_ya2, r_sq =max((lasso_mod1)$R2), cv_r_sq = mean(cv_lasso_ya1$resample$Rsquared))
frame_ya2 <- add_row(frame_ya2, r_sq =max((lasso_mod2)$R2), cv_r_sq = mean(cv_lasso_ya2$resample$Rsquared))
frame_ya2 <- add_row(frame_ya2, r_sq =max((lasso_mod3)$R2), cv_r_sq = mean(cv_lasso_ya3$resample$Rsquared))
frame_ya2 <- add_row(frame_ya2, r_sq =max((lasso_mod4)$R2), cv_r_sq = mean(cv_lasso_ya4$resample$Rsquared))

#PlottingTheValues
plot(frame_ya2$r_sq,type="l",col="red",main = "LassoPLOT",ylab="Variation",ylim = c(0,1))
lines(frame_ya2$cv_r_sq,col="Blue")
legend(2,1,legend = c("r_square","cv_r_square"),col=c("red","blue"),lty = 1:2,cex = 0.8)

#Quad Regression
ys<-yacht
ys$f1<-ys$f^2
ys$e1<-ys$e^2
ys$c1<-ys$c^2
ys$b1<-ys$b^2
ys$d1<-ys$d^2
#Linear Regression
Quad1 <- lm(a~b+b1,data=ys)
Quad2 <- lm(a~b+b1+c+c1,data=ys)
Quad3 <- lm(a~b+b1+c+c1+d+d1,data=ys)
Quad4 <- lm(a~b+b1+c+c1+d+d1+e+e1,data=ys)
Quad5 <- lm(a~b+b1+c+c1+d+d1+e+e1+f+f1,data=ys)

#Cross-Validation
cv_quad_ya1 <- train(a ~ b+b1, data = ys, trControl = train_control, method = "lm")
cv_quad_ya2 <- train(a ~ b+b1+c+c1, data = ys, trControl = train_control, method = "lm")
cv_quad_ya3 <- train(a ~ b+b1+c+c1+d+d1, data = ys, trControl = train_control, method = "lm")
cv_quad_ya4 <- train(a ~ b+b1+c+c1+d+d1+e+e1, data = ys, trControl = train_control, method = "lm")
cv_quad_ya5 <- train(a ~ b+b1+c+c1+d+d1+e+e1+f+f1, data = ys, trControl = train_control, method = "lm")

#DataFrames
frame_ya3 <- data.frame("adj_r_sq" = double(0),"r_sq" = double(0), "cv_r_sq" = double(0))
frame_ya3<- add_row(frame_ya3, r_sq = summary(Quad1)$r.squared, adj_r_sq = summary(Quad1)$adj.r.squared, cv_r_sq = mean(cv_quad_ya1$resample$Rsquared))
frame_ya3<- add_row(frame_ya3, r_sq = summary(Quad2)$r.squared, adj_r_sq = summary(Quad2)$adj.r.squared, cv_r_sq = mean(cv_quad_ya2$resample$Rsquared))
frame_ya3<- add_row(frame_ya3, r_sq = summary(Quad3)$r.squared, adj_r_sq = summary(Quad3)$adj.r.squared, cv_r_sq = mean(cv_quad_ya3$resample$Rsquared))
frame_ya3<- add_row(frame_ya3, r_sq = summary(Quad4)$r.squared, adj_r_sq = summary(Quad4)$adj.r.squared, cv_r_sq = mean(cv_quad_ya4$resample$Rsquared))

#PlottingTheValue
plot(frame_ya3$adj_r_sq,type="l",col="red",main = "QuadPLOT",ylab="Variation",ylim = c(0,1))
lines(frame_ya3$r_sq,col="green")
lines(frame_ya3$cv_r_sq,col="Blue")
legend(2,1,legend = c("adj_r_square","r_square","cv_r_square"),col=c("red","green","blue"),lty = 1:2,cex = 0.8)

#Response Surface
resp_ya<-yacht

rs_ya1<-rsm(a~SO(b,c),data=resp_ya)
rs_ya2<-rsm(a~SO(b,c,d),data=resp_ya)
rs_ya3<-rsm(a~SO(b,c,d,e),data=resp_ya)
rs_ya4<-rsm(a~SO(b,c,d,e,f),data=resp_ya)

#Cross-Validation
frame_ya4 <-data.frame("adj_r_square" = double(0), "r_square" = double(0))
frame_ya4 <- add_row(frame_ya4, adj_r_square = summary(rs_ya1)$adj.r.squared, r_square = summary(rs_ya1)$r.squared)
frame_ya4 <- add_row(frame_ya4, adj_r_square = summary(rs_ya2)$adj.r.squared, r_square = summary(rs_ya2)$r.squared)
frame_ya4 <- add_row(frame_ya4, adj_r_square = summary(rs_ya3)$adj.r.squared, r_square = summary(rs_ya3)$r.squared)
frame_ya4 <- add_row(frame_ya4, adj_r_square = summary(rs_ya4)$adj.r.squared, r_square = summary(rs_ya4)$r.squared)

#Plotting The Values
plot(frame_ya4$adj_r_square,type="l",col="red",main = "ReponseSurfacePLOT",ylab="Variation",ylim = c(0,1))
lines(frame_ya4$r_square,col="green")
legend(2,1,legend = c("adj_r_square","r_square"),col=c("red","green"),lty = 1:2,cex = 0.8)




