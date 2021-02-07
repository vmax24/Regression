library(ggpubr)
library(ggplot2)
library(dplyr) #add rows
library(lmridge) #for ridge regression
library(lars) # lasso regression
library(caret) # for cross validation
library(gdata) #to read xls file format
library(rsm) # for response surface





auto<-read.csv(file.choose(),header = T)
str(auto)


#MeanImputation For Filling out the missing values
auto$mpg[is.na(auto$mpg)] <- mean(auto$mpg, na.rm = TRUE)
auto$cyl[is.na(auto$cyl)] <- mean(auto$cyl, na.rm = TRUE)
auto$hp[is.na(auto$hp)] <- mean(auto$hp, na.rm = TRUE)
auto$wgt[is.na(auto$wgt)] <- mean(auto$wgt, na.rm = TRUE)
auto$acc[is.na(auto$acc)] <- mean(auto$acc, na.rm = TRUE)
auto$year[is.na(auto$year)] <- mean(auto$year, na.rm = TRUE)
auto$disp[is.na(auto$disp)] <- mean(auto$disp, na.rm = TRUE)

#Forward Selection
names(auto)<-c("mpg","cyl","hp","wgt","acc","year","disp")
str(auto)

#Forward Selection
fwd_model_auto <- lm(mpg ~ 1,  data = auto) 
step(fwd_model_auto, direction = "forward", scope = formula(mpg ~ cyl + hp + wgt + acc + year+disp))

#lm(formula = mpg ~ wgt + year+hp+cyl+acc+disp, data = auto)

#Linear Regression
lin_auto1 <- lm(mpg ~ wgt, data = auto)
lin_auto2 <- lm(mpg ~ wgt+year, data = auto)
lin_auto3 <- lm(mpg ~ wgt+year+hp, data = auto)
lin_auto4 <- lm(mpg ~ wgt+year+hp+cyl, data = auto)
lin_auto5 <- lm(mpg ~ wgt+year+hp+cyl+acc, data = auto)
lin_auto6 <- lm(mpg ~ wgt+year+hp+cyl+acc+disp, data = auto)

#Cross-Validation
#lm(formula = mpg ~ wgt + year+hp+cyl+acc+disp, data = auto)
train_control <- trainControl(method = "cv", number = 10)
cv_nsmodel1 <- train(mpg ~ wgt, data = auto, trControl = train_control, method = "lm")
cv_nsmodel2 <- train(mpg ~ wgt+year, data = auto, trControl = train_control, method = "lm")
cv_nsmodel3 <- train(mpg ~ wgt+year+hp, data = auto, trControl = train_control, method = "lm")
cv_nsmodel4 <- train(mpg ~ wgt+year+hp+cyl, data = auto, trControl = train_control, method = "lm")
cv_nsmodel5 <- train(mpg ~ wgt+year+hp+cyl+acc, data = auto, trControl = train_control, method = "lm")
cv_nsmodel6 <- train(mpg ~ wgt+year+hp+cyl+acc+disp, data = auto, trControl = train_control, method = "lm")


#DataFrame
frame_ns<-data.frame("r_sq" = double(0), "adj_r_sq" = double(0), "cv_r_sq" = double(0))
frame_ns<- add_row(frame_ns, r_sq = summary(lin_auto1)$r.squared, adj_r_sq = summary(lin_auto1)$adj.r.squared, cv_r_sq = mean(cv_nsmodel1$resample$Rsquared))
frame_ns<- add_row(frame_ns, r_sq = summary(lin_auto2)$r.squared, adj_r_sq = summary(lin_auto2)$adj.r.squared, cv_r_sq = mean(cv_nsmodel2$resample$Rsquared))
frame_ns<- add_row(frame_ns, r_sq = summary(lin_auto3)$r.squared, adj_r_sq = summary(lin_auto3)$adj.r.squared, cv_r_sq = mean(cv_nsmodel3$resample$Rsquared))
frame_ns<- add_row(frame_ns, r_sq = summary(lin_auto4)$r.squared, adj_r_sq = summary(lin_auto4)$adj.r.squared, cv_r_sq = mean(cv_nsmodel4$resample$Rsquared))
frame_ns<- add_row(frame_ns, r_sq = summary(lin_auto5)$r.squared, adj_r_sq = summary(lin_auto5)$adj.r.squared, cv_r_sq = mean(cv_nsmodel5$resample$Rsquared))
frame_ns<- add_row(frame_ns, r_sq = summary(lin_auto6)$r.squared, adj_r_sq = summary(lin_auto6)$adj.r.squared, cv_r_sq = mean(cv_nsmodel6$resample$Rsquared))

#Plotting The Values

plot(frame_ns$r_sq, type = 'l', col = 'red', main = "Linear Plot", ylab = "Errors", ylim = c(0.1,1))
lines(frame_ns$adj_r_sq,  col = 'green' )
lines(frame_ns$cv_r_sq,  col = 'blue')
legend(5,0.8, legend = c("R Squared","Adj R Squared","R Squared CV"), 
       col = c("red","green","blue"), lty = 1:2, cex = 0.8)

#Ridge Regression
#mpg ~ wgt+year+hp+cyl+acc+disp
ridge_ns1 <- lmridge(mpg ~ wgt + year, auto, K = c(0.1, 0.001))
ridge_ns2 <- lmridge(mpg ~ wgt + year+hp, auto, K = c(0.1, 0.001))
ridge_ns3 <- lmridge(mpg ~ wgt + year+hp+cyl, auto, K = c(0.1, 0.001))
ridge_ns4 <- lmridge(mpg ~ wgt + year+hp+cyl+acc, auto, K = c(0.1, 0.001))
ridge_ns5<- lmridge(mpg ~ wgt + year+hp+cyl+acc+disp, auto, K = c(0.1, 0.001))

#Cross-Validation
cv_ridge_ns1 <- train(mpg ~ wgt + year, data = auto, trControl = train_control, method = "ridge")
cv_ridge_ns2 <- train(mpg ~ wgt + year+hp, data = auto, trControl = train_control, method = "ridge")
cv_ridge_ns3 <- train(mpg ~ wgt + year+hp+cyl, data = auto, trControl = train_control, method = "ridge")
cv_ridge_ns4 <- train(mpg ~ wgt + year+hp+cyl+acc, data = auto, trControl = train_control, method = "ridge")
cv_ridge_ns5 <- train(mpg ~ wgt + year+hp+cyl+acc+disp, data = auto, trControl = train_control, method = "ridge")

#DataFrames
frame_ns1 <-data.frame("r_sq" = double(0), "adj_r_sq" = double(0), "cv_r_sq" = double(0))
frame_ns1 <- add_row(frame_ns1, r_sq = max(rstats1(ridge_ns1)$R2), adj_r_sq = max(rstats1(ridge_ns1)$adjR2), cv_r_sq = mean(cv_ridge_ns1$resample$Rsquared))
frame_ns1 <- add_row(frame_ns1, r_sq = max(rstats1(ridge_ns2)$R2), adj_r_sq = max(rstats1(ridge_ns2)$adjR2), cv_r_sq = mean(cv_ridge_ns2$resample$Rsquared))
frame_ns1 <- add_row(frame_ns1, r_sq = max(rstats1(ridge_ns3)$R2), adj_r_sq = max(rstats1(ridge_ns3)$adjR2), cv_r_sq = mean(cv_ridge_ns3$resample$Rsquared))
frame_ns1 <- add_row(frame_ns1, r_sq = max(rstats1(ridge_ns4)$R2), adj_r_sq = max(rstats1(ridge_ns4)$adjR2), cv_r_sq = mean(cv_ridge_ns4$resample$Rsquared))
frame_ns1 <- add_row(frame_ns1, r_sq = max(rstats1(ridge_ns5)$R2), adj_r_sq = max(rstats1(ridge_ns5)$adjR2), cv_r_sq = mean(cv_ridge_ns5$resample$Rsquared))

#Plotting The Values
plot(frame_ns1$adj_r_sq,type="l",col="red",main = "RidgePlot",ylab="Variation",ylim = c(0,1))
lines(frame_ns1$r_sq,col="green")
lines(frame_ns1$cv_r_sq,col="Blue")
legend(2,1,legend = c("adj_r_square","r_square","cv_r_square"),col=c("red","green","blue"),lty = 1:2,cex = 0.8)


#Lasso Regression
#mpg ~ wgt + year+hp+cyl+acc+disp
x <-(auto$wgt)
x <-cbind(auto$year,x)
lasso_mod1 <- lars(x, auto$mpg, type = 'lasso')

x <-(auto$wgt)
x <-cbind(auto$year,x)
x <-cbind(auto$hp,x)
lasso_mod2 <- lars(x, auto$mpg, type = 'lasso')

x <-(auto$wgt)
x <-cbind(auto$year,x)
x <-cbind(auto$hp,x)
x <-cbind(auto$cyl,x)
lasso_mod3 <- lars(x, auto$mpg, type = 'lasso')

x <-(auto$wgt)
x <-cbind(auto$year,x)
x <-cbind(auto$hp,x)
x <-cbind(auto$cyl,x)
x <-cbind(auto$acc,x)
lasso_mod4 <- lars(x, auto$mpg, type = 'lasso')


x <-(auto$wgt)
x <-cbind(auto$year,x)
x <-cbind(auto$hp,x)
x <-cbind(auto$cyl,x)
x <-cbind(auto$acc,x)
x <-cbind(auto$disp,x)
lasso_mod5 <- lars(x, auto$mpg, type = 'lasso')

#Cross-Validation
#mpg ~ wgt + year+hp+cyl+acc+disp
cv_lasso_ns1<-train(mpg~wgt+year,auto,method="lasso",trControl=train_control)
cv_lasso_ns2<-train(mpg~wgt+year+hp,auto,method="lasso",trControl=train_control)
cv_lasso_ns3<-train(mpg~wgt+year+hp+cyl,auto,method="lasso",trControl=train_control)
cv_lasso_ns4<-train(mpg~wgt+year+hp+cyl+acc,auto,method="lasso",trControl=train_control)
cv_lasso_ns5<-train(mpg~wgt+year+hp+cyl+acc+disp,auto,method="lasso",trControl=train_control)

#DataFrame

frame_ns2 <- data.frame("r_sq" = double(0), "cv_r_sq" = double(0))
frame_ns2 <- add_row(frame_ns2, r_sq =max((lasso_mod1)$R2), cv_r_sq = mean(cv_lasso_ns1$resample$Rsquared))
frame_ns2 <- add_row(frame_ns2, r_sq =max((lasso_mod2)$R2), cv_r_sq = mean(cv_lasso_ns2$resample$Rsquared))
frame_ns2 <- add_row(frame_ns2, r_sq =max((lasso_mod3)$R2), cv_r_sq = mean(cv_lasso_ns3$resample$Rsquared))
frame_ns2 <- add_row(frame_ns2, r_sq =max((lasso_mod4)$R2), cv_r_sq = mean(cv_lasso_ns4$resample$Rsquared))
frame_ns2 <- add_row(frame_ns2, r_sq =max((lasso_mod5)$R2), cv_r_sq = mean(cv_lasso_ns5$resample$Rsquared))


#Plotting The Values:-
plot(frame_ns2$r_sq,type="l",col="red",main = "LassoPLOT",ylab="Variation",ylim = c(0,1))
lines(frame_ns2$cv_r_sq,col="Blue")
legend(2,1,legend = c("r_square","cv_r_square"),col=c("red","blue"),lty = 1:2,cex = 0.8)


#Quad Regression
q_nas<-auto

#Squaring The Variables:-
#mpg ~ wgt + year+hp+cyl+acc+disp
q_nas$wgt1<-q_nas$wgt^2
q_nas$year1<-q_nas$year^2
q_nas$hp1<-q_nas$hp^2
q_nas$cyl1<-q_nas$cyl^2
q_nas$acc1<-q_nas$acc^2
q_nas$disp1<-q_nas$disp^2

#Linear Regression
Quad1 <- lm(mpg~wgt+wgt1,data=q_nas)
Quad2 <- lm(mpg~wgt+wgt1+year+year1,data=q_nas)
Quad3 <- lm(mpg~wgt+wgt1+year+year1+hp+hp1,data=q_nas)
Quad4 <- lm(mpg~wgt+wgt1+year+year1+hp+hp1+cyl+cyl1,data=q_nas)
Quad5 <- lm(mpg~wgt+wgt1+year+year1+hp+hp1+cyl+cyl1+acc+acc1,data=q_nas)
Quad6 <- lm(mpg~wgt+wgt1+year+year1+hp+hp1+cyl+cyl1+acc+acc1+disp+disp1,data=q_nas)

#Cross-Validation
cv_quad_ns1 <- train(mpg ~ wgt+wgt1, data = q_nas, trControl = train_control, method = "lm")
cv_quad_ns2 <- train(mpg ~ wgt+wgt1+year+year1, data = q_nas, trControl = train_control, method = "lm")
cv_quad_ns3 <- train(mpg ~ wgt+wgt1+year+year1+hp+hp1, data = q_nas, trControl = train_control, method = "lm")
cv_quad_ns4 <- train(mpg ~ wgt+wgt1+year+year1+hp+hp1+cyl+cyl1, data = q_nas, trControl = train_control, method = "lm")
cv_quad_ns5 <- train(mpg ~ wgt+wgt1+year+year1+hp+hp1+cyl+cyl1+acc+acc1, data = q_nas, trControl = train_control, method = "lm")
cv_quad_ns6 <- train(mpg ~ wgt+wgt1+year+year1+hp+hp1+cyl+cyl1+acc+acc1+disp+disp1, data = q_nas, trControl = train_control, method = "lm")

#DataFrame
frame_ns3 <- data.frame("adj_r_sq" = double(0),"r_sq" = double(0), "cv_r_sq" = double(0))
frame_ns3<- add_row(frame_ns3, r_sq = summary(Quad1)$r.squared, adj_r_sq = summary(Quad1)$adj.r.squared, cv_r_sq = mean(cv_quad_ns1$resample$Rsquared))
frame_ns3<- add_row(frame_ns3, r_sq = summary(Quad2)$r.squared, adj_r_sq = summary(Quad2)$adj.r.squared, cv_r_sq = mean(cv_quad_ns2$resample$Rsquared))
frame_ns3<- add_row(frame_ns3, r_sq = summary(Quad3)$r.squared, adj_r_sq = summary(Quad3)$adj.r.squared, cv_r_sq = mean(cv_quad_ns3$resample$Rsquared))
frame_ns3<- add_row(frame_ns3, r_sq = summary(Quad4)$r.squared, adj_r_sq = summary(Quad4)$adj.r.squared, cv_r_sq = mean(cv_quad_ns4$resample$Rsquared))
frame_ns3<- add_row(frame_ns3, r_sq = summary(Quad5)$r.squared, adj_r_sq = summary(Quad5)$adj.r.squared, cv_r_sq = mean(cv_quad_ns5$resample$Rsquared))
frame_ns3<- add_row(frame_ns3, r_sq = summary(Quad6)$r.squared, adj_r_sq = summary(Quad6)$adj.r.squared, cv_r_sq = mean(cv_quad_ns6$resample$Rsquared))

#Plotting The Values
plot(frame_ns3$adj_r_sq,type="l",col="red",main = "QuadPLOT",ylab="Variation",ylim = c(0,1))
lines(frame_ns3$r_sq,col="green")
lines(frame_ns3$cv_r_sq,col="Blue")
legend(2,0.6,legend = c("adj_r_square","r_square","cv_r_square"),col=c("red","green","blue"),lty = 1:2,cex = 0.8)


#Response Surface
#mpg ~ wgt + year+hp+cyl+acc+disp
resp_ns<-auto
rs_md1<-rsm(mpg~SO(wgt,year),data=resp_ns)
rs_md2<-rsm(mpg~SO(wgt,year,hp),data=resp_ns)
rs_md3<-rsm(mpg~SO(wgt,year,hp,cyl),data=resp_ns)
rs_md4<-rsm(mpg~SO(wgt,year,hp,cyl,acc),data=resp_ns)
rs_md5<-rsm(mpg~SO(wgt,year,hp,cyl,acc,disp),data=resp_ns)

#DataFrames
frame_ns4 <-data.frame("adj_r_square" = double(0), "r_square" = double(0))
frame_ns4 <- add_row(frame_ns4, adj_r_square = summary(rs_md1)$adj.r.squared, r_square = summary(rs_md1)$r.squared)
frame_ns4 <- add_row(frame_ns4, adj_r_square = summary(rs_md2)$adj.r.squared, r_square = summary(rs_md2)$r.squared)
frame_ns4 <- add_row(frame_ns4, adj_r_square = summary(rs_md3)$adj.r.squared, r_square = summary(rs_md3)$r.squared)
frame_ns4 <- add_row(frame_ns4, adj_r_square = summary(rs_md4)$adj.r.squared, r_square = summary(rs_md4)$r.squared)
frame_ns4 <- add_row(frame_ns4, adj_r_square = summary(rs_md5)$adj.r.squared, r_square = summary(rs_md5)$r.squared)

#Plotting The Values
plot(frame_ns4$adj_r_square,type="l",col="red",main = "ReponseSurfacePLOT",ylab="Variation",ylim = c(0,1))
lines(frame_ns4$r_square,col="green")
legend(2,0.6,legend = c("adj_r_square","r_square"),col=c("red","green"),lty = 1:2,cex = 0.8)

