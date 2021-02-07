library(ggpubr)
library(ggplot2)
library(dplyr) #add rows
library(lmridge) #for ridge regression
library(lars) # lasso regression
library(caret) # for cross validation
library(gdata) #to read xls file format
library(rsm) # for response surface

Plant <-Folds5x2_pp
str(Plant)

#Forward Model
fwd_model_power <- lm(AT ~ 1,  data = Plant)
step(fwd_model_power, direction = "forward", scope = formula(AT~V+AP+RH+PE))


lm(formula = AT ~ PE + RH + V + AP, data = Plant)

#LinearRegression
lmp1 <- lm(AT~PE,data=Plant)
lmp2 <- lm(AT~PE+RH,data=Plant)
lmp3 <- lm(AT~PE+RH+V,data=Plant)
lmp4 <- lm(AT~PE+RH+V+AP,data=Plant)

#Cross-Validation 
train_control <- trainControl(method = "cv", number = 10)
cv_power1<-train(AT~PE,data=Plant,trControl=train_control,method="lm")
cv_power2<-train(AT~PE+RH,data=Plant,trControl=train_control,method="lm")
cv_power3<-train(AT~PE+RH+V,data=Plant,trControl=train_control,method="lm")
cv_power4<-train(AT~PE+RH+V+AP,data=Plant,trControl=train_control,method="lm")

#DataFrame
power_frame <- data.frame("r_sq" = double(0), "adj_r_sq" = double(0), "cv_r_sq" = double(0))
power_frame <-add_row(power_frame, r_sq = summary(lmp1)$r.squared, adj_r_sq = summary(lmp1)$adj.r.squared, cv_r_sq = mean(cv_power1$resample$Rsquared))
power_frame <-add_row(power_frame, r_sq = summary(lmp2)$r.squared, adj_r_sq = summary(lmp2)$adj.r.squared, cv_r_sq = mean(cv_power2$resample$Rsquared))
power_frame <-add_row(power_frame, r_sq = summary(lmp3)$r.squared, adj_r_sq = summary(lmp3)$adj.r.squared, cv_r_sq = mean(cv_power3$resample$Rsquared))
power_frame <-add_row(power_frame, r_sq = summary(lmp4)$r.squared, adj_r_sq = summary(lmp4)$adj.r.squared, cv_r_sq = mean(cv_power4$resample$Rsquared))

#Plotting The Values


plot(power_frame$r_sq, type = 'l', col = 'red', main = "Linear Plot", ylab = "Errors", ylim = c(0,1))
lines(power_frame$adj_r_sq,  col = 'green' )
lines(power_frame$cv_r_sq,  col = 'blue')
legend(2,0.6, legend = c("R Squared","Adj R Squared","R Squared CV"), 
       col = c("red","green","blue"), lty = 1:2, cex = 0.8)


#Ridge Regression

ridge_power1 <- lmridge(AT ~ PE + RH,Plant, K = c(0.1, 0.001))
ridge_power2 <- lmridge(AT ~ PE + RH + V,Plant, K = c(0.1, 0.001))
ridge_power3 <- lmridge(AT ~ PE + RH + V + AP,Plant, K = c(0.1, 0.001))

#Cross-Validation On Ridge

cv_powermodel1 <- train(AT ~ PE+RH, data = Plant, trControl = train_control, method = "ridge")
cv_powermodel2 <- train(AT ~ PE+RH+V, data = Plant, trControl = train_control, method = "ridge")
cv_powermodel3 <- train(AT ~ PE+RH+V+AP, data = Plant, trControl = train_control, method = "ridge")

#DataFrame
frame_plant2 <- data.frame("r_sq" = double(0), "adj_r_sq" = double(0), "cv_r_sq" = double(0))
frame_plant2 <-add_row(frame_plant2, r_sq = max(rstats1(ridge_power1)$R2), adj_r_sq = max(rstats1(ridge_power1)$adjR2), cv_r_sq = mean(cv_powermodel1$resample$Rsquared))
frame_plant2 <-add_row(frame_plant2, r_sq = max(rstats1(ridge_power2)$R2), adj_r_sq = max(rstats1(ridge_power2)$adjR2), cv_r_sq = mean(cv_powermodel2$resample$Rsquared))
frame_plant2 <-add_row(frame_plant2, r_sq = max(rstats1(ridge_power3)$R2), adj_r_sq = max(rstats1(ridge_power3)$adjR2), cv_r_sq = mean(cv_powermodel3$resample$Rsquared))

#Plotting The Variables:-
plot(frame_plant2$adj_r_sq,type="l",col="red",main = "RidgePlot",ylab="Variation",ylim = c(0,1))
lines(frame_plant2$r_sq,col="green")
lines(frame_plant2$cv_r_sq,col="Blue")
legend(2,1,legend = c("adj_r_square","r_square","cv_r_square"),col=c("red","green","blue"),lty = 1:2,cex = 0.8)

#Lasso Regression
x <- (Plant$PE)
x <- cbind(Plant$RH,x)
lasso_power1 <- lars(x, Plant$AT, type = 'lasso')

x <- (Plant$PE)
x <- cbind(Plant$RH,x)
x <- cbind(Plant$V,x)
lasso_power2 <- lars(x, Plant$AT, type = 'lasso')

x <- (Plant$PE)
x <- cbind(Plant$RH,x)
x <- cbind(Plant$V,x)
x <- cbind(Plant$AP,x)
lasso_power3 <- lars(x, Plant$AT, type = 'lasso')

#Cross-Validation on Lasso
cv_lasso_plant1<-train(AT~PE+RH,Plant,method="lasso",trControl=train_control)
cv_lasso_plant2<-train(AT~PE+RH+V,Plant,method="lasso",trControl=train_control)
cv_lasso_plant3<-train(AT~PE+RH+V+AP,Plant,method="lasso",trControl=train_control)

#DataFrame
frame_plant3 <- data.frame("r_sq" = double(0), "cv_r_sq" = double(0))
frame_plant3 <- add_row(frame_plant3, r_sq =max((lasso_power1)$R2), cv_r_sq = mean(cv_lasso_plant1$resample$Rsquared))
frame_plant3 <- add_row(frame_plant3, r_sq =max((lasso_power2)$R2), cv_r_sq = mean(cv_lasso_plant2$resample$Rsquared))
frame_plant3 <- add_row(frame_plant3, r_sq =max((lasso_power3)$R2), cv_r_sq = mean(cv_lasso_plant3$resample$Rsquared))

#Plotting The Variables:-
plot(frame_plant3$r_sq,type="l",col="red",main = "LassoPLOT",ylab="Variation",ylim = c(0,1))
lines(frame_plant3$cv_r_sq,col="Blue")
legend(2,0.6,legend = c("r_square","cv_r_square"),col=c("red","blue"),lty = 1:2,cex = 0.8)

#Quad Regression
q_plant<-Plant
q_plant$pe_sq<-q_plant$PE^2
q_plant$rh_sq<-q_plant$RH^2
q_plant$v_sq<-q_plant$V^2
q_plant$ap_sq<-q_plant$AP^2

#Linear Regression In R 
Quad_plant1 <- lm(AT~PE+pe_sq,data=q_plant)
Quad_plant2 <- lm(AT~PE+pe_sq+RH+rh_sq,data=q_plant)
Quad_plant3 <- lm(AT~PE+pe_sq+RH+rh_sq+V+v_sq,data=q_plant)
Quad_plant4 <- lm(AT~PE+pe_sq+RH+rh_sq+V+v_sq+AP+ap_sq,data=q_plant)

#Cross Validation
cv_quad_plant1<-train(AT~PE+pe_sq,q_plant,method="lasso",trControl=train_control)
cv_quad_plant2<-train(AT~PE+pe_sq+RH+rh_sq,q_plant,method="lasso",trControl=train_control)
cv_quad_plant3<-train(AT~PE+pe_sq+RH+rh_sq+V+v_sq,q_plant,method="lasso",trControl=train_control)
cv_quad_plant4<-train(AT~PE+pe_sq+RH+rh_sq+V+v_sq+AP+ap_sq,q_plant,method="lasso",trControl=train_control)

#DataFrame
frame_plant4 <- data.frame("adj_r_sq" = double(0),"r_sq" = double(0), "cv_r_sq" = double(0))
frame_plant4<- add_row(frame_plant4, r_sq = summary(Quad_plant1)$r.squared, adj_r_sq = summary(Quad_plant1)$adj.r.squared, cv_r_sq = mean(cv_quad_plant1$resample$Rsquared))
frame_plant4<- add_row(frame_plant4, r_sq = summary(Quad_plant2)$r.squared, adj_r_sq = summary(Quad_plant2)$adj.r.squared, cv_r_sq = mean(cv_quad_plant2$resample$Rsquared))
frame_plant4<- add_row(frame_plant4, r_sq = summary(Quad_plant3)$r.squared, adj_r_sq = summary(Quad_plant3)$adj.r.squared, cv_r_sq = mean(cv_quad_plant3$resample$Rsquared))
frame_plant4<- add_row(frame_plant4, r_sq = summary(Quad_plant4)$r.squared, adj_r_sq = summary(Quad_plant4)$adj.r.squared, cv_r_sq = mean(cv_quad_plant4$resample$Rsquared))

#Plotting The Values:-
plot(frame_plant4$adj_r_sq,type="l",col="red",main = "QuadPLOT",ylab="Variation",ylim = c(0,1))
lines(frame_plant4$r_sq,col="green")
lines(frame_plant4$cv_r_sq,col="Blue")
legend(2,0.6,legend = c("adj_r_square","r_square","cv_r_square"),col=c("red","green","blue"),lty = 1:2,cex = 0.8)
  

#Response Surface
resp_plant<-Plant
rs_plant1<-rsm(AT~SO(PE,RH),data=resp_plant)
rs_plant2<-rsm(AT~SO(PE,RH,V),data=resp_plant)
rs_plant3<-rsm(AT~SO(PE,RH,V,AP),data=resp_plant)

#DataFrame
frame_plant5 <-data.frame("adj_r_square" = double(0), "r_square" = double(0))
frame_plant5 <- add_row(frame_plant5, adj_r_square = summary(rs_plant1)$adj.r.squared, r_square = summary(rs_plant1)$r.squared)
frame_plant5 <- add_row(frame_plant5, adj_r_square = summary(rs_plant2)$adj.r.squared, r_square = summary(rs_plant2)$r.squared)
frame_plant5 <- add_row(frame_plant5, adj_r_square = summary(rs_plant3)$adj.r.squared, r_square = summary(rs_plant3)$r.squared)

#Plotting The Variables:-
plot(frame_plant5$adj_r_square,type="l",col="red",main = "ReponseSurfacePLOT",ylab="Variation",ylim = c(0,1))
lines(frame_plant5$r_square,col="green")
legend(2,1,legend = c("adj_r_square","r_square"),col=c("red","green"),lty = 1:2,cex = 0.8)


