library(ggpubr)
library(ggplot2)
library(dplyr) #add rows
library(lmridge) #for ridge regression
library(lars) # lasso regression
library(caret) # for cross validation
library(gdata) #to read xls file format
library(rsm) # for response surface

#LinearRegression
conc<-Concrete_Data
str(conc)
colnames(conc)

names(conc) <-c("cmt","blst","Ash","Water","Pzer","Coarse","Agg","Age","strength")

fwd_model_conc <- lm(cmt ~ 1,  data = conc)
step(fwd_model_conc, direction = "forward", scope = formula(cmt~blst+Ash+Water+Pzer+Coarse+Agg+Age+strength))


#Lm Formula
lm(formula = cmt ~ strength + Ash + blst + Agg + Coarse + Water + 
     Age + Pzer, data = conc)


#LinearRegression
lin_conc1 <- lm(cmt ~ strength, data = conc)
lin_conc2 <- lm(cmt ~ strength+Ash, data = conc)
lin_conc3 <- lm(cmt ~ strength+Ash+blst, data = conc)
lin_conc4 <- lm(cmt ~ strength+Ash+blst+Agg, data = conc)
lin_conc5 <- lm(cmt ~ strength+Ash+blst+Agg+Coarse, data = conc)
lin_conc6 <- lm(cmt ~ strength+Ash+blst+Agg+Coarse+Water, data = conc)
lin_conc7 <- lm(cmt ~ strength+Ash+blst+Agg+Coarse+Water+Age, data = conc)
lin_conc8 <- lm(cmt ~ strength+Ash+blst+Agg+Coarse+Water+Age+Pzer, data = conc)

#Cross-Validation
train_control <- trainControl(method = "cv", number = 10)
cv_concmodel1 <- train(cmt ~ strength, data = conc, trControl = train_control, method = "lm")
cv_concmodel2 <- train(cmt ~ strength+Ash, data = conc, trControl = train_control, method = "lm")
cv_concmodel3 <- train(cmt ~ strength+Ash+blst, data = conc, trControl = train_control, method = "lm")
cv_concmodel4 <- train(cmt ~ strength+Ash+blst+Agg, data = conc, trControl = train_control, method = "lm")
cv_concmodel5 <- train(cmt ~ strength+Ash+blst+Agg+Coarse, data = conc, trControl = train_control, method = "lm")
cv_concmodel6 <- train(cmt ~ strength+Ash+blst+Agg+Coarse+Water, data = conc, trControl = train_control, method = "lm")
cv_concmodel7 <- train(cmt ~ strength+Ash+blst+Agg+Coarse+Water+Age, data = conc, trControl = train_control, method = "lm")
cv_concmodel8 <- train(cmt ~ strength+Ash+blst+Agg+Coarse+Water+Age+Pzer, data = conc, trControl = train_control, method = "lm")

#DataFrame
frame_conc<-data.frame("r_sq" = double(0), "adj_r_sq" = double(0), "cv_r_sq" = double(0))
frame_conc<- add_row(frame_conc, r_sq = summary(lin_conc1)$r.squared, adj_r_sq = summary(lin_conc1)$adj.r.squared, cv_r_sq = mean(cv_concmodel1$resample$Rsquared))
frame_conc<- add_row(frame_conc, r_sq = summary(lin_conc2)$r.squared, adj_r_sq = summary(lin_conc2)$adj.r.squared, cv_r_sq = mean(cv_concmodel2$resample$Rsquared))
frame_conc<- add_row(frame_conc, r_sq = summary(lin_conc3)$r.squared, adj_r_sq = summary(lin_conc3)$adj.r.squared, cv_r_sq = mean(cv_concmodel3$resample$Rsquared))
frame_conc<- add_row(frame_conc, r_sq = summary(lin_conc4)$r.squared, adj_r_sq = summary(lin_conc4)$adj.r.squared, cv_r_sq = mean(cv_concmodel4$resample$Rsquared))
frame_conc<- add_row(frame_conc, r_sq = summary(lin_conc5)$r.squared, adj_r_sq = summary(lin_conc5)$adj.r.squared, cv_r_sq = mean(cv_concmodel5$resample$Rsquared))
frame_conc<- add_row(frame_conc, r_sq = summary(lin_conc6)$r.squared, adj_r_sq = summary(lin_conc6)$adj.r.squared, cv_r_sq = mean(cv_concmodel6$resample$Rsquared))
frame_conc<- add_row(frame_conc, r_sq = summary(lin_conc7)$r.squared, adj_r_sq = summary(lin_conc7)$adj.r.squared, cv_r_sq = mean(cv_concmodel7$resample$Rsquared))
frame_conc<- add_row(frame_conc, r_sq = summary(lin_conc8)$r.squared, adj_r_sq = summary(lin_conc8)$adj.r.squared, cv_r_sq = mean(cv_concmodel8$resample$Rsquared))

#Plotting The Values


plot(frame_conc$r_sq, type = 'l', col = 'red', main = "Linear Plot", ylab = "Errors", ylim = c(0.1,1))
lines(frame_conc$adj_r_sq,  col = 'green' )
lines(frame_conc$cv_r_sq,  col = 'blue')
legend(5,0.8, legend = c("R Squared","Adj R Squared","R Squared CV"), 
       col = c("red","green","blue"), lty = 1:2, cex = 0.8)

#Ridge Regression

ridge_conc1 <- lmridge(cmt ~ strength + Ash, conc, K = c(0.1, 0.001))
ridge_conc2 <- lmridge(cmt ~ strength + Ash + blst, conc, K = c(0.1, 0.001))
ridge_conc3 <- lmridge(cmt ~ strength + Ash + blst + Agg, conc, K = c(0.1, 0.001))
ridge_conc4 <- lmridge(cmt ~ strength + Ash + blst + Agg + Coarse , conc, K = c(0.1, 0.001))
ridge_conc5 <- lmridge(cmt ~ strength + Ash + blst + Agg + Coarse+Water, conc, K = c(0.1, 0.001))
ridge_conc6 <- lmridge(cmt ~ strength + Ash + blst + Agg + Coarse+Water+Age, conc, K = c(0.1, 0.001))
ridge_conc7 <- lmridge(cmt ~ strength + Ash + blst + Agg + Coarse+Water+Age+Pzer, conc, K = c(0.1, 0.001))

#Cross-Validation

cv_concmodel12 <- train(cmt ~ strength+Ash, data = conc, trControl = train_control, method = "ridge")
cv_concmodel13 <- train(cmt ~ strength+Ash+blst, data = conc, trControl = train_control, method = "ridge")
cv_concmodel14 <- train(cmt ~ strength+Ash+blst+Agg, data = conc, trControl = train_control, method = "ridge")
cv_concmodel15 <- train(cmt ~ strength+Ash+blst+Agg+Coarse, data = conc, trControl = train_control, method = "ridge")
cv_concmodel16 <- train(cmt ~ strength+Ash+blst+Agg+Coarse+Water, data = conc, trControl = train_control, method = "ridge")
cv_concmodel17 <- train(cmt ~ strength+Ash+blst+Agg+Coarse+Water+Age, data = conc, trControl = train_control, method = "ridge")
cv_concmodel18 <- train(cmt ~ strength+Ash+blst+Agg+Coarse+Water+Age+Pzer, data = conc, trControl = train_control, method = "ridge")

#DataFrame
frame_conc1 <- data.frame("r_sq" = double(0), "adj_r_sq" = double(0), "cv_r_sq" = double(0))
frame_conc1 <-add_row(frame_conc1, r_sq = max(rstats1(ridge_conc1)$R2), adj_r_sq = max(rstats1(ridge_conc1)$adjR2), cv_r_sq = mean(cv_concmodel12$resample$Rsquared))
frame_conc1 <-add_row(frame_conc1, r_sq = max(rstats1(ridge_conc2)$R2), adj_r_sq = max(rstats1(ridge_conc2)$adjR2), cv_r_sq = mean(cv_concmodel13$resample$Rsquared))
frame_conc1 <-add_row(frame_conc1, r_sq = max(rstats1(ridge_conc3)$R2), adj_r_sq = max(rstats1(ridge_conc3)$adjR2), cv_r_sq = mean(cv_concmodel14$resample$Rsquared))
frame_conc1 <-add_row(frame_conc1, r_sq = max(rstats1(ridge_conc4)$R2), adj_r_sq = max(rstats1(ridge_conc4)$adjR2), cv_r_sq = mean(cv_concmodel15$resample$Rsquared))
frame_conc1 <-add_row(frame_conc1, r_sq = max(rstats1(ridge_conc5)$R2), adj_r_sq = max(rstats1(ridge_conc5)$adjR2), cv_r_sq = mean(cv_concmodel16$resample$Rsquared))
frame_conc1 <-add_row(frame_conc1, r_sq = max(rstats1(ridge_conc6)$R2), adj_r_sq = max(rstats1(ridge_conc6)$adjR2), cv_r_sq = mean(cv_concmodel17$resample$Rsquared))
frame_conc1 <-add_row(frame_conc1, r_sq = max(rstats1(ridge_conc7)$R2), adj_r_sq = max(rstats1(ridge_conc7)$adjR2), cv_r_sq = mean(cv_concmodel18$resample$Rsquared))

#Plotting The Values
plot(frame_conc1$r_sq, type = 'l', col = 'red', main = "Ridge Plot", ylab = "Errors", ylim = c(0.1,1))
lines(frame_conc1$adj_r_sq,  col = 'green' )
lines(frame_conc1$cv_r_sq,  col = 'blue')
legend(5,0.8, legend = c("R Squared","Adj R Squared","R Squared CV"), 
       col = c("red","green","blue"), lty = 1:2, cex = 0.8)


#Lasso Regression cmt ~ strength+Ash+blst+Agg+Coarse+Water+Age+Pzer
x <-(conc$strength)
x <-cbind(conc$Ash,x)
lasso_mod1 <- lars(x, conc$cmt, type = 'lasso')

x <-(conc$strength)
x <-cbind(conc$Ash,x)
x <-cbind(conc$blst,x)
lasso_mod2 <- lars(x, conc$cmt, type = 'lasso')

x <-(conc$strength)
x <-cbind(conc$Ash,x)
x <-cbind(conc$blst,x)
x <-cbind(conc$Agg,x)
lasso_mod3 <- lars(x, conc$cmt, type = 'lasso')

x <-(conc$strength)
x <-cbind(conc$Ash,x)
x <-cbind(conc$blst,x)
x <-cbind(conc$Agg,x)
x <-cbind(conc$Coarse,x)
lasso_mod4 <- lars(x, conc$cmt, type = 'lasso')

x <-(conc$strength)
x <-cbind(conc$Ash,x)
x <-cbind(conc$blst,x)
x <-cbind(conc$Agg,x)
x <-cbind(conc$Coarse,x)
x <-cbind(conc$Water,x)
lasso_mod5 <- lars(x, conc$cmt, type = 'lasso')

x <-(conc$strength)
x <-cbind(conc$Ash,x)
x <-cbind(conc$blst,x)
x <-cbind(conc$Agg,x)
x <-cbind(conc$Coarse,x)
x <-cbind(conc$Water,x)
x <-cbind(conc$Age,x)
lasso_mod6 <- lars(x, conc$cmt, type = 'lasso')

x <-(conc$strength)
x <-cbind(conc$Ash,x)
x <-cbind(conc$blst,x)
x <-cbind(conc$Agg,x)
x <-cbind(conc$Coarse,x)
x <-cbind(conc$Water,x)
x <-cbind(conc$Age,x)
x <-cbind(conc$Pzer,x)
lasso_mod7 <- lars(x, conc$cmt, type = 'lasso')


#Cross-Validation
cv_lasso_conc1<-train(cmt~strength+Ash,conc,method="lasso",trControl=train_control)
cv_lasso_conc2<-train(cmt~strength+Ash+blst,conc,method="lasso",trControl=train_control)
cv_lasso_conc3<-train(cmt~strength+Ash+blst+Agg,conc,method="lasso",trControl=train_control)
cv_lasso_conc4<-train(strength~Ash+blst+Agg+Coarse,conc,method="lasso",trControl=train_control)
cv_lasso_conc5<-train(strength~Ash+blst+Agg+Coarse+Water,conc,method="lasso",trControl=train_control)
cv_lasso_conc6<-train(strength~Ash+blst+Agg+Coarse+Water+Age,conc,method="lasso",trControl=train_control)
cv_lasso_conc7<-train(strength~Ash+blst+Agg+Coarse+Water+Age+Pzer,conc,method="lasso",trControl=train_control)

#DataFrame
frame_conc2 <- data.frame("r_sq" = double(0), "cv_r_sq" = double(0))
frame_conc2 <- add_row(frame_conc2, r_sq =max((lasso_mod1)$R2), cv_r_sq = mean(cv_lasso_conc1$resample$Rsquared))
frame_conc2 <- add_row(frame_conc2, r_sq =max((lasso_mod2)$R2), cv_r_sq = mean(cv_lasso_conc2$resample$Rsquared))
frame_conc2 <- add_row(frame_conc2, r_sq =max((lasso_mod3)$R2), cv_r_sq = mean(cv_lasso_conc3$resample$Rsquared))
frame_conc2 <- add_row(frame_conc2, r_sq =max((lasso_mod4)$R2), cv_r_sq = mean(cv_lasso_conc4$resample$Rsquared))
frame_conc2 <- add_row(frame_conc2, r_sq =max((lasso_mod5)$R2), cv_r_sq = mean(cv_lasso_conc5$resample$Rsquared))
frame_conc2 <- add_row(frame_conc2, r_sq =max((lasso_mod6)$R2), cv_r_sq = mean(cv_lasso_conc6$resample$Rsquared))
frame_conc2 <- add_row(frame_conc2, r_sq =max((lasso_mod7)$R2), cv_r_sq = mean(cv_lasso_conc7$resample$Rsquared))

#Plotting Values
plot(frame_conc2$r_sq,type="l",col="red",main = "LassoRegression",ylab="Variation",ylim = c(0,1))
lines(frame_conc2$cv_r_sq,col="Blue")
legend(2,1,legend = c("r_square","cv_r_square"),col=c("red","blue"),lty = 1:2,cex = 0.8)

#QuadRegression
q_conc<-conc
strength~Ash+blst+Agg+Coarse+Water+Age+Pzer

#SquaringTheVariables
q_conc$strength_sq<-q_conc$strength^2
q_conc$Ash_sq<-q_conc$Ash^2
q_conc$blst_sq<-q_conc$blst^2
q_conc$Agg_sq<-q_conc$Agg^2
q_conc$Coarse_sq<-q_conc$Coarse^2
q_conc$Water_sq<-q_conc$Water^2
q_conc$Age_Sq<-q_conc$Age^2
q_conc$Pzer_Sq<-q_conc$Pzer^2

#Linear Regression
Quad_conc1 <- lm(cmt~strength+strength_sq,data=q_conc)
Quad_conc2 <- lm(cmt~strength+strength_sq+Ash+Ash_sq,data=q_conc)
Quad_conc3 <- lm(cmt~strength+strength_sq+Ash+Ash_sq+blst+blst_sq,data=q_conc)
Quad_conc4 <- lm(cmt~strength+strength_sq+Ash+Ash_sq+blst+blst_sq+Agg+Agg_sq,data=q_conc)
Quad_conc5 <- lm(cmt~strength+strength_sq+Ash+Ash_sq+blst+blst_sq+Agg+Agg_sq+Coarse+Coarse_sq,data=q_conc)
Quad_conc6 <- lm(cmt~strength+strength_sq+Ash+Ash_sq+blst+blst_sq+Agg+Agg_sq+Coarse+Coarse_sq+Water+Water_sq,data=q_conc)
Quad_conc7 <- lm(cmt~strength+strength_sq+Ash+Ash_sq+blst+blst_sq+Agg+Agg_sq+Coarse+Coarse_sq+Water+Water_sq+Age+Age_Sq,data=q_conc)
Quad_conc8 <- lm(cmt~strength+strength_sq+Ash+Ash_sq+blst+blst_sq+Agg+Agg_sq+Coarse+Coarse_sq+Water+Water_sq+Age+Age_Sq+Pzer+Pzer_Sq,data=q_conc)


#Cross-Validation
Quad_cv1 <- train(cmt~ strength+strength_sq, data = q_conc, trControl = train_control, method = "lm")
Quad_cv2 <- train(cmt~ strength+strength_sq+Ash+Ash_sq, data = q_conc, trControl = train_control, method = "lm")
Quad_cv3 <- train(cmt~ strength+strength_sq+Ash+Ash_sq+blst+blst_sq, data = q_conc, trControl = train_control, method = "lm")
Quad_cv4 <- train(cmt~ strength+strength_sq+Ash+Ash_sq+blst+blst_sq+Agg+Agg_sq, data = q_conc, trControl = train_control, method = "lm")
Quad_cv5 <- train(cmt~ strength+strength_sq+Ash+Ash_sq+blst+blst_sq+Agg+Agg_sq+Coarse+Coarse_sq, data = q_conc, trControl = train_control, method = "lm")
Quad_cv6 <- train(cmt~ strength+strength_sq+Ash+Ash_sq+blst+blst_sq+Agg+Agg_sq+Coarse+Coarse_sq+Water+Water_sq, data = q_conc, trControl = train_control, method = "lm")
Quad_cv7 <- train(cmt~ strength+strength_sq+Ash+Ash_sq+blst+blst_sq+Agg+Agg_sq+Coarse+Coarse_sq+Water+Water_sq+Age+Age_Sq, data = q_conc, trControl = train_control, method = "lm")
Quad_cv8 <- train(cmt~ strength+strength_sq+Ash+Ash_sq+blst+blst_sq+Agg+Agg_sq+Coarse+Coarse_sq+Water+Water_sq+Age+Age_Sq+Pzer+Pzer_Sq, data = q_conc, trControl = train_control, method = "lm")



#Data-Frame
frame_conc3 <- data.frame("adj_r_sq" = double(0),"r_sq" = double(0), "cv_r_sq" = double(0))
frame_conc3 <- add_row(frame_conc3, r_sq = summary(Quad_conc1)$r.squared, adj_r_sq = summary(Quad_conc1)$adj.r.squared, cv_r_sq = mean(Quad_cv1$resample$Rsquared))
frame_conc3 <- add_row(frame_conc3, r_sq = summary(Quad_conc2)$r.squared, adj_r_sq = summary(Quad_conc2)$adj.r.squared, cv_r_sq = mean(Quad_cv2$resample$Rsquared))
frame_conc3 <- add_row(frame_conc3, r_sq = summary(Quad_conc3)$r.squared, adj_r_sq = summary(Quad_conc3)$adj.r.squared, cv_r_sq = mean(Quad_cv3$resample$Rsquared))
frame_conc3 <- add_row(frame_conc3, r_sq = summary(Quad_conc4)$r.squared, adj_r_sq = summary(Quad_conc4)$adj.r.squared, cv_r_sq = mean(Quad_cv4$resample$Rsquared))
frame_conc3 <- add_row(frame_conc3, r_sq = summary(Quad_conc5)$r.squared, adj_r_sq = summary(Quad_conc5)$adj.r.squared, cv_r_sq = mean(Quad_cv5$resample$Rsquared))
frame_conc3 <- add_row(frame_conc3, r_sq = summary(Quad_conc6)$r.squared, adj_r_sq = summary(Quad_conc6)$adj.r.squared, cv_r_sq = mean(Quad_cv6$resample$Rsquared))
frame_conc3 <- add_row(frame_conc3, r_sq = summary(Quad_conc7)$r.squared, adj_r_sq = summary(Quad_conc7)$adj.r.squared, cv_r_sq = mean(Quad_cv7$resample$Rsquared))
frame_conc3 <- add_row(frame_conc3, r_sq = summary(Quad_conc8)$r.squared, adj_r_sq = summary(Quad_conc8)$adj.r.squared, cv_r_sq = mean(Quad_cv8$resample$Rsquared))

#Plotting The Values
plot(frame_conc3$adj_r_sq,type="l",col="red",main = "QuadPLOT",ylab="Variation",ylim = c(0,1))
lines(frame_conc3$r_sq,col="green")
lines(frame_conc3$cv_r_sq,col="Blue")
legend(2,1,legend = c("adj_r_square","r_square","cv_r_square"),col=c("red","green","blue"),lty = 1:2,cex = 0.8)

#ResponseSurface
strength~Ash+blst+Agg+Coarse+Water+Age+Pzer
resp_conc<-conc
rs_conc1<-rsm(cmt~SO(strength,Ash),data=resp_conc)
rs_conc2<-rsm(cmt~SO(strength,Ash,blst),data=resp_conc)
rs_conc3<-rsm(cmt~SO(strength,Ash,blst,Agg),data=resp_conc)
rs_conc4<-rsm(cmt~SO(strength,Ash,blst,Agg,Coarse),data=resp_conc)
rs_conc5<-rsm(cmt~SO(strength,Ash,blst,Agg,Coarse,Water),data=resp_conc)
rs_conc6<-rsm(cmt~SO(strength,Ash,blst,Agg,Coarse,Water+Age),data=resp_conc)
rs_conc7<-rsm(cmt~SO(strength,Ash,blst,Agg,Coarse,Water+Age+Pzer),data=resp_conc)


#DataFrame
frame_conc4<-data.frame("adj_r_square" = double(0), "r_square" = double(0))
frame_conc4 <- add_row(frame_conc4, adj_r_square = summary(rs_conc1)$adj.r.squared, r_square = summary(rs_conc1)$r.squared)
frame_conc4 <- add_row(frame_conc4, adj_r_square = summary(rs_conc2)$adj.r.squared, r_square = summary(rs_conc2)$r.squared)
frame_conc4 <- add_row(frame_conc4, adj_r_square = summary(rs_conc3)$adj.r.squared, r_square = summary(rs_conc3)$r.squared)
frame_conc4 <- add_row(frame_conc4, adj_r_square = summary(rs_conc4)$adj.r.squared, r_square = summary(rs_conc4)$r.squared)
frame_conc4 <- add_row(frame_conc4, adj_r_square = summary(rs_conc5)$adj.r.squared, r_square = summary(rs_conc5)$r.squared)
frame_conc4 <- add_row(frame_conc4, adj_r_square = summary(rs_conc6)$adj.r.squared, r_square = summary(rs_conc6)$r.squared)
frame_conc4 <- add_row(frame_conc4, adj_r_square = summary(rs_conc7)$adj.r.squared, r_square = summary(rs_conc7)$r.squared)

#PlottingTheVariables
plot(frame_conc4$adj_r_square,type="l",col="red",main = "ReponseSurfacePLOT",ylab="Variation",ylim = c(0,1))
lines(frame_conc4$r_square,col="green")
legend(2,1,legend = c("adj_r_square","r_square"),col=c("red","green"),lty = 1:2,cex = 0.8)




