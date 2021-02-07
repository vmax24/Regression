library(ggpubr)
library(ggplot2)
library(dplyr) #add rows
library(lmridge) #for ridge regression
library(lars) # lasso regression
library(caret) # for cross validation
library(gdata) #to read xls file format
library(rsm) # for response surface

#Importing Dataset into energy
energy <-ENB2012_data
str(energy)

#Forward Selection
fwd_model_energy <- lm(X1 ~ 1,  data = energy) 
step(fwd_model_energy, direction = "forward", scope = formula(X1 ~ X2 + X3 + X4 + X5 + X6+X7+X8+Y1+Y2))


#Lm Formula
lm(formula = X1 ~ X2 + X5 + X4 + Y2 + X7 + Y1, data = energy)

#MeanImputations
energy$X1[is.na(energy$X1)] <- mean(energy$X1, na.rm = TRUE)
energy$X2[is.na(energy$X2)] <- mean(energy$X2, na.rm = TRUE)
energy$X3[is.na(energy$X3)] <- mean(energy$X3, na.rm = TRUE)
energy$X4[is.na(energy$X4)] <- mean(energy$X4, na.rm = TRUE)
energy$X5[is.na(energy$X5)] <- mean(energy$X5, na.rm = TRUE)
energy$X6[is.na(energy$X6)] <- mean(energy$X6, na.rm = TRUE)
energy$X7[is.na(energy$X7)] <- mean(energy$X7, na.rm = TRUE)
energy$X8[is.na(energy$X8)] <- mean(energy$X8, na.rm = TRUE)
energy$Y1[is.na(energy$Y1)] <- mean(energy$Y1, na.rm = TRUE)
energy$Y2[is.na(energy$Y2)] <- mean(energy$Y2, na.rm = TRUE)




#Variables Remaining X3,X6,X8

#Linear Regression
lin_energy1 <- lm(X1 ~ X2, data = energy)
lin_energy2 <- lm(X1 ~ X2+X5, data = energy)
lin_energy3 <- lm(X1 ~ X2+X5+X4, data = energy)
lin_energy4 <- lm(X1 ~ X2+X5+X4+Y2, data = energy)
lin_energy5 <- lm(X1 ~ X2+X5+X4+Y2+X7, data = energy)
lin_energy6 <- lm(X1 ~ X2+X5+X4+Y2+X7+Y1, data = energy)
lin_energy7 <- lm(X1 ~ X2+X5+X4+Y2+X7+Y1+X3, data = energy)
lin_energy8 <- lm(X1 ~ X2+X5+X4+Y2+X7+Y1+X3+X6, data = energy)
lin_energy9 <- lm(X1 ~ X2+X5+X4+Y2+X7+Y1+X3+X6+X8, data = energy)

#Cross-Validation
train_control <- trainControl(method = "cv", number = 10)
cv_energymodel1 <- train(X1 ~ X2, data = energy, trControl = train_control, method = "lm")
cv_energymodel2 <- train(X1 ~ X2+X5, data = energy, trControl = train_control, method = "lm")
cv_energymodel3 <- train(X1 ~ X2+X5+X4, data = energy, trControl = train_control, method = "lm")
cv_energymodel4 <- train(X1 ~ X2+X5+X4+Y2, data = energy, trControl = train_control, method = "lm")
cv_energymodel5 <- train(X1 ~ X2+X5+X4+Y2+X7, data = energy, trControl = train_control, method = "lm")
cv_energymodel6 <- train(X1 ~ X2+X5+X4+Y2+X7+Y1, data = energy, trControl = train_control, method = "lm")
cv_energymodel7 <- train(X1 ~ X2+X5+X4+Y2+X7+Y1+X3, data = energy, trControl = train_control, method = "lm")
cv_energymodel8 <- train(X1 ~ X2+X5+X4+Y2+X7+Y1+X3+X6, data = energy, trControl = train_control, method = "lm")
cv_energymodel9 <- train(X1 ~ X2+X5+X4+Y2+X7+Y1+X3+X6+X8, data = energy, trControl = train_control, method = "lm")


#Dataframes
frame_energy<-data.frame("r_sq" = double(0), "adj_r_sq" = double(0), "cv_r_sq" = double(0))
frame_energy<- add_row(frame_energy, r_sq = summary(lin_energy1)$r.squared, adj_r_sq = summary(lin_energy1)$adj.r.squared, cv_r_sq = mean(cv_energymodel1$resample$Rsquared))
frame_energy<- add_row(frame_energy, r_sq = summary(lin_energy2)$r.squared, adj_r_sq = summary(lin_energy2)$adj.r.squared, cv_r_sq = mean(cv_energymodel2$resample$Rsquared))
frame_energy<- add_row(frame_energy, r_sq = summary(lin_energy3)$r.squared, adj_r_sq = summary(lin_energy3)$adj.r.squared, cv_r_sq = mean(cv_energymodel3$resample$Rsquared))
frame_energy<- add_row(frame_energy, r_sq = summary(lin_energy4)$r.squared, adj_r_sq = summary(lin_energy4)$adj.r.squared, cv_r_sq = mean(cv_energymodel4$resample$Rsquared))
frame_energy<- add_row(frame_energy, r_sq = summary(lin_energy5)$r.squared, adj_r_sq = summary(lin_energy5)$adj.r.squared, cv_r_sq = mean(cv_energymodel5$resample$Rsquared))
frame_energy<- add_row(frame_energy, r_sq = summary(lin_energy6)$r.squared, adj_r_sq = summary(lin_energy6)$adj.r.squared, cv_r_sq = mean(cv_energymodel6$resample$Rsquared))
frame_energy<- add_row(frame_energy, r_sq = summary(lin_energy7)$r.squared, adj_r_sq = summary(lin_energy7)$adj.r.squared, cv_r_sq = mean(cv_energymodel7$resample$Rsquared))
frame_energy<- add_row(frame_energy, r_sq = summary(lin_energy8)$r.squared, adj_r_sq = summary(lin_energy8)$adj.r.squared, cv_r_sq = mean(cv_energymodel8$resample$Rsquared))
frame_energy<- add_row(frame_energy, r_sq = summary(lin_energy9)$r.squared, adj_r_sq = summary(lin_energy9)$adj.r.squared, cv_r_sq = mean(cv_energymodel9$resample$Rsquared))

#Plotting The Values:-

plot(frame_energy$r_sq, type = 'l', col = 'red', main = "Linear Plot", ylab = "Errors", ylim = c(0.1,1))
lines(frame_energy$adj_r_sq,  col = 'green' )
lines(frame_energy$cv_r_sq,  col = 'blue')
legend(5,0.8, legend = c("R Squared","Adj R Squared","R Squared CV"), 
       col = c("red","green","blue"), lty = 1:2, cex = 0.8)

#Ridge Regression
ridge_ns1 <- lmridge(X1 ~ X2 + X5, energy, K = c(0.1, 0.001))
ridge_ns2 <- lmridge(X1 ~ X2 + X5 + X4, energy, K = c(0.1, 0.001))
ridge_ns3 <- lmridge(X1 ~ X2 + X5 + X4 +Y2, energy, K = c(0.1, 0.001))
ridge_ns4 <- lmridge(X1 ~ X2 + X5 + X4+Y2+X7, energy, K = c(0.1, 0.001))
ridge_ns5 <- lmridge(X1 ~ X2 + X5 + X4+Y2+X7+Y1, energy, K = c(0.1, 0.001))
ridge_ns6 <- lmridge(X1 ~ X2 + X5 + X4+Y2+X7+Y1+X3, energy, K = c(0.1, 0.001))
ridge_ns7 <- lmridge(X1 ~ X2 + X5 + X4+Y2+X7+Y1+X3+X6, energy, K = c(0.1, 0.001))
ridge_ns8 <- lmridge(X1 ~ X2 + X5 + X4+Y2+X7+Y1+X3+X6+X8, energy, K = c(0.1, 0.001))

#Cross-Validation
cv_ridge_ns1 <- train(X1 ~ X2 + X5, data = energy, trControl = train_control, method = "ridge")
cv_ridge_ns2 <- train(X1 ~ X2 + X5+X4, data = energy, trControl = train_control, method = "ridge")
cv_ridge_ns3 <- train(X1 ~ X2 + X5+X4+Y2, data = energy, trControl = train_control, method = "ridge")
cv_ridge_ns4 <- train(X1 ~ X2 + X5+X4+Y2+X7, data = energy, trControl = train_control, method = "ridge")
cv_ridge_ns5 <- train(X1 ~ X2 + X5+X4+Y2+X7+Y1, data = energy, trControl = train_control, method = "ridge")
cv_ridge_ns6 <- train(X1 ~ X2 + X5+X4+Y2+X7+Y1+X3, data = energy, trControl = train_control, method = "ridge")
cv_ridge_ns7 <- train(X1 ~ X2 + X5+X4+Y2+X7+Y1+X3+X6, data = energy, trControl = train_control, method = "ridge")
cv_ridge_ns8 <- train(X1 ~ X2 + X5+X4+Y2+X7+Y1+X3+X6+X8, data = energy, trControl = train_control, method = "ridge")



#DataFrames
frame_ns1 <-data.frame("r_sq" = double(0), "adj_r_sq" = double(0), "cv_r_sq" = double(0))
frame_ns1 <- add_row(frame_ns1, r_sq = max(rstats1(ridge_ns1)$R2), adj_r_sq = max(rstats1(ridge_ns1)$adjR2), cv_r_sq = mean(cv_ridge_ns1$resample$Rsquared))
frame_ns1 <- add_row(frame_ns1, r_sq = max(rstats1(ridge_ns2)$R2), adj_r_sq = max(rstats1(ridge_ns2)$adjR2), cv_r_sq = mean(cv_ridge_ns2$resample$Rsquared))
frame_ns1 <- add_row(frame_ns1, r_sq = max(rstats1(ridge_ns3)$R2), adj_r_sq = max(rstats1(ridge_ns3)$adjR2), cv_r_sq = mean(cv_ridge_ns3$resample$Rsquared))
frame_ns1 <- add_row(frame_ns1, r_sq = max(rstats1(ridge_ns4)$R2), adj_r_sq = max(rstats1(ridge_ns4)$adjR2), cv_r_sq = mean(cv_ridge_ns4$resample$Rsquared))
frame_ns1 <- add_row(frame_ns1, r_sq = max(rstats1(ridge_ns5)$R2), adj_r_sq = max(rstats1(ridge_ns5)$adjR2), cv_r_sq = mean(cv_ridge_ns5$resample$Rsquared))

#Plotting the values

plot(frame_ns1$adj_r_sq,type="l",col="red",main = "RidgePlot",ylab="Variation",ylim = c(0,1))
lines(frame_ns1$r_sq,col="green")
lines(frame_ns1$cv_r_sq,col="Blue")
legend(2,1,legend = c("adj_r_square","r_square","cv_r_square"),col=c("red","green","blue"),lty = 1:2,cex = 0.8)


#Lasso Regression
#X1 ~ X2 + X5+X4+Y2+X7+Y1+X3+X6+X8
x <-(energy$X2)
x <-cbind(energy$X5,x)
lasso_mod1 <- lars(x, energy$X1, type = 'lasso')

x <-(energy$X2)
x <-cbind(energy$X5,x)
x <-cbind(energy$X4,x)
lasso_mod2 <- lars(x, energy$X1, type = 'lasso')

x <-(energy$X2)
x <-cbind(energy$X5,x)
x <-cbind(energy$X4,x)
x <-cbind(energy$Y2,x)
lasso_mod3 <- lars(x, energy$X1, type = 'lasso')

x <-(energy$X2)
x <-cbind(energy$X5,x)
x <-cbind(energy$X4,x)
x <-cbind(energy$Y2,x)
x <-cbind(energy$X7,x)

lasso_mod4 <- lars(x, energy$X1, type = 'lasso')

x <-(energy$X2)
x <-cbind(energy$X5,x)
x <-cbind(energy$X4,x)
x <-cbind(energy$Y2,x)
x <-cbind(energy$X7,x)
x <-cbind(energy$Y1,x)
lasso_mod5 <- lars(x, energy$X1, type = 'lasso')

x <-(energy$X2)
x <-cbind(energy$X5,x)
x <-cbind(energy$X4,x)
x <-cbind(energy$Y2,x)
x <-cbind(energy$X7,x)
x <-cbind(energy$Y1,x)
x <-cbind(energy$X3,x)
lasso_mod6 <- lars(x, energy$X1, type = 'lasso')

x <-(energy$X2)
x <-cbind(energy$X5,x)
x <-cbind(energy$X4,x)
x <-cbind(energy$Y2,x)
x <-cbind(energy$X7,x)
x <-cbind(energy$Y1,x)
x <-cbind(energy$X3,x)
x <-cbind(energy$X8,x)
lasso_mod7 <- lars(x, energy$X1, type = 'lasso')

x <-(energy$X2)
x <-cbind(energy$X5,x)
x <-cbind(energy$X4,x)
x <-cbind(energy$Y2,x)
x <-cbind(energy$X7,x)
x <-cbind(energy$Y1,x)
x <-cbind(energy$X3,x)
x <-cbind(energy$X8,x)
x <-cbind(energy$X6,x)
lasso_mod8 <- lars(x, energy$X1, type = 'lasso')

#Cross-Validation
##X1 ~ X2 + X5+X4+Y2+X7+Y1+X3+X6+X8
cv_lasso_ns1<-train(X1~X2+X5,energy,method="lasso",trControl=train_control)
cv_lasso_ns2<-train(X1~X2+X5+X4,energy,method="lasso",trControl=train_control)
cv_lasso_ns3<-train(X1~X5+X4+Y2,energy,method="lasso",trControl=train_control)
cv_lasso_ns4<-train(X1~X5+X4+Y2+X7,energy,method="lasso",trControl=train_control)
cv_lasso_ns5<-train(X1~X5+X4+Y2+X7+Y1,energy,method="lasso",trControl=train_control)
cv_lasso_ns6<-train(X1~X5+X4+Y2+X7+Y1+X3,energy,method="lasso",trControl=train_control)
cv_lasso_ns7<-train(X1~X5+X4+Y2+X7+Y1+X3+X6,energy,method="lasso",trControl=train_control)
cv_lasso_ns8<-train(X1~X5+X4+Y2+X7+Y1+X3+X6+X8,energy,method="lasso",trControl=train_control)

#DataFrames:-
frame_ns2 <- data.frame("r_sq" = double(0), "cv_r_sq" = double(0))
frame_ns2 <- add_row(frame_ns2, r_sq =max((lasso_mod1)$R2), cv_r_sq = mean(cv_lasso_ns1$resample$Rsquared))
frame_ns2 <- add_row(frame_ns2, r_sq =max((lasso_mod2)$R2), cv_r_sq = mean(cv_lasso_ns2$resample$Rsquared))
frame_ns2 <- add_row(frame_ns2, r_sq =max((lasso_mod3)$R2), cv_r_sq = mean(cv_lasso_ns3$resample$Rsquared))
frame_ns2 <- add_row(frame_ns2, r_sq =max((lasso_mod4)$R2), cv_r_sq = mean(cv_lasso_ns4$resample$Rsquared))
frame_ns2 <- add_row(frame_ns2, r_sq =max((lasso_mod5)$R2), cv_r_sq = mean(cv_lasso_ns5$resample$Rsquared))
frame_ns2 <- add_row(frame_ns2, r_sq =max((lasso_mod6)$R2), cv_r_sq = mean(cv_lasso_ns6$resample$Rsquared))
frame_ns2 <- add_row(frame_ns2, r_sq =max((lasso_mod7)$R2), cv_r_sq = mean(cv_lasso_ns7$resample$Rsquared))
frame_ns2 <- add_row(frame_ns2, r_sq =max((lasso_mod8)$R2), cv_r_sq = mean(cv_lasso_ns8$resample$Rsquared))

#Plotting The Values
plot(frame_ns2$r_sq,type="l",col="red",main = "LassoPLOT",ylab="Variation",ylim = c(0,1))
lines(frame_ns2$cv_r_sq,col="Blue")
legend(2,1,legend = c("r_square","cv_r_square"),col=c("red","blue"),lty = 1:2,cex = 0.8)

#QuadPlot
q_energy<-energy
#Squaring The Values
#X1~X5+X4+Y2+X7+Y1+X3+X6+X8
q_energy$X1_sq<-q_energy$X1^2
q_energy$X2_sq<-q_energy$X2^2
q_energy$X3_sq<-q_energy$X3^2
q_energy$X4_sq<-q_energy$X4^2
q_energy$X5_sq<-q_energy$X5^2
q_energy$X6_sq<-q_energy$X6^2
q_energy$X7_sq<-q_energy$X7^2
q_energy$X8_sq<-q_energy$X8^2
q_energy$Y1_sq<-q_energy$Y1^2
q_energy$Y2_sq<-q_energy$Y2^2

#Linear Regression
#X1~X5+X4+Y2+X7+Y1+X3+X6+X8

Quad1 <- lm(X1~X5+X5_sq,data=q_energy)
Quad2 <- lm(X1~X5+X5_sq+X4+X4_sq,data=q_energy)
Quad3 <- lm(X1~X5+X5_sq+X4+X4_sq+Y2+Y2_sq,data=q_energy)
Quad4 <- lm(X1~X5+X5_sq+X4+X4_sq+Y2+Y2_sq+X7+X7_sq,data=q_energy)
Quad5 <- lm(X1~X5+X5_sq+X4+X4_sq+Y2+Y2_sq+X7+X7_sq+Y1+Y1_sq,data=q_energy)
Quad6 <- lm(X1~X5+X5_sq+X4+X4_sq+Y2+Y2_sq+X7+X7_sq+Y1+Y1_sq+X3+X3_sq,data=q_energy)
Quad7 <- lm(X1~X5+X5_sq+X4+X4_sq+Y2+Y2_sq+X7+X7_sq+Y1+Y1_sq+X3+X3_sq+X6+X6_sq,data=q_energy)
Quad8 <- lm(X1~X5+X5_sq+X4+X4_sq+Y2+Y2_sq+X7+X7_sq+Y1+Y1_sq+X3+X3_sq+X6+X6_sq+X8+X8_sq,data=q_energy)

#Cross-Validation
cv_quad_ns1 <- train(X1~X5+X5_sq, data = q_energy, trControl = train_control, method = "lm")
cv_quad_ns2 <- train(X1~X5+X5_sq+X4+X4_sq, data = q_energy, trControl = train_control, method = "lm")
cv_quad_ns3 <- train(X1~X5+X5_sq+X4+X4_sq+Y2+Y2_sq, data = q_energy, trControl = train_control, method = "lm")
cv_quad_ns4 <- train(X1~X5+X5_sq+X4+X4_sq+Y2+Y2_sq+X7+X7_sq, data = q_energy, trControl = train_control, method = "lm")
cv_quad_ns5 <- train(X1~X5+X5_sq+X4+X4_sq+Y2+Y2_sq+X7+X7_sq+Y1+Y1_sq, data = q_energy, trControl = train_control, method = "lm")
cv_quad_ns6 <- train(X1~X5+X5_sq+X4+X4_sq+Y2+Y2_sq+X7+X7_sq+Y1+Y1_sq+X3+X3_sq, data = q_energy, trControl = train_control, method = "lm")
cv_quad_ns7 <- train(X1~X5+X5_sq+X4+X4_sq+Y2+Y2_sq+X7+X7_sq+Y1+Y1_sq+X3+X3_sq+X6+X6_sq, data = q_energy, trControl = train_control, method = "lm")
cv_quad_ns8 <- train(X1~X5+X5_sq+X4+X4_sq+Y2+Y2_sq+X7+X7_sq+Y1+Y1_sq+X3+X3_sq+X6+X6_sq+X8+X8_sq, data = q_energy, trControl = train_control, method = "lm")

#DataFrames:-
frame_ns3 <- data.frame("adj_r_sq" = double(0),"r_sq" = double(0), "cv_r_sq" = double(0))
frame_ns3<- add_row(frame_ns3, r_sq = summary(Quad1)$r.squared, adj_r_sq = summary(Quad1)$adj.r.squared, cv_r_sq = mean(cv_quad_ns1$resample$Rsquared))
frame_ns3<- add_row(frame_ns3, r_sq = summary(Quad2)$r.squared, adj_r_sq = summary(Quad2)$adj.r.squared, cv_r_sq = mean(cv_quad_ns2$resample$Rsquared))
frame_ns3<- add_row(frame_ns3, r_sq = summary(Quad3)$r.squared, adj_r_sq = summary(Quad3)$adj.r.squared, cv_r_sq = mean(cv_quad_ns3$resample$Rsquared))
frame_ns3<- add_row(frame_ns3, r_sq = summary(Quad4)$r.squared, adj_r_sq = summary(Quad4)$adj.r.squared, cv_r_sq = mean(cv_quad_ns4$resample$Rsquared))
frame_ns3<- add_row(frame_ns3, r_sq = summary(Quad5)$r.squared, adj_r_sq = summary(Quad5)$adj.r.squared, cv_r_sq = mean(cv_quad_ns5$resample$Rsquared))
frame_ns3<- add_row(frame_ns3, r_sq = summary(Quad6)$r.squared, adj_r_sq = summary(Quad6)$adj.r.squared, cv_r_sq = mean(cv_quad_ns6$resample$Rsquared))
frame_ns3<- add_row(frame_ns3, r_sq = summary(Quad7)$r.squared, adj_r_sq = summary(Quad7)$adj.r.squared, cv_r_sq = mean(cv_quad_ns7$resample$Rsquared))

frame_ns3<- add_row(frame_ns3, r_sq = summary(Quad8)$r.squared, adj_r_sq = summary(Quad8)$adj.r.squared, cv_r_sq = mean(cv_quad_ns8$resample$Rsquared))

#Plotting The Values
plot(frame_ns3$adj_r_sq,type="l",col="red",main = "QuadPLOT",ylab="Variation",ylim = c(0,1))
lines(frame_ns3$r_sq,col="green")
lines(frame_ns3$cv_r_sq,col="Blue")
legend(2,1,legend = c("adj_r_square","r_square","cv_r_square"),col=c("red","green","blue"),lty = 1:2,cex = 0.8)


#Response Surface
#X1~X5+X4+Y2+X7+Y1+X3+X6+X8

resp<-energy
rs_md1<-rsm(X1~SO(X5,X4),data=resp)

rs_md2<-rsm(X1~SO(X5,X4,Y2),data=resp)
rs_md3<-rsm(X1~SO(X5,X4,Y2),data=resp)
rs_md4<-rsm(X1~SO(X5,X4,Y2,X7),data=resp)
rs_md5<-rsm(X1~SO(X5,X4,Y2,X7,Y1),data=resp)
rs_md6<-rsm(X1~SO(X5,X4,Y2,X7,Y1,X3),data=resp)
rs_md7<-rsm(X1~SO(X5,X4,Y2,X7,Y1,X3,X6),data=resp)
rs_md8<-rsm(X1~SO(X5,X4,Y2,X7,Y1,X3,X6,X8),data=resp)

#DataFrame
frame_ns4 <-data.frame("adj_r_square" = double(0), "r_square" = double(0))
frame_ns4 <- add_row(frame_ns4, adj_r_square = summary(rs_md1)$adj.r.squared, r_square = summary(rs_md1)$r.squared)
frame_ns4 <- add_row(frame_ns4, adj_r_square = summary(rs_md2)$adj.r.squared, r_square = summary(rs_md2)$r.squared)
frame_ns4 <- add_row(frame_ns4, adj_r_square = summary(rs_md3)$adj.r.squared, r_square = summary(rs_md3)$r.squared)
frame_ns4 <- add_row(frame_ns4, adj_r_square = summary(rs_md4)$adj.r.squared, r_square = summary(rs_md4)$r.squared)
frame_ns4 <- add_row(frame_ns4, adj_r_square = summary(rs_md5)$adj.r.squared, r_square = summary(rs_md5)$r.squared)
frame_ns4 <- add_row(frame_ns4, adj_r_square = summary(rs_md6)$adj.r.squared, r_square = summary(rs_md6)$r.squared)
frame_ns4 <- add_row(frame_ns4, adj_r_square = summary(rs_md7)$adj.r.squared, r_square = summary(rs_md7)$r.squared)
frame_ns4 <- add_row(frame_ns4, adj_r_square = summary(rs_md8)$adj.r.squared, r_square = summary(rs_md8)$r.squared)

#Plotting The Values
plot(frame_ns4$adj_r_square,type="l",col="red",main = "ReponseSurfacePLOT",ylab="Variation",ylim = c(0,1))
lines(frame_ns4$r_square,col="green")
legend(2,1,legend = c("adj_r_square","r_square"),col=c("red","green"),lty = 1:2,cex = 0.8)