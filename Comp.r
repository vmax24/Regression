library(ggpubr)
library(ggplot2)
library(dplyr) #add rows
library(lmridge) #for ridge regression
library(lars) # lasso regression
library(caret) # for cross validation
library(gdata) #to read xls file format
library(rsm) # for response surface

comp1<-read.csv(file.choose(),header = T)
str(comp1)


#Changing Col Names
colnames(comp1)

names(comp1)<-cbind("adv","x1","x2","x3","x4","x5","x6","x7","x8","x9")



#Converting Adv and X1 into num
comp1$adv <- as.numeric(as.factor(comp1$adv))
comp1$adv[is.na(comp1$adv)] <- mean(comp1$adv, na.rm = TRUE)
comp1$x1 <- as.numeric(as.factor(comp1$x1))
comp1$adv[is.na(comp1$x1)] <- mean(comp1$x1, na.rm = TRUE)


#Mean Imputation
comp1$adv[is.na(comp1$adv)] <- mean(comp1$adv, na.rm = TRUE)
comp1$adv[is.na(comp1$x1)] <- mean(comp1$x1, na.rm = TRUE)
comp1$adv[is.na(comp1$x2)] <- mean(comp1$x2, na.rm = TRUE)
comp1$adv[is.na(comp1$x3)] <- mean(comp1$x3, na.rm = TRUE)
comp1$adv[is.na(comp1$x4)] <- mean(comp1$x4, na.rm = TRUE)
comp1$adv[is.na(comp1$x5)] <- mean(comp1$x5, na.rm = TRUE)
comp1$adv[is.na(comp1$x6)] <- mean(comp1$x6, na.rm = TRUE)
comp1$adv[is.na(comp1$x7)] <- mean(comp1$x7, na.rm = TRUE)
comp1$adv[is.na(comp1$x8)] <- mean(comp1$x8, na.rm = TRUE)
comp1$adv[is.na(comp1$x9)] <- mean(comp1$x9, na.rm = TRUE)


#Forward Selection
fwd_model_comp <- lm(adv ~ 1,  data = comp1) 
step(fwd_model_comp, direction = "forward", scope = formula(adv ~ x1 + x2 + x3 + x4 + x5+x6+x7+x8+x9))

#lm(formula = adv ~ x1 + x3 + x5, data = comp1)

#adv ~ x1 + x3 + x5+x4+x6+x7+x8+x9.

#Linear Regression
lin_nos1 <- lm(adv ~ x1, data = comp1)
lin_nos2 <- lm(adv ~ x1+x3, data = comp1)
lin_nos3 <- lm(adv ~ x1+x3+x5, data = comp1)
lin_nos4 <- lm(adv ~ x1+x3+x5+x6, data = comp1)
lin_nos5 <- lm(adv ~ x1+x3+x5+x6+x7, data = comp1)
lin_nos6 <- lm(adv ~ x1+x3+x5+x6+x7+x8, data = comp1)
lin_nos7 <- lm(adv ~ x1+x3+x5+x6+x7+x8+x9, data = comp1)


#Cross-Validation
train_control <- trainControl(method = "cv", number = 10)
cv_nsmodel1 <- train(adv ~ x1, data = comp1, trControl = train_control, method = "lm")
cv_nsmodel2 <- train(adv ~ x1+x3, data = comp1, trControl = train_control, method = "lm")
cv_nsmodel3 <- train(adv ~ x1+x3+x5, data = comp1, trControl = train_control, method = "lm")
cv_nsmodel4 <- train(adv ~ x1+x3+x5+x6, data = comp1, trControl = train_control, method = "lm")
cv_nsmodel5 <- train(adv ~ x1+x3+x5+x6+x7, data = comp1, trControl = train_control, method = "lm")
cv_nsmodel6 <- train(adv ~ x1+x3+x5+x6+x7+x8, data = comp1, trControl = train_control, method = "lm")
cv_nsmodel7 <- train(adv ~ x1+x3+x5+x6+x7+x8+x9, data = comp1, trControl = train_control, method = "lm")

#DataFrames
frame_ns<-data.frame("r_sq" = double(0), "adj_r_sq" = double(0), "cv_r_sq" = double(0))
frame_ns<- add_row(frame_ns, r_sq = summary(lin_nos1)$r.squared, adj_r_sq = summary(lin_nos1)$adj.r.squared, cv_r_sq = mean(cv_nsmodel1$resample$Rsquared))
frame_ns<- add_row(frame_ns, r_sq = summary(lin_nos2)$r.squared, adj_r_sq = summary(lin_nos2)$adj.r.squared, cv_r_sq = mean(cv_nsmodel2$resample$Rsquared))
frame_ns<- add_row(frame_ns, r_sq = summary(lin_nos3)$r.squared, adj_r_sq = summary(lin_nos3)$adj.r.squared, cv_r_sq = mean(cv_nsmodel3$resample$Rsquared))
frame_ns<- add_row(frame_ns, r_sq = summary(lin_nos4)$r.squared, adj_r_sq = summary(lin_nos4)$adj.r.squared, cv_r_sq = mean(cv_nsmodel4$resample$Rsquared))
frame_ns<- add_row(frame_ns, r_sq = summary(lin_nos5)$r.squared, adj_r_sq = summary(lin_nos5)$adj.r.squared, cv_r_sq = mean(cv_nsmodel5$resample$Rsquared))
frame_ns<- add_row(frame_ns, r_sq = summary(lin_nos6)$r.squared, adj_r_sq = summary(lin_nos6)$adj.r.squared, cv_r_sq = mean(cv_nsmodel6$resample$Rsquared))
frame_ns<- add_row(frame_ns, r_sq = summary(lin_nos7)$r.squared, adj_r_sq = summary(lin_nos7)$adj.r.squared, cv_r_sq = mean(cv_nsmodel7$resample$Rsquared))

#Plotting The values

plot(frame_ns$r_sq, type = 'l', col = 'red', main = "Linear Plot", ylab = "Errors", ylim = c(0,1))
lines(frame_ns$adj_r_sq,  col = 'green' )
lines(frame_ns$cv_r_sq,  col = 'blue')
legend(5,0.8, legend = c("R Squared","Adj R Squared","R Squared CV"), 
       col = c("red","green","blue"), lty = 1:2, cex = 0.8)


#Ridge Regression
#adv ~ x1 + x3 + x5+x4+x6+x7+x8+x9.
ridge_ns1 <- lmridge(adv ~ x1 + x3, comp1, K = c(0.1, 0.001))
ridge_ns2 <- lmridge(adv ~ x1 + x3+x5, comp1, K = c(0.1, 0.001))
ridge_ns3 <- lmridge(adv ~ x1 + x3+x5+x4, comp1, K = c(0.1, 0.001))
ridge_ns4 <- lmridge(adv ~ x1 + x3+x5+x4+x6, comp1, K = c(0.1, 0.001))
ridge_ns5 <- lmridge(adv ~ x1 + x3+x5+x4+x6+x7, comp1, K = c(0.1, 0.001))
ridge_ns6 <- lmridge(adv ~ x1 + x3+x5+x4+x6+x7+x8, comp1, K = c(0.1, 0.001))
ridge_ns7 <- lmridge(adv ~ x1 + x3+x5+x4+x6+x7+x8+x9, comp1, K = c(0.1, 0.001))

#Cross-Validation
cv_ridge_ns1 <- train(adv ~ x1 + x3, data = comp1, trControl = train_control, method = "ridge")
cv_ridge_ns2 <- train(adv ~ x1 + x3+x5, data = comp1, trControl = train_control, method = "ridge")
cv_ridge_ns3 <- train(adv ~ x1 + x3+x5+x4, data = comp1, trControl = train_control, method = "ridge")
cv_ridge_ns4 <- train(adv ~ x1 + x3+x5+x4+x6, data = comp1, trControl = train_control, method = "ridge")
cv_ridge_ns5 <- train(adv ~ x1 + x3+x5+x4+x6+x7, data = comp1, trControl = train_control, method = "ridge")
cv_ridge_ns6 <- train(adv ~ x1 + x3+x5+x4+x6+x7+x8, data = comp1, trControl = train_control, method = "ridge")
cv_ridge_ns7 <- train(adv ~ x1 + x3+x5+x4+x6+x7+x8+x9, data = comp1, trControl = train_control, method = "ridge")

#DataFrame
frame_ns1 <-data.frame("r_sq" = double(0), "adj_r_sq" = double(0), "cv_r_sq" = double(0))
frame_ns1 <- add_row(frame_ns1, r_sq = max(rstats1(ridge_ns1)$R2), adj_r_sq = max(rstats1(ridge_ns1)$adjR2), cv_r_sq = mean(cv_ridge_ns1$resample$Rsquared))
frame_ns1 <- add_row(frame_ns1, r_sq = max(rstats1(ridge_ns2)$R2), adj_r_sq = max(rstats1(ridge_ns2)$adjR2), cv_r_sq = mean(cv_ridge_ns2$resample$Rsquared))
frame_ns1 <- add_row(frame_ns1, r_sq = max(rstats1(ridge_ns3)$R2), adj_r_sq = max(rstats1(ridge_ns3)$adjR2), cv_r_sq = mean(cv_ridge_ns3$resample$Rsquared))
frame_ns1 <- add_row(frame_ns1, r_sq = max(rstats1(ridge_ns4)$R2), adj_r_sq = max(rstats1(ridge_ns4)$adjR2), cv_r_sq = mean(cv_ridge_ns4$resample$Rsquared))
frame_ns1 <- add_row(frame_ns1, r_sq = max(rstats1(ridge_ns5)$R2), adj_r_sq = max(rstats1(ridge_ns5)$adjR2), cv_r_sq = mean(cv_ridge_ns5$resample$Rsquared))
frame_ns1 <- add_row(frame_ns1, r_sq = max(rstats1(ridge_ns6)$R2), adj_r_sq = max(rstats1(ridge_ns6)$adjR2), cv_r_sq = mean(cv_ridge_ns6$resample$Rsquared))
frame_ns1 <- add_row(frame_ns1, r_sq = max(rstats1(ridge_ns7)$R2), adj_r_sq = max(rstats1(ridge_ns7)$adjR2), cv_r_sq = mean(cv_ridge_ns7$resample$Rsquared))

#Plotting The Values
plot(frame_ns1$adj_r_sq,type="l",col="red",main = "RidgePlot",ylab="Variation",ylim = c(0,1))
lines(frame_ns1$r_sq,col="green")
lines(frame_ns1$cv_r_sq,col="Blue")
legend(2,1,legend = c("adj_r_square","r_square","cv_r_square"),col=c("red","green","blue"),lty = 1:2,cex = 0.8)


#Lasso Regression
#adv ~ x1 + x3 + x5+x4+x6+x7+x8+x9.
x <-( comp1$x1)
x <-cbind(comp1$x3,x)
lasso_mod1 <- lars(x, comp1$adv, type = 'lasso')

x <-(comp1$x1)
x <-cbind(comp1$x3,x)
x <-cbind(comp1$x5,x)

lasso_mod2 <- lars(x, comp1$adv, type = 'lasso')

x <-(comp1$x1)
x <-cbind(comp1$x3,x)
x <-cbind(comp1$x5,x)
x <-cbind(comp1$x4,x)

lasso_mod3 <- lars(x, comp1$adv, type = 'lasso')

x <-(comp1$x1)
x <-cbind(comp1$x3,x)
x <-cbind(comp1$x5,x)
x <-cbind(comp1$x4,x)
x <-cbind(comp1$x6,x)
lasso_mod5 <- lars(x, comp1$adv, type = 'lasso')

x <-(comp1$x1)
x <-cbind(comp1$x3,x)
x <-cbind(comp1$x5,x)
x <-cbind(comp1$x4,x)
x <-cbind(comp1$x6,x)
x <-cbind(comp1$x7,x)
lasso_mod6 <- lars(x, comp1$adv, type = 'lasso')

x <-(comp1$x1)
x <-cbind(comp1$x3,x)
x <-cbind(comp1$x5,x)
x <-cbind(comp1$x4,x)
x <-cbind(comp1$x6,x)
x <-cbind(comp1$x7,x)
x <-cbind(comp1$x8,x)
lasso_mod7<- lars(x, comp1$adv, type = 'lasso')

x <-(comp1$x1)
x <-cbind(comp1$x3,x)
x <-cbind(comp1$x5,x)
x <-cbind(comp1$x4,x)
x <-cbind(comp1$x6,x)
x <-cbind(comp1$x7,x)
x <-cbind(comp1$x8,x)
x <-cbind(comp1$x9,x)
lasso_mod8<- lars(x, comp1$adv, type = 'lasso')

#Cross-Validation
#adv ~ x1 + x3 + x5+x4+x6+x7+x8+x9
cv_lasso_ns1<-train(adv~x1+x3,comp1,method="lasso",trControl=train_control)
cv_lasso_ns2<-train(adv~x1+x3+x5,comp1,method="lasso",trControl=train_control)
cv_lasso_ns3<-train(adv~x1+x3+x5+x4,comp1,method="lasso",trControl=train_control)
cv_lasso_ns4<-train(adv~x1+x3+x5+x4+x6,comp1,method="lasso",trControl=train_control)
cv_lasso_ns5<-train(adv~x1+x3+x5+x4+x6+x7,comp1,method="lasso",trControl=train_control)
cv_lasso_ns6<-train(adv~x1+x3+x5+x4+x6+x7+x8,comp1,method="lasso",trControl=train_control)
cv_lasso_ns7<-train(adv~x1+x3+x5+x4+x6+x7+x8+x9,comp1,method="lasso",trControl=train_control)

#DataFrames
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




#Quad Regression
#adv ~ x1 + x3 + x5+x4+x6+x7+x8+x9
q_nas<-comp1
q_nas$x11<-q_nas$x1^2
q_nas$x21<-q_nas$x2^2
q_nas$x31<-q_nas$x3^2
q_nas$x41<-q_nas$x4^2
q_nas$x51<-q_nas$x5^2
q_nas$x61<-q_nas$x6^2
q_nas$x71<-q_nas$x7^2
q_nas$x81<-q_nas$x8^2
q_nas$x91<-q_nas$x9^2

#Linear Regression
Quad1 <- lm(adv~x1+x11,data=q_nas)
Quad2 <- lm(adv~x1+x11+x2+x21,data=q_nas)
Quad3 <- lm(adv~x1+x11+x2+x21+x3+x31,data=q_nas)
Quad4 <- lm(adv~x1+x11+x2+x21+x3+x31+x4+x41,data=q_nas)
Quad5 <- lm(adv~x1+x11+x2+x21+x3+x31+x4+x41+x5+x51,data=q_nas)
Quad6 <- lm(adv~x1+x11+x2+x21+x3+x31+x4+x41+x5+x51+x6+x61,data=q_nas)
Quad7 <- lm(adv~x1+x11+x2+x21+x3+x31+x4+x41+x5+x51+x6+x61+x7+x71,data=q_nas)
Quad8 <- lm(adv~x1+x11+x2+x21+x3+x31+x4+x41+x5+x51+x6+x61+x7+x71+x8+x81,data=q_nas)
Quad9 <- lm(adv~x1+x11+x2+x21+x3+x31+x4+x41+x5+x51+x6+x61+x7+x71+x8+x81+x9+x91,data=q_nas)

#Cross-Validation
#adv ~ x1 + x3 + x5+x4+x6+x7+x8+x9
cv_quad_ns1 <- train(adv ~ x1+x11, data = q_nas, trControl = train_control, method = "lm")
cv_quad_ns2 <- train(adv ~ x1+x11+x2+x21, data = q_nas, trControl = train_control, method = "lm")
cv_quad_ns3 <- train(adv ~ x1+x11+x2+x21+x3+x31, data = q_nas, trControl = train_control, method = "lm")
cv_quad_ns4 <- train(adv ~ x1+x11+x2+x21+x3+x31+x4+x41, data = q_nas, trControl = train_control, method = "lm")
cv_quad_ns5 <- train(adv ~ x1+x11+x2+x21+x3+x31+x4+x41+x5+x51, data = q_nas, trControl = train_control, method = "lm")
cv_quad_ns6 <- train(adv ~ x1+x11+x2+x21+x3+x31+x4+x41+x5+x51+x6+x61, data = q_nas, trControl = train_control, method = "lm")
cv_quad_ns7 <- train(adv ~ x1+x11+x2+x21+x3+x31+x4+x41+x5+x51+x6+x61+x7+x71, data = q_nas, trControl = train_control, method = "lm")
cv_quad_ns8 <- train(adv ~ x1+x11+x2+x21+x3+x31+x4+x41+x5+x51+x6+x61+x7+x71+x8+x81, data = q_nas, trControl = train_control, method = "lm")
cv_quad_ns9 <- train(adv ~ x1+x11+x2+x21+x3+x31+x4+x41+x5+x51+x6+x61+x7+x71+x8+x81+x9+x91, data = q_nas, trControl = train_control, method = "lm")


#DataFrames
frame_ns3 <- data.frame("adj_r_sq" = double(0),"r_sq" = double(0), "cv_r_sq" = double(0))
frame_ns3<- add_row(frame_ns3, r_sq = summary(Quad1)$r.squared, adj_r_sq = summary(Quad1)$adj.r.squared, cv_r_sq = mean(cv_quad_ns1$resample$Rsquared))
frame_ns3<- add_row(frame_ns3, r_sq = summary(Quad2)$r.squared, adj_r_sq = summary(Quad2)$adj.r.squared, cv_r_sq = mean(cv_quad_ns2$resample$Rsquared))
frame_ns3<- add_row(frame_ns3, r_sq = summary(Quad3)$r.squared, adj_r_sq = summary(Quad3)$adj.r.squared, cv_r_sq = mean(cv_quad_ns3$resample$Rsquared))
frame_ns3<- add_row(frame_ns3, r_sq = summary(Quad4)$r.squared, adj_r_sq = summary(Quad4)$adj.r.squared, cv_r_sq = mean(cv_quad_ns4$resample$Rsquared))
frame_ns3<- add_row(frame_ns3, r_sq = summary(Quad5)$r.squared, adj_r_sq = summary(Quad5)$adj.r.squared, cv_r_sq = mean(cv_quad_ns5$resample$Rsquared))
frame_ns3<- add_row(frame_ns3, r_sq = summary(Quad6)$r.squared, adj_r_sq = summary(Quad6)$adj.r.squared, cv_r_sq = mean(cv_quad_ns6$resample$Rsquared))
frame_ns3<- add_row(frame_ns3, r_sq = summary(Quad7)$r.squared, adj_r_sq = summary(Quad7)$adj.r.squared, cv_r_sq = mean(cv_quad_ns7$resample$Rsquared))
frame_ns3<- add_row(frame_ns3, r_sq = summary(Quad8)$r.squared, adj_r_sq = summary(Quad8)$adj.r.squared, cv_r_sq = mean(cv_quad_ns8$resample$Rsquared))
frame_ns3<- add_row(frame_ns3, r_sq = summary(Quad9)$r.squared, adj_r_sq = summary(Quad9)$adj.r.squared, cv_r_sq = mean(cv_quad_ns9$resample$Rsquared))

#Plotting The Variables:-
plot(frame_ns3$adj_r_sq,type="l",col="red",main = "QuadPLOT",ylab="Variation",ylim = c(0,1))
lines(frame_ns3$r_sq,col="green")
lines(frame_ns3$cv_r_sq,col="Blue")
legend(2,1,legend = c("adj_r_square","r_square","cv_r_square"),col=c("red","green","blue"),lty = 1:2,cex = 0.8)


#Reponse Surface
#adv ~ x1 + x3 + x5+x4+x6+x7+x8+x9
resp<-comp1
rs_md1<-rsm(adv~SO(x1,x3),data=resp)
rs_md2<-rsm(adv~SO(x1,x3,x5),data=resp)
rs_md3<-rsm(adv~SO(x1,x3,x5,x4),data=resp)
rs_md4<-rsm(adv~SO(x1,x3,x5,x4,x6),data=resp)
rs_md5<-rsm(adv~SO(x1,x3,x5,x4,x6,x7),data=resp)
rs_md6<-rsm(adv~SO(x1,x3,x5,x4,x6,x7,x8),data=resp)
rs_md7<-rsm(adv~SO(x1,x3,x5,x4,x6,x7,x8,x9),data=resp)


#DataFrame
frame_ns4 <-data.frame("adj_r_square" = double(0), "r_square" = double(0))
frame_ns4 <- add_row(frame_ns4, adj_r_square = summary(rs_md1)$adj.r.squared, r_square = summary(rs_md1)$r.squared)
frame_ns4 <- add_row(frame_ns4, adj_r_square = summary(rs_md2)$adj.r.squared, r_square = summary(rs_md2)$r.squared)
frame_ns4 <- add_row(frame_ns4, adj_r_square = summary(rs_md3)$adj.r.squared, r_square = summary(rs_md3)$r.squared)
frame_ns4 <- add_row(frame_ns4, adj_r_square = summary(rs_md4)$adj.r.squared, r_square = summary(rs_md4)$r.squared)
frame_ns4 <- add_row(frame_ns4, adj_r_square = summary(rs_md5)$adj.r.squared, r_square = summary(rs_md5)$r.squared)
frame_ns4 <- add_row(frame_ns4, adj_r_square = summary(rs_md6)$adj.r.squared, r_square = summary(rs_md6)$r.squared)
frame_ns4 <- add_row(frame_ns4, adj_r_square = summary(rs_md7)$adj.r.squared, r_square = summary(rs_md7)$r.squared)


#Plotting The Values
plot(frame_ns4$adj_r_square,type="l",col="red",main = "ReponseSurfacePLOT",ylab="Variation",ylim = c(0,1))
 lines(frame_ns4$r_square,col="green")
 legend(2,1,legend = c("adj_r_square","r_square"),col=c("red","green"),lty = 1:2,cex = 0.8)