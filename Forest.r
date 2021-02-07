library(ggpubr)
library(ggplot2)
library(dplyr) #add rows
library(lmridge) #for ridge regression
library(lars) # lasso regression
library(caret) # for cross validation
library(gdata) #to read xls file format
library(rsm) # for response surface

forest<-read.csv(file.choose(),header = T)
#Determining structure of forest
str(forest)


#Changing Factor Values to num
forest$month <- as.numeric(as.factor(forest$month))
forest$day <- as.numeric(as.factor(forest$day))


#Changing Col Name
colnames(forest)
names(forest)<-c("x1","x2","x3","x4","x5","x6","x7","x8","x9","x10","x11","x12","x13")


#Mean Imputation
forest$adv[is.na(forest$x1)] <- mean(forest$x1, na.rm = TRUE)
forest$adv[is.na(forest$x2)] <- mean(forest$x2, na.rm = TRUE)
forest$adv[is.na(forest$x3)] <- mean(forest$x3, na.rm = TRUE)
forest$adv[is.na(forest$x4)] <- mean(forest$x4, na.rm = TRUE)
forest$adv[is.na(forest$x5)] <- mean(forest$x5, na.rm = TRUE)
forest$adv[is.na(forest$x6)] <- mean(forest$x6, na.rm = TRUE)
forest$adv[is.na(forest$x7)] <- mean(forest$x7, na.rm = TRUE)
forest$adv[is.na(forest$x8)] <- mean(forest$x8, na.rm = TRUE)
forest$adv[is.na(forest$x9)] <- mean(forest$x9, na.rm = TRUE)
forest$adv[is.na(forest$x10)] <- mean(forest$x10, na.rm = TRUE)
forest$adv[is.na(forest$x11)] <- mean(forest$x11, na.rm = TRUE)
forest$adv[is.na(forest$x12)] <- mean(forest$x12, na.rm = TRUE)
forest$adv[is.na(forest$x13)] <- mean(forest$x13, na.rm = TRUE)


#Forward Selection
fwd_model_forest <- lm(x1~ 1,  data = forest) 
step(fwd_model_forest, direction = "forward", scope = formula(x1 ~ x2 + x3 + x4 + x5 + x6 +x7+x8+x9+x10+x11+x12+x13))


#lm(formula = x1 ~ x2 + x6 + x10, data = forest)
x1 ~ x2 + x6 + x10+x3+x4+x5+x7+x8+x9
#Linear Regression
lin_nos1 <- lm(x1 ~ x2, data = forest)
lin_nos2 <- lm(x1 ~ x2+x6, data = forest)
lin_nos3 <- lm(x1 ~ x2+x6+x10, data = forest)
lin_nos4 <- lm(x1 ~ x2+x6+x10+x3, data = forest)
lin_nos5 <- lm(x1 ~ x2+x6+x10+x3+x4, data = forest)
lin_nos6 <- lm(x1 ~ x2+x6+x10+x3+x4+x5, data = forest)
lin_nos7 <- lm(x1 ~ x2+x6+x10+x3+x4+x5+x7, data = forest)
lin_nos8 <- lm(x1 ~ x2+x6+x10+x3+x4+x5+x7+x8, data = forest)
lin_nos9 <- lm(x1 ~ x2+x6+x10+x3+x4+x5+x7+x8+x9, data = forest)

#Cross-Validation on Linear Regression

train_control <- trainControl(method = "cv", number = 10)
cv_nsmodel1 <- train(x1 ~ x2, data = forest, trControl = train_control, method = "lm")
cv_nsmodel2 <- train(x1 ~ x2+x6, data = forest, trControl = train_control, method = "lm")
cv_nsmodel3 <- train(x1 ~ x2+x6+x10, data = forest, trControl = train_control, method = "lm")
cv_nsmodel4 <- train(x1 ~ x2+x6+x10+x3, data = forest, trControl = train_control, method = "lm")
cv_nsmodel5 <- train(x1 ~ x2+x6+x10+x3+x4, data = forest, trControl = train_control, method = "lm")
cv_nsmodel6 <- train(x1 ~ x2+x6+x10+x3+x4+x5, data = forest, trControl = train_control, method = "lm")
cv_nsmodel7 <- train(x1 ~ x2+x6+x10+x3+x4+x5+x7, data = forest, trControl = train_control, method = "lm")
cv_nsmodel8 <- train(x1 ~ x2+x6+x10+x3+x4+x5+x7+x8, data = forest, trControl = train_control, method = "lm")
cv_nsmodel9 <- train(x1 ~ x2+x6+x10+x3+x4+x5+x7+x8+x9, data = forest, trControl = train_control, method = "lm")

#DataFrames
frame_ns<-data.frame("r_sq" = double(0), "adj_r_sq" = double(0), "cv_r_sq" = double(0))
frame_ns<- add_row(frame_ns, r_sq = summary(lin_nos1)$r.squared, adj_r_sq = summary(lin_nos1)$adj.r.squared, cv_r_sq = mean(cv_nsmodel1$resample$Rsquared))
frame_ns<- add_row(frame_ns, r_sq = summary(lin_nos2)$r.squared, adj_r_sq = summary(lin_nos2)$adj.r.squared, cv_r_sq = mean(cv_nsmodel2$resample$Rsquared))
frame_ns<- add_row(frame_ns, r_sq = summary(lin_nos3)$r.squared, adj_r_sq = summary(lin_nos3)$adj.r.squared, cv_r_sq = mean(cv_nsmodel3$resample$Rsquared))
frame_ns<- add_row(frame_ns, r_sq = summary(lin_nos4)$r.squared, adj_r_sq = summary(lin_nos4)$adj.r.squared, cv_r_sq = mean(cv_nsmodel4$resample$Rsquared))
frame_ns<- add_row(frame_ns, r_sq = summary(lin_nos5)$r.squared, adj_r_sq = summary(lin_nos5)$adj.r.squared, cv_r_sq = mean(cv_nsmodel5$resample$Rsquared))
frame_ns<- add_row(frame_ns, r_sq = summary(lin_nos6)$r.squared, adj_r_sq = summary(lin_nos6)$adj.r.squared, cv_r_sq = mean(cv_nsmodel6$resample$Rsquared))
frame_ns<- add_row(frame_ns, r_sq = summary(lin_nos7)$r.squared, adj_r_sq = summary(lin_nos7)$adj.r.squared, cv_r_sq = mean(cv_nsmodel7$resample$Rsquared))
frame_ns<- add_row(frame_ns, r_sq = summary(lin_nos8)$r.squared, adj_r_sq = summary(lin_nos8)$adj.r.squared, cv_r_sq = mean(cv_nsmodel8$resample$Rsquared))
frame_ns<- add_row(frame_ns, r_sq = summary(lin_nos9)$r.squared, adj_r_sq = summary(lin_nos9)$adj.r.squared, cv_r_sq = mean(cv_nsmodel9$resample$Rsquared))

#Plotting The values

plot(frame_ns$r_sq, type = 'l', col = 'red', main = "Linear Plot", ylab = "Errors", ylim = c(0.1,1))
lines(frame_ns$adj_r_sq,  col = 'green' )
lines(frame_ns$cv_r_sq,  col = 'blue')
legend(5,0.8, legend = c("R Squared","Adj R Squared","R Squared CV"), 
       col = c("red","green","blue"), lty = 1:2, cex = 0.8)

#Ridge Regression
#x1 ~ x2+x6+x10+x3+x4+x5+x7+x8+x9
ridge_ns1 <- lmridge(x1 ~ x2 + x6, forest, K = c(0.1, 0.001))
ridge_ns2 <- lmridge(x1 ~ x2 + x6+x10, forest, K = c(0.1, 0.001))
ridge_ns3 <- lmridge(x1 ~ x2 + x6+x10+x3, forest, K = c(0.1, 0.001))
ridge_ns4 <- lmridge(x1 ~ x2 + x6+x10+x3+x4, forest, K = c(0.1, 0.001))
ridge_ns5 <- lmridge(x1 ~ x2 + x6+x10+x3+x4+x5, forest, K = c(0.1, 0.001))
ridge_ns6 <- lmridge(x1 ~ x2 + x6+x10+x3+x4+x5+x7, forest, K = c(0.1, 0.001))
ridge_ns7 <- lmridge(x1 ~ x2 + x6+x10+x3+x4+x5+x7+x8, forest, K = c(0.1, 0.001))
ridge_ns8 <- lmridge(x1 ~ x2 + x6+x10+x3+x4+x5+x7+x8+x9, forest, K = c(0.1, 0.001))

#Cross-Validation
cv_ridge_ns1 <- train(x1 ~ x2 + x6, data = forest, trControl = train_control, method = "ridge")
cv_ridge_ns2 <- train(x1 ~ x2 + x6+x10, data = forest, trControl = train_control, method = "ridge")
cv_ridge_ns3 <- train(x1 ~ x2 + x6+x10+x3, data = forest, trControl = train_control, method = "ridge")
cv_ridge_ns4 <- train(x1 ~ x2 + x6+x10+x3+x4, data = forest, trControl = train_control, method = "ridge")
cv_ridge_ns5 <- train(x1 ~ x2 + x6+x10+x3+x4+x5, data = forest, trControl = train_control, method = "ridge")
cv_ridge_ns6 <- train(x1 ~ x2 + x6+x10+x3+x4+x5+x7, data = forest, trControl = train_control, method = "ridge")
cv_ridge_ns7 <- train(x1 ~ x2 + x6+x10+x3+x4+x5+x7+x8, data = forest, trControl = train_control, method = "ridge")
cv_ridge_ns8 <- train(x1 ~ x2 + x6+x10+x3+x4+x5+x7+x8+x9, data = forest, trControl = train_control, method = "ridge")

#Dataframe
frame_ns1 <-data.frame("r_sq" = double(0), "adj_r_sq" = double(0), "cv_r_sq" = double(0))
frame_ns1 <- add_row(frame_ns1, r_sq = max(rstats1(ridge_ns1)$R2), adj_r_sq = max(rstats1(ridge_ns1)$adjR2), cv_r_sq = mean(cv_ridge_ns1$resample$Rsquared))
frame_ns1 <- add_row(frame_ns1, r_sq = max(rstats1(ridge_ns2)$R2), adj_r_sq = max(rstats1(ridge_ns2)$adjR2), cv_r_sq = mean(cv_ridge_ns2$resample$Rsquared))
frame_ns1 <- add_row(frame_ns1, r_sq = max(rstats1(ridge_ns3)$R2), adj_r_sq = max(rstats1(ridge_ns3)$adjR2), cv_r_sq = mean(cv_ridge_ns3$resample$Rsquared))
frame_ns1 <- add_row(frame_ns1, r_sq = max(rstats1(ridge_ns4)$R2), adj_r_sq = max(rstats1(ridge_ns4)$adjR2), cv_r_sq = mean(cv_ridge_ns4$resample$Rsquared))
frame_ns1 <- add_row(frame_ns1, r_sq = max(rstats1(ridge_ns5)$R2), adj_r_sq = max(rstats1(ridge_ns5)$adjR2), cv_r_sq = mean(cv_ridge_ns5$resample$Rsquared))
frame_ns1 <- add_row(frame_ns1, r_sq = max(rstats1(ridge_ns6)$R2), adj_r_sq = max(rstats1(ridge_ns6)$adjR2), cv_r_sq = mean(cv_ridge_ns6$resample$Rsquared))
frame_ns1 <- add_row(frame_ns1, r_sq = max(rstats1(ridge_ns7)$R2), adj_r_sq = max(rstats1(ridge_ns7)$adjR2), cv_r_sq = mean(cv_ridge_ns7$resample$Rsquared))
frame_ns1 <- add_row(frame_ns1, r_sq = max(rstats1(ridge_ns8)$R2), adj_r_sq = max(rstats1(ridge_ns8)$adjR2), cv_r_sq = mean(cv_ridge_ns8$resample$Rsquared))

#Plotting The variables
plot(frame_ns1$adj_r_sq,type="l",col="red",main = "RidgePlot",ylab="Variation",ylim = c(0,1))
lines(frame_ns1$r_sq,col="green")
lines(frame_ns1$cv_r_sq,col="Blue")
legend(2,1,legend = c("adj_r_square","r_square","cv_r_square"),col=c("red","green","blue"),lty = 1:2,cex = 0.8)


#Lasso Regression
#x1 ~ x2+x6+x10+x3+x4+x5+x7+x8+x9

x <-(forest$x2)
x <-cbind(forest$x6,x)
lasso_mod1 <- lars(x, forest$x1, type = 'lasso')

x <-(forest$x2)
x <-cbind(forest$x6,x)
x <-cbind(forest$x10,x)
lasso_mod2 <- lars(x, forest$x1, type = 'lasso')

x <-(forest$x2)
x <-cbind(forest$x6,x)
x <-cbind(forest$x10,x)
x <-cbind(forest$x3,x)
lasso_mod3 <- lars(x, forest$x1, type = 'lasso')

x <-(forest$x2)
x <-cbind(forest$x6,x)
x <-cbind(forest$x10,x)
x <-cbind(forest$x3,x)
x <-cbind(forest$x4,x)
lasso_mod4 <- lars(x, forest$x1, type = 'lasso')

x <-(forest$x2)
x <-cbind(forest$x6,x)
x <-cbind(forest$x10,x)
x <-cbind(forest$x3,x)
x <-cbind(forest$x4,x)
x <-cbind(forest$x5,x)
lasso_mod5 <- lars(x, forest$x1, type = 'lasso')

x <-(forest$x2)
x <-cbind(forest$x6,x)
x <-cbind(forest$x10,x)
x <-cbind(forest$x3,x)
x <-cbind(forest$x4,x)
x <-cbind(forest$x5,x)
x <-cbind(forest$x7,x)
lasso_mod6 <- lars(x, forest$x1, type = 'lasso')

x <-(forest$x2)
x <-cbind(forest$x6,x)
x <-cbind(forest$x10,x)
x <-cbind(forest$x3,x)
x <-cbind(forest$x4,x)
x <-cbind(forest$x5,x)
x <-cbind(forest$x7,x)
x <-cbind(forest$x8,x)
lasso_mod7 <- lars(x, forest$x1, type = 'lasso')

x <-(forest$x2)
x <-cbind(forest$x6,x)
x <-cbind(forest$x10,x)
x <-cbind(forest$x3,x)
x <-cbind(forest$x4,x)
x <-cbind(forest$x5,x)
x <-cbind(forest$x7,x)
x <-cbind(forest$x8,x)
x <-cbind(forest$x9,x)
lasso_mod8 <- lars(x, forest$x1, type = 'lasso')

#Cross-Validation
cv_ridge_ns1 <- train(x1 ~ x2 + x6, data = forest, trControl = train_control, method = "ridge")
cv_ridge_ns2 <- train(x1 ~ x2 + x6+x10, data = forest, trControl = train_control, method = "ridge")
cv_ridge_ns3 <- train(x1 ~ x2 + x6+x10+x3, data = forest, trControl = train_control, method = "ridge")
cv_ridge_ns4 <- train(x1 ~ x2 + x6+x10+x3+x4, data = forest, trControl = train_control, method = "ridge")
cv_ridge_ns5 <- train(x1 ~ x2 + x6+x10+x3+x4+x5, data = forest, trControl = train_control, method = "ridge")
cv_ridge_ns6 <- train(x1 ~ x2 + x6+x10+x3+x4+x5+x7, data = forest, trControl = train_control, method = "ridge")
cv_ridge_ns7 <- train(x1 ~ x2 + x6+x10+x3+x4+x5+x7+x8, data = forest, trControl = train_control, method = "ridge")
cv_ridge_ns8 <- train(x1 ~ x2 + x6+x10+x3+x4+x5+x7+x8+x9, data = forest, trControl = train_control, method = "ridge")

#DataFrame
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
#x1 ~ x2+x6+x10+x3+x4+x5+x7+x8+x9

#Squaring The Values
q_nas<-forest
q_nas$x11<-q_nas$x1^2
q_nas$x11<-q_nas$x1^2
q_nas$x21<-q_nas$x2^2
q_nas$x31<-q_nas$x3^2
q_nas$x41<-q_nas$x4^2
q_nas$x51<-q_nas$x5^2
q_nas$x61<-q_nas$x6^2
q_nas$x71<-q_nas$x7^2
q_nas$x81<-q_nas$x8^2
q_nas$x91<-q_nas$x9^2
q_nas$x101<-q_nas$x10^2

#Linear Regression
#x1 ~ x2+x6+x10+x3+x4+x5+x7+x8+x9
Quad1 <- lm(x1~x2+x21,data=q_nas)
Quad2 <- lm(x1~x2+x21+x6+x61,data=q_nas)
Quad3 <- lm(x1~x2+x21+x6+x61+x10+x101,data=q_nas)
Quad4 <- lm(x1~x2+x21+x6+x61+x10+x101+x3+x31,data=q_nas)
Quad5 <- lm(x1~x2+x21+x6+x61+x10+x101+x3+x31+x4+x41,data=q_nas)
Quad6 <- lm(x1~x2+x21+x6+x61+x10+x101+x3+x31+x4+x41+x5+x51,data=q_nas)
Quad7 <- lm(x1~x2+x21+x6+x61+x10+x101+x3+x31+x4+x41+x5+x51+x7+x71,data=q_nas)
Quad8 <- lm(x1~x2+x21+x6+x61+x10+x101+x3+x31+x4+x41+x5+x51+x7+x71+x8+x81,data=q_nas)
Quad9 <- lm(x1~x2+x21+x6+x61+x10+x101+x3+x31+x4+x41+x5+x51+x7+x71+x8+x81+x9+x91,data=q_nas)


#Cross-Validation
#x1 ~ x2+x6+x10+x3+x4+x5+x7+x8+x9
cv_quad_ns1 <- train(x1~x2+x21, data = q_nas, trControl = train_control, method = "lm")
cv_quad_ns2 <- train(x1~x2+x21+x6+x61, data = q_nas, trControl = train_control, method = "lm")
cv_quad_ns3 <- train(x1~x2+x21+x6+x61+x10+x101, data = q_nas, trControl = train_control, method = "lm")
cv_quad_ns4 <- train(x1~x2+x21+x6+x61+x10+x101+x3+x31, data = q_nas, trControl = train_control, method = "lm")
cv_quad_ns5 <- train(x1~x2+x21+x6+x61+x10+x101+x3+x31+x4+x41, data = q_nas, trControl = train_control, method = "lm")
cv_quad_ns6 <- train(x1~x2+x21+x6+x61+x10+x101+x3+x31+x4+x41+x5+x51, data = q_nas, trControl = train_control, method = "lm")
cv_quad_ns7 <- train(x1~x2+x21+x6+x61+x10+x101+x3+x31+x4+x41+x5+x51+x7+x71, data = q_nas, trControl = train_control, method = "lm")
cv_quad_ns8<- train(x1~x2+x21+x6+x61+x10+x101+x3+x31+x4+x41+x5+x51+x7+x71+x8+x81, data = q_nas, trControl = train_control, method = "lm")
cv_quad_ns9<- train(x1~x2+x21+x6+x61+x10+x101+x3+x31+x4+x41+x5+x51+x7+x71+x8+x81+x9+x91, data = q_nas, trControl = train_control, method = "lm")

#DataFrame
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

#Data Plot
plot(frame_ns3$adj_r_sq,type="l",col="red",main = "QuadPLOT",ylab="Variation",ylim = c(0,1))
lines(frame_ns3$r_sq,col="green")
lines(frame_ns3$cv_r_sq,col="Blue")
legend(2,1,legend = c("adj_r_square","r_square","cv_r_square"),col=c("red","green","blue"),lty = 1:2,cex = 0.8)

#Reponse Surface
#x1 ~ x2+x6+x10+x3+x4+x5+x7+x8+x9
resp<-forest
rs_md1<-rsm(x1~SO(x2,x3),data=resp)
rs_md2<-rsm(x1~SO(x2,x6,x10),data=resp)
rs_md3<-rsm(x1~SO(x2,x6,x10,x3),data=resp)
rs_md4<-rsm(x1~SO(x2,x6,x10,x3,x4),data=resp)
rs_md5<-rsm(x1~SO(x2,x6,x10,x3,x4,x5),data=resp)
rs_md6<-rsm(x1~SO(x2,x6,x10,x3,x4,x5,x7),data=resp)
rs_md7<-rsm(x1~SO(x2,x6,x10,x3,x4,x5,x7,x8),data=resp)
rs_md8<-rsm(x1~SO(x2,x6,x10,x3,x4,x5,x7,x8,x9),data=resp)

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
#Plotting The Values
plot(frame_ns4$adj_r_square,type="l",col="red",main = "ReponseSurfacePLOT",ylab="Variation",ylim = c(0,1))
lines(frame_ns4$r_square,col="green")
legend(2,1,legend = c("adj_r_square","r_square"),col=c("red","green"),lty = 1:2,cex = 0.8)
