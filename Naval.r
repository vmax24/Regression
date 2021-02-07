library(ggpubr)
library(ggplot2)
library(dplyr) #add rows
library(lmridge) #for ridge regression
library(lars) # lasso regression
library(caret) # for cross validation
library(gdata) #to read xls file format
library(rsm) # for response surface

naval<-read.csv(file.choose(),header = T)
str(naval)

names(naval)

#Mean Imputation 
naval$a[is.na(naval$a)] <- mean(naval$a, na.rm = TRUE)
naval$b[is.na(naval$b)] <- mean(naval$b, na.rm = TRUE)
naval$c[is.na(naval$c)] <- mean(naval$c, na.rm = TRUE)
naval$d[is.na(naval$d)] <- mean(naval$d, na.rm = TRUE)
naval$e[is.na(naval$e)] <- mean(naval$e, na.rm = TRUE)
naval$f[is.na(naval$f)] <- mean(naval$f, na.rm = TRUE)
naval$g[is.na(naval$g)] <- mean(naval$g, na.rm = TRUE)
naval$h[is.na(naval$h)] <- mean(naval$h, na.rm = TRUE)
naval$i[is.na(naval$I)] <- mean(naval$I, na.rm = TRUE)
naval$j[is.na(naval$j)] <- mean(naval$j, na.rm = TRUE)
naval$k[is.na(naval$k)] <- mean(naval$k, na.rm = TRUE)
naval$l[is.na(naval$l)] <- mean(naval$l, na.rm = TRUE)
naval$m[is.na(naval$m)] <- mean(naval$m, na.rm = TRUE)
naval$n[is.na(naval$n)] <- mean(naval$n, na.rm = TRUE)
naval$o[is.na(naval$o)] <- mean(naval$o, na.rm = TRUE)
naval$p[is.na(naval$p)] <- mean(naval$p, na.rm = TRUE)

#Forward Selection
fwd_model_naval <- lm(a~ 1,  data = naval) 
step(fwd_model_naval, direction = "forward", scope = formula(a ~ b + c +d + e + f + g + h + I + j + k + l + m + n + o +p))

str(naval)

#Linear Regression
lm(formula = a ~ b + f + d + I + n + o + e + m + p + l + k + 
     j + h + c, data = naval)#g

#Linear Regression
lin_nos1 <- lm(a ~ b, data = naval)
lin_nos2 <- lm(a ~ b+f, data = naval)
lin_nos3 <- lm(a ~ b+f+d, data = naval)
lin_nos4 <- lm(a ~ b+f+d+I, data = naval)
lin_nos5 <- lm(a ~ b+f+d+I+n, data = naval)
lin_nos6 <- lm(a ~ b+f+d+I+n+o, data = naval)
lin_nos7 <- lm(a ~ b+f+d+I+n+o+e, data = naval)
lin_nos8 <- lm(a ~ b+f+d+I+n+o+e+m, data = naval)
lin_nos9 <- lm(a ~ b+f+d+I+n+o+e+m+p, data = naval)
lin_nos10<- lm(a ~ b+f+d+I+n+o+e+m+p+l, data = naval)
lin_nos11<- lm(a ~ b+f+d+I+n+o+e+m+p+l+k, data = naval)
lin_nos12<- lm(a ~ b+f+d+I+n+o+e+m+p+l+k+j, data = naval)
lin_nos13<- lm(a ~ b+f+d+I+n+o+e+m+p+l+k+j+h, data = naval)
lin_nos14<- lm(a ~ b+f+d+I+n+o+e+m+p+l+k+j+h+c, data = naval)
lin_nos15<- lm(a ~ b+f+d+I+n+o+e+m+p+l+k+j+h+c+g, data = naval)

#Cross-Validation
train_control <- trainControl(method = "cv", number = 10)
cv_nsmodel1 <- train(a ~ b, data = naval, trControl = train_control, method = "lm")
cv_nsmodel2 <- train(a ~ b+f, data = naval, trControl = train_control, method = "lm")
cv_nsmodel3 <- train(a ~ b+f+d, data = naval, trControl = train_control, method = "lm")
cv_nsmodel4 <- train(a ~ b+f+d+I, data = naval, trControl = train_control, method = "lm")
cv_nsmodel5 <- train(a ~ b+f+d+I+n, data = naval, trControl = train_control, method = "lm")
cv_nsmodel6 <- train(a ~ b+f+d+I+n+o, data = naval, trControl = train_control, method = "lm")
cv_nsmodel7 <- train(a ~ b+f+d+I+n+o+e, data = naval, trControl = train_control, method = "lm")
cv_nsmodel8 <- train(a ~ b+f+d+I+n+o+e+m, data = naval, trControl = train_control, method = "lm")
cv_nsmodel9 <- train(a ~ b+f+d+I+n+o+e+m+p, data = naval, trControl = train_control, method = "lm")
cv_nsmodel9 <- train(a ~ b+f+d+I+n+o+e+m+p, data = naval, trControl = train_control, method = "lm")
cv_nsmodel10 <- train(a ~ b+f+d+I+n+o+e+m+p+l, data = naval, trControl = train_control, method = "lm")
cv_nsmodel11 <- train(a ~ b+f+d+I+n+o+e+m+p+l+k, data = naval, trControl = train_control, method = "lm")
cv_nsmodel12 <- train(a ~ b+f+d+I+n+o+e+m+p+l+k+j, data = naval, trControl = train_control, method = "lm")
cv_nsmodel13 <- train(a ~ b+f+d+I+n+o+e+m+p+l+k+j+h, data = naval, trControl = train_control, method = "lm")
cv_nsmodel14 <- train(a ~ b+f+d+I+n+o+e+m+p+l+k+j+h+c, data = naval, trControl = train_control, method = "lm")
cv_nsmodel15 <- train(a ~ b+f+d+I+n+o+e+m+p+l+k+j+h+c+g, data = naval, trControl = train_control, method = "lm")

#DataFrame:-
frame_ns<-data.frame("r_sq" = double(0), "adj_r_sq" = double(0), "cv_r_sq" = double(0))
frame_ns<- add_row(frame_ns, r_sq = summary(lin_nos1)$r.squared, adj_r_sq = summary(lin_nos1)$adj.r.squared, cv_r_sq = mean(cv_nsmodel1$resample$Rsquared))
frame_ns<- add_row(frame_ns, r_sq = summary(lin_nos2)$r.squared, adj_r_sq = summary(lin_nos2)$adj.r.squared, cv_r_sq = mean(cv_nsmodel1$resample$Rsquared))
frame_ns<- add_row(frame_ns, r_sq = summary(lin_nos3)$r.squared, adj_r_sq = summary(lin_nos3)$adj.r.squared, cv_r_sq = mean(cv_nsmodel1$resample$Rsquared))
frame_ns<- add_row(frame_ns, r_sq = summary(lin_nos4)$r.squared, adj_r_sq = summary(lin_nos4)$adj.r.squared, cv_r_sq = mean(cv_nsmodel1$resample$Rsquared))
frame_ns<- add_row(frame_ns, r_sq = summary(lin_nos5)$r.squared, adj_r_sq = summary(lin_nos5)$adj.r.squared, cv_r_sq = mean(cv_nsmodel1$resample$Rsquared))
frame_ns<- add_row(frame_ns, r_sq = summary(lin_nos6)$r.squared, adj_r_sq = summary(lin_nos6)$adj.r.squared, cv_r_sq = mean(cv_nsmodel1$resample$Rsquared))
frame_ns<- add_row(frame_ns, r_sq = summary(lin_nos7)$r.squared, adj_r_sq = summary(lin_nos7)$adj.r.squared, cv_r_sq = mean(cv_nsmodel1$resample$Rsquared))
frame_ns<- add_row(frame_ns, r_sq = summary(lin_nos8)$r.squared, adj_r_sq = summary(lin_nos8)$adj.r.squared, cv_r_sq = mean(cv_nsmodel1$resample$Rsquared))
frame_ns<- add_row(frame_ns, r_sq = summary(lin_nos9)$r.squared, adj_r_sq = summary(lin_nos9)$adj.r.squared, cv_r_sq = mean(cv_nsmodel1$resample$Rsquared))
frame_ns<- add_row(frame_ns, r_sq = summary(lin_nos10)$r.squared, adj_r_sq = summary(lin_nos10)$adj.r.squared, cv_r_sq = mean(cv_nsmodel1$resample$Rsquared))
frame_ns<- add_row(frame_ns, r_sq = summary(lin_nos11)$r.squared, adj_r_sq = summary(lin_nos11)$adj.r.squared, cv_r_sq = mean(cv_nsmodel1$resample$Rsquared))
frame_ns<- add_row(frame_ns, r_sq = summary(lin_nos12)$r.squared, adj_r_sq = summary(lin_nos12)$adj.r.squared, cv_r_sq = mean(cv_nsmodel1$resample$Rsquared))
frame_ns<- add_row(frame_ns, r_sq = summary(lin_nos13)$r.squared, adj_r_sq = summary(lin_nos13)$adj.r.squared, cv_r_sq = mean(cv_nsmodel1$resample$Rsquared))
frame_ns<- add_row(frame_ns, r_sq = summary(lin_nos14)$r.squared, adj_r_sq = summary(lin_nos14)$adj.r.squared, cv_r_sq = mean(cv_nsmodel1$resample$Rsquared))
frame_ns<- add_row(frame_ns, r_sq = summary(lin_nos15)$r.squared, adj_r_sq = summary(lin_nos15)$adj.r.squared, cv_r_sq = mean(cv_nsmodel1$resample$Rsquared))


#Plotting Values

plot(frame_ns$r_sq, type = 'l', col = 'red', main = "Linear Plot", ylab = "Errors", ylim = c(0.1,1))
lines(frame_ns$adj_r_sq,  col = 'green' )
lines(frame_ns$cv_r_sq,  col = 'blue')
legend(5,0.8, legend = c("R Squared","Adj R Squared","R Squared CV"), 
       col = c("red","green","blue"), lty = 1:2, cex = 0.8)

#Ridge Regression
#lm(formula = a ~ b + f + d + I + n + o + e + m + p + l + k + j + h + c, data = naval)
ridge_ns1 <- lmridge(a ~ b + f, naval, K = c(0.1, 0.001))
ridge_ns2 <- lmridge(a ~ b + f+d, naval, K = c(0.1, 0.001))
ridge_ns3 <- lmridge(a ~ b + f+d+I, naval, K = c(0.1, 0.001))
ridge_ns4 <- lmridge(a ~ b + f+d+I+n, naval, K = c(0.1, 0.001))
ridge_ns5 <- lmridge(a ~ b + f+d+I+n+o, naval, K = c(0.1, 0.001))
ridge_ns6 <- lmridge(a ~ b + f+d+I+n+o+e, naval, K = c(0.1, 0.001))
ridge_ns7 <- lmridge(a ~ b + f+d+I+n+o+e+m, naval, K = c(0.1, 0.001))
ridge_ns8 <- lmridge(a ~ b + f+d+I+n+o+e+m+p, naval, K = c(0.1, 0.001))
ridge_ns9 <- lmridge(a ~ b + f+d+I+n+o+e+m+p+l, naval, K = c(0.1, 0.001))
ridge_ns10 <- lmridge(a ~ b + f+d+I+n+o+e+m+p+l+k, naval, K = c(0.1, 0.001))
ridge_ns11 <- lmridge(a ~ b + f+d+I+n+o+e+m+p+l+k+j, naval, K = c(0.1, 0.001))
ridge_ns12 <- lmridge(a ~ b + f+d+I+n+o+e+m+p+l+k+j+h, naval, K = c(0.1, 0.001))
ridge_ns13 <- lmridge(a ~ b + f+d+I+n+o+e+m+p+l+k+j+h+c, naval, K = c(0.1, 0.001))
ridge_ns14 <- lmridge(a ~ b + f+d+I+n+o+e+m+p+l+k+j+h+c+g, naval, K = c(0.1, 0.001))

#Cross-Validation on Ridge
#lm(formula = a ~ b + f + d + I + n + o + e + m + p + l + k + j + h + c, data = naval)

cv_ridge_ns1 <- train(a ~ b + f, data = naval, trControl = train_control, method = "ridge")
cv_ridge_ns2 <- train(a ~ b + f+d, data = naval, trControl = train_control, method = "ridge")
cv_ridge_ns3 <- train(a ~ b + f+d+I, data = naval, trControl = train_control, method = "ridge")
cv_ridge_ns4 <- train(a ~ b + f+d+I+n, data = naval, trControl = train_control, method = "ridge")
cv_ridge_ns5 <- train(a ~ b + f+d+I+n+o, data = naval, trControl = train_control, method = "ridge")
cv_ridge_ns6 <- train(a ~ b + f+d+I+n+o+e, data = naval, trControl = train_control, method = "ridge")
cv_ridge_ns7 <- train(a ~ b + f+d+I+n+o+e+m, data = naval, trControl = train_control, method = "ridge")
cv_ridge_ns8 <- train(a ~ b + f+d+I+n+o+e+m+p, data = naval, trControl = train_control, method = "ridge")
cv_ridge_ns9 <- train(a ~ b + f+d+I+n+o+e+m+p+l, data = naval, trControl = train_control, method = "ridge")
cv_ridge_ns10 <- train(a ~ b + f+d+I+n+o+e+m+p+l+k, data = naval, trControl = train_control, method = "ridge")
cv_ridge_ns11 <- train(a ~ b + f+d+I+n+o+e+m+p+l+k+j, data = naval, trControl = train_control, method = "ridge")
cv_ridge_ns12 <- train(a ~ b + f+d+I+n+o+e+m+p+l+k+j+h, data = naval, trControl = train_control, method = "ridge")
cv_ridge_ns13 <- train(a ~ b + f+d+I+n+o+e+m+p+l+k+j+h+c, data = naval, trControl = train_control, method = "ridge")
cv_ridge_ns14 <- train(a ~ b + f+d+I+n+o+e+m+p+l+k+j+h+c+g, data = naval, trControl = train_control, method = "ridge")

#DataFrame
frame_ns1 <-data.frame("r_sq" = double(0), "adj_r_sq" = double(0), "cv_r_sq" = double(0))
frame_ns1 <- add_row(frame_ns1, r_sq = max(rstats1(ridge_ns1)$R2), adj_r_sq = max(rstats1(ridge_ns1)$adjR2), cv_r_sq = mean(cv_ridge_ns1$resample$Rsquared))
frame_ns1 <- add_row(frame_ns1, r_sq = max(rstats1(ridge_ns2)$R2), adj_r_sq = max(rstats1(ridge_ns2)$adjR2), cv_r_sq = mean(cv_ridge_ns2$resample$Rsquared))
frame_ns1 <- add_row(frame_ns1, r_sq = max(rstats1(ridge_ns3)$R2), adj_r_sq = max(rstats1(ridge_ns3)$adjR2), cv_r_sq = mean(cv_ridge_ns3$resample$Rsquared))
frame_ns1 <- add_row(frame_ns1, r_sq = max(rstats1(ridge_ns4)$R2), adj_r_sq = max(rstats1(ridge_ns4)$adjR2), cv_r_sq = mean(cv_ridge_ns4$resample$Rsquared))
frame_ns1 <- add_row(frame_ns1, r_sq = max(rstats1(ridge_ns5)$R2), adj_r_sq = max(rstats1(ridge_ns5)$adjR2), cv_r_sq = mean(cv_ridge_ns5$resample$Rsquared))
frame_ns1 <- add_row(frame_ns1, r_sq = max(rstats1(ridge_ns6)$R2), adj_r_sq = max(rstats1(ridge_ns6)$adjR2), cv_r_sq = mean(cv_ridge_ns6$resample$Rsquared))
frame_ns1 <- add_row(frame_ns1, r_sq = max(rstats1(ridge_ns7)$R2), adj_r_sq = max(rstats1(ridge_ns7)$adjR2), cv_r_sq = mean(cv_ridge_ns7$resample$Rsquared))
frame_ns1 <- add_row(frame_ns1, r_sq = max(rstats1(ridge_ns8)$R2), adj_r_sq = max(rstats1(ridge_ns8)$adjR2), cv_r_sq = mean(cv_ridge_ns8$resample$Rsquared))
frame_ns1 <- add_row(frame_ns1, r_sq = max(rstats1(ridge_ns9)$R2), adj_r_sq = max(rstats1(ridge_ns9)$adjR2), cv_r_sq = mean(cv_ridge_ns9$resample$Rsquared))
frame_ns1 <- add_row(frame_ns1, r_sq = max(rstats1(ridge_ns10)$R2), adj_r_sq = max(rstats1(ridge_ns10)$adjR2), cv_r_sq = mean(cv_ridge_ns10$resample$Rsquared))
frame_ns1 <- add_row(frame_ns1, r_sq = max(rstats1(ridge_ns11)$R2), adj_r_sq = max(rstats1(ridge_ns11)$adjR2), cv_r_sq = mean(cv_ridge_ns11$resample$Rsquared))
frame_ns1 <- add_row(frame_ns1, r_sq = max(rstats1(ridge_ns12)$R2), adj_r_sq = max(rstats1(ridge_ns12)$adjR2), cv_r_sq = mean(cv_ridge_ns12$resample$Rsquared))
frame_ns1 <- add_row(frame_ns1, r_sq = max(rstats1(ridge_ns13)$R2), adj_r_sq = max(rstats1(ridge_ns13)$adjR2), cv_r_sq = mean(cv_ridge_ns13$resample$Rsquared))
frame_ns1 <- add_row(frame_ns1, r_sq = max(rstats1(ridge_ns14)$R2), adj_r_sq = max(rstats1(ridge_ns14)$adjR2), cv_r_sq = mean(cv_ridge_ns14$resample$Rsquared))

#PLotting The Variables:-

plot(frame_ns1$r_sq, type = 'l', col = 'red', main = "Linear Plot", ylab = "Errors", ylim = c(0,1))
lines(frame_ns1$adj_r_sq,  col = 'green' )
lines(frame_ns1$cv_r_sq,  col = 'blue')
legend(5,0.8, legend = c("R Squared","Adj R Squared","R Squared CV"), 
       col = c("red","green","blue"), lty = 1:2, cex = 0.8)

#Lasso Regression
#lm(formula = a ~ b + f + d + I + n + o + e + m + p + l + k + j + h + c, data = naval)
x <-(naval$b)
x <-cbind(naval$f,x)
lasso_mod1 <- lars(x, naval$a, type = 'lasso')

x <-(naval$b)
x <-cbind(naval$f,x)
x <-cbind(naval$d,x)
lasso_mod2 <- lars(x, naval$a, type = 'lasso')

x <-(naval$b)
x <-cbind(naval$f,x)
x <-cbind(naval$d,x)
x <-cbind(naval$I,x)
lasso_mod3 <- lars(x, naval$a, type = 'lasso')

x <-(naval$b)
x <-cbind(naval$f,x)
x <-cbind(naval$d,x)
x <-cbind(naval$I,x)
x <-cbind(naval$n,x)
lasso_mod4 <- lars(x, naval$a, type = 'lasso')

x <-(naval$b)
x <-cbind(naval$f,x)
x <-cbind(naval$d,x)
x <-cbind(naval$I,x)
x <-cbind(naval$n,x)
x <-cbind(naval$o,x)
lasso_mod5 <- lars(x, naval$a, type = 'lasso')

x <-(naval$b)
x <-cbind(naval$f,x)
x <-cbind(naval$d,x)
x <-cbind(naval$I,x)
x <-cbind(naval$n,x)
x <-cbind(naval$o,x)
x <-cbind(naval$e,x)
lasso_mod6 <- lars(x, naval$a, type = 'lasso')

x <-(naval$b)
x <-cbind(naval$f,x)
x <-cbind(naval$d,x)
x <-cbind(naval$I,x)
x <-cbind(naval$n,x)
x <-cbind(naval$o,x)
x <-cbind(naval$e,x)
x <-cbind(naval$m,x)

lasso_mod7 <- lars(x, naval$a, type = 'lasso')

x <-(naval$b)
x <-cbind(naval$f,x)
x <-cbind(naval$d,x)
x <-cbind(naval$I,x)
x <-cbind(naval$n,x)
x <-cbind(naval$o,x)
x <-cbind(naval$e,x)
x <-cbind(naval$m,x)
x <-cbind(naval$p,x)

lasso_mod8 <- lars(x, naval$a, type = 'lasso')

x <-(naval$b)
x <-cbind(naval$f,x)
x <-cbind(naval$d,x)
x <-cbind(naval$I,x)
x <-cbind(naval$n,x)
x <-cbind(naval$o,x)
x <-cbind(naval$e,x)
x <-cbind(naval$m,x)
x <-cbind(naval$p,x)
x <-cbind(naval$l,x)
lasso_mod9 <- lars(x, naval$a, type = 'lasso')

x <-(naval$b)
x <-cbind(naval$f,x)
x <-cbind(naval$d,x)
x <-cbind(naval$I,x)
x <-cbind(naval$n,x)
x <-cbind(naval$o,x)
x <-cbind(naval$e,x)
x <-cbind(naval$m,x)
x <-cbind(naval$p,x)
x <-cbind(naval$l,x)
x <-cbind(naval$k,x)

lasso_mod10 <- lars(x, naval$a, type = 'lasso')

x <-(naval$b)
x <-cbind(naval$f,x)
x <-cbind(naval$d,x)
x <-cbind(naval$I,x)
x <-cbind(naval$n,x)
x <-cbind(naval$o,x)
x <-cbind(naval$e,x)
x <-cbind(naval$m,x)
x <-cbind(naval$p,x)
x <-cbind(naval$l,x)
x <-cbind(naval$k,x)
x <-cbind(naval$j,x)


lasso_mod11 <- lars(x, naval$a, type = 'lasso')

x <-(naval$b)
x <-cbind(naval$f,x)
x <-cbind(naval$d,x)
x <-cbind(naval$I,x)
x <-cbind(naval$n,x)
x <-cbind(naval$o,x)
x <-cbind(naval$e,x)
x <-cbind(naval$m,x)
x <-cbind(naval$p,x)
x <-cbind(naval$l,x)
x <-cbind(naval$k,x)
x <-cbind(naval$j,x)
x <-cbind(naval$h,x)
lasso_mod12 <- lars(x, naval$a, type = 'lasso')

x <-(naval$b)
x <-cbind(naval$f,x)
x <-cbind(naval$d,x)
x <-cbind(naval$I,x)
x <-cbind(naval$n,x)
x <-cbind(naval$o,x)
x <-cbind(naval$e,x)
x <-cbind(naval$m,x)
x <-cbind(naval$p,x)
x <-cbind(naval$l,x)
x <-cbind(naval$k,x)
x <-cbind(naval$j,x)
x <-cbind(naval$h,x)
x <-cbind(naval$c,x)
lasso_mod13 <- lars(x, naval$a, type = 'lasso')

x <-(naval$b)
x <-cbind(naval$f,x)
x <-cbind(naval$d,x)
x <-cbind(naval$I,x)
x <-cbind(naval$n,x)
x <-cbind(naval$o,x)
x <-cbind(naval$e,x)
x <-cbind(naval$m,x)
x <-cbind(naval$p,x)
x <-cbind(naval$l,x)
x <-cbind(naval$k,x)
x <-cbind(naval$j,x)
x <-cbind(naval$h,x)
x <-cbind(naval$c,x)
x <-cbind(naval$g)
lasso_mod14 <- lars(x, naval$a, type = 'lasso')

#Cross-Validation
#lm(formula = a ~ b + f + d + I + n + o + e + m + p + l + k + j + h + c, data = naval)
cv_lasso_ns1<-train(a~b+f,naval,method="lasso",trControl=train_control)
cv_lasso_ns2<-train(a~b+f+d,naval,method="lasso",trControl=train_control)
cv_lasso_ns3<-train(a~b+f+d+I,naval,method="lasso",trControl=train_control)
cv_lasso_ns4<-train(a~b+f+d+I+n,naval,method="lasso",trControl=train_control)
cv_lasso_ns5<-train(a~b+f+d+I+n+o,naval,method="lasso",trControl=train_control)
cv_lasso_ns6<-train(a~b+f+d+I+n+o+e,naval,method="lasso",trControl=train_control)
cv_lasso_ns7<-train(a~b+f+d+I+n+o+e+m,naval,method="lasso",trControl=train_control)
cv_lasso_ns8<-train(a~b+f+d+I+n+o+e+m+p,naval,method="lasso",trControl=train_control)
cv_lasso_ns9<-train(a~b+f+d+I+n+o+e+m+p+l,naval,method="lasso",trControl=train_control)
cv_lasso_ns10<-train(a~b+f+d+I+n+o+e+m+p+l+k,naval,method="lasso",trControl=train_control)
cv_lasso_ns11<-train(a~b+f+d+I+n+o+e+m+p+l+k+j,naval,method="lasso",trControl=train_control)
cv_lasso_ns12<-train(a~b+f+d+I+n+o+e+m+p+l+k+j+h,naval,method="lasso",trControl=train_control)
cv_lasso_ns13<-train(a~b+f+d+I+n+o+e+m+p+l+k+j+h+c,naval,method="lasso",trControl=train_control)
cv_lasso_ns14<-train(a~b+f+d+I+n+o+e+m+p+l+k+j+h+c+g,naval,method="lasso",trControl=train_control)

#Dataframes
frame_ns2 <- data.frame("r_sq" = double(0), "cv_r_sq" = double(0))
frame_ns2 <- add_row(frame_ns2, r_sq =max((lasso_mod1)$R2), cv_r_sq = mean(cv_lasso_ns1$resample$Rsquared))
frame_ns2 <- add_row(frame_ns2, r_sq =max((lasso_mod2)$R2), cv_r_sq = mean(cv_lasso_ns2$resample$Rsquared))
frame_ns2 <- add_row(frame_ns2, r_sq =max((lasso_mod3)$R2), cv_r_sq = mean(cv_lasso_ns3$resample$Rsquared))
frame_ns2 <- add_row(frame_ns2, r_sq =max((lasso_mod4)$R2), cv_r_sq = mean(cv_lasso_ns4$resample$Rsquared))
frame_ns2 <- add_row(frame_ns2, r_sq =max((lasso_mod5)$R2), cv_r_sq = mean(cv_lasso_ns5$resample$Rsquared))
frame_ns2 <- add_row(frame_ns2, r_sq =max((lasso_mod6)$R2), cv_r_sq = mean(cv_lasso_ns6$resample$Rsquared))
frame_ns2 <- add_row(frame_ns2, r_sq =max((lasso_mod7)$R2), cv_r_sq = mean(cv_lasso_ns7$resample$Rsquared))
frame_ns2 <- add_row(frame_ns2, r_sq =max((lasso_mod8)$R2), cv_r_sq = mean(cv_lasso_ns8$resample$Rsquared))
frame_ns2 <- add_row(frame_ns2, r_sq =max((lasso_mod9)$R2), cv_r_sq = mean(cv_lasso_ns9$resample$Rsquared))
frame_ns2 <- add_row(frame_ns2, r_sq =max((lasso_mod10)$R2), cv_r_sq = mean(cv_lasso_ns10$resample$Rsquared))
frame_ns2 <- add_row(frame_ns2, r_sq =max((lasso_mod11)$R2), cv_r_sq = mean(cv_lasso_ns11$resample$Rsquared))
frame_ns2 <- add_row(frame_ns2, r_sq =max((lasso_mod12)$R2), cv_r_sq = mean(cv_lasso_ns12$resample$Rsquared))
frame_ns2 <- add_row(frame_ns2, r_sq =max((lasso_mod13)$R2), cv_r_sq = mean(cv_lasso_ns13$resample$Rsquared))
frame_ns2 <- add_row(frame_ns2, r_sq =max((lasso_mod14)$R2), cv_r_sq = mean(cv_lasso_ns14$resample$Rsquared))

#Plotting The Values
plot(frame_ns2$r_sq,type="l",col="red",main = "LassoPLOT",ylab="Variation",ylim = c(0,1))
lines(frame_ns2$cv_r_sq,col="Blue")
legend(2,1,legend = c("r_square","cv_r_square"),col=c("red","blue"),lty = 1:2,cex = 0.8)

#Response Surface
#lm(formula = a ~ b + f + d + I + n + o + e + m + p + l + k + j + h + c, data = naval)
resp<-naval
rs_md1<-rsm(a~SO(b,f),data=resp)
rs_md2<-rsm(a~SO(b,f,d),data=resp)
rs_md3<-rsm(a~SO(b,f,d,I),data=resp)
rs_md4<-rsm(a~SO(b,f,d,I,n),data=resp)
rs_md5<-rsm(a~SO(b,f,d,I,n,o),data=resp)
rs_md6<-rsm(a~SO(b,f,d,I,n,o,e),data=resp)
rs_md7<-rsm(a~SO(b,f,d,I,n,o,e,m),data=resp)
rs_md8<-rsm(a~SO(b,f,d,I,n,o,e,m,p),data=resp)
rs_md9<-rsm(a~SO(b,f,d,I,n,o,e,m,p,l),data=resp)
rs_md10<-rsm(a~SO(b,f,d,I,n,o,e,m,p,l,k),data=resp)
rs_md11<-rsm(a~SO(b,f,d,I,n,o,e,m,p,l,k,j),data=resp)
rs_md12<-rsm(a~SO(b,f,d,I,n,o,e,m,p,l,k,j,h),data=resp)
rs_md13<-rsm(a~SO(b,f,d,I,n,o,e,m,p,l,k,j,h,c),data=resp)
rs_md14<-rsm(a~SO(b,f,d,I,n,o,e,m,p,l,k,j,h,c,g),data=resp)

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
frame_ns4 <- add_row(frame_ns4, adj_r_square = summary(rs_md9)$adj.r.squared, r_square = summary(rs_md9)$r.squared)
frame_ns4 <- add_row(frame_ns4, adj_r_square = summary(rs_md10)$adj.r.squared, r_square = summary(rs_md10)$r.squared)
frame_ns4 <- add_row(frame_ns4, adj_r_square = summary(rs_md11)$adj.r.squared, r_square = summary(rs_md11)$r.squared)
frame_ns4 <- add_row(frame_ns4, adj_r_square = summary(rs_md12)$adj.r.squared, r_square = summary(rs_md12)$r.squared)
frame_ns4 <- add_row(frame_ns4, adj_r_square = summary(rs_md13)$adj.r.squared, r_square = summary(rs_md13)$r.squared)
frame_ns4 <- add_row(frame_ns4, adj_r_square = summary(rs_md14)$adj.r.squared, r_square = summary(rs_md14)$r.squared)


#Plotting The Values
plot(frame_ns4$adj_r_square,type="l",col="red",main = "ReponseSurfacePLOT",ylab="Variation",ylim = c(0,1))
lines(frame_ns4$r_square,col="green")
legend(2,1,legend = c("adj_r_square","r_square"),col=c("red","green"),lty = 1:2,cex = 0.8)


#Quad Regression
#a ~ b + f + d + I + n + o + e + m + p + l + k + j + h + c
q_nas<-naval
q_nas$a1<-q_nas$a^2
q_nas$b1<-q_nas$b^2
q_nas$c1<-q_nas$c^2
q_nas$d1<-q_nas$d^2
q_nas$e1<-q_nas$e^2
q_nas$f1<-q_nas$f^2
q_nas$g1<-q_nas$g^2
q_nas$h1<-q_nas$h^2
q_nas$I1<-q_nas$I^2
q_nas$j1<-q_nas$j^2
q_nas$k1<-q_nas$k^2
q_nas$l1<-q_nas$l^2
q_nas$m1<-q_nas$m^2
q_nas$n1<-q_nas$n^2
q_nas$o1<-q_nas$o^2
q_nas$p1<-q_nas$p^2

#Linear Regression
Quad1 <- lm(a~b+b1,data=q_nas)
Quad2 <- lm(a~b+b1+f+f1,data=q_nas)
Quad3 <- lm(a~b+b1+f+f1+d+d1,data=q_nas)
Quad4 <- lm(a~b+b1+f+f1+d+d1+I+I1,data=q_nas)
Quad5 <- lm(a~b+b1+f+f1+d+d1+I+I1+n+n1,data=q_nas)
Quad6 <- lm(a~b+b1+f+f1+d+d1+I+I1+n+n1+o+o1,data=q_nas)
Quad7 <- lm(a~b+b1+f+f1+d+d1+I+I1+n+n1+o+o1+e+e1,data=q_nas)
Quad8 <- lm(a~b+b1+f+f1+d+d1+I+I1+n+n1+o+o1+e+e1+m+m1,data=q_nas)
Quad9 <- lm(a~b+b1+f+f1+d+d1+I+I1+n+n1+o+o1+e+e1+m+m1+p+p1,data=q_nas)
Quad10 <- lm(a~b+b1+f+f1+d+d1+I+I1+n+n1+o+o1+e+e1+m+m1+p+p1+l+l1,data=q_nas)
Quad11 <- lm(a~b+b1+f+f1+d+d1+I+I1+n+n1+o+o1+e+e1+m+m1+p+p1+l+l1+k+k1,data=q_nas)
Quad12 <- lm(a~b+b1+f+f1+d+d1+I+I1+n+n1+o+o1+e+e1+m+m1+p+p1+l+l1+k+k1+j+j1,data=q_nas)
Quad13 <- lm(a~b+b1+f+f1+d+d1+I+I1+n+n1+o+o1+e+e1+m+m1+p+p1+l+l1+k+k1+j+j1+h+h1,data=q_nas)
Quad14 <- lm(a~b+b1+f+f1+d+d1+I+I1+n+n1+o+o1+e+e1+m+m1+p+p1+l+l1+k+k1+j+j1+h+h1+c+c1,data=q_nas)
Quad15 <- lm(a~b+b1+f+f1+d+d1+I+I1+n+n1+o+o1+e+e1+m+m1+p+p1+l+l1+k+k1+j+j1+h+h1+c+c1+g+g1,data=q_nas)


#Cross=-Validation
#a ~ b + f + d + I + n + o + e + m + p + l + k + j + h + c
cv_quad_ns1 <- train(a ~ b+b1, data = q_nas, trControl = train_control, method = "lm")
cv_quad_ns2 <- train(a ~ b+b1+f+f1, data = q_nas, trControl = train_control, method = "lm")
cv_quad_ns3 <- train(a ~ b+b1+f+f1+d+d1, data = q_nas, trControl = train_control, method = "lm")
cv_quad_ns4 <- train(a ~ b+b1+f+f1+d+d1+I+I1, data = q_nas, trControl = train_control, method = "lm")
cv_quad_ns5 <- train(a ~ b+b1+f+f1+d+d1+I+I1+n+n1, data = q_nas, trControl = train_control, method = "lm")
cv_quad_ns6 <- train(a ~ b+b1+f+f1+d+d1+I+I1+n+n1+o+o1, data = q_nas, trControl = train_control, method = "lm")
cv_quad_ns7 <- train(a ~ b+b1+f+f1+d+d1+I+I1+n+n1+o+o1+e+e1, data = q_nas, trControl = train_control, method = "lm")
cv_quad_ns8 <- train(a ~ b+b1+f+f1+d+d1+I+I1+n+n1+o+o1+e+e1+m+m1, data = q_nas, trControl = train_control, method = "lm")
cv_quad_ns9 <- train(a ~ b+b1+f+f1+d+d1+I+I1+n+n1+o+o1+e+e1+m+m1+p+p1, data = q_nas, trControl = train_control, method = "lm")
cv_quad_ns10 <- train(a ~ b+b1+f+f1+d+d1+I+I1+n+n1+o+o1+e+e1+m+m1+p+p1+l+l1, data = q_nas, trControl = train_control, method = "lm")
cv_quad_ns11 <- train(a ~ b+b1+f+f1+d+d1+I+I1+n+n1+o+o1+e+e1+m+m1+p+p1+l+l1+k+k1, data = q_nas, trControl = train_control, method = "lm")
cv_quad_ns12 <- train(a ~ b+b1+f+f1+d+d1+I+I1+n+n1+o+o1+e+e1+m+m1+p+p1+l+l1+k+k1+j+j1, data = q_nas, trControl = train_control, method = "lm")
cv_quad_ns13 <- train(a ~ b+b1+f+f1+d+d1+I+I1+n+n1+o+o1+e+e1+m+m1+p+p1+l+l1+k+k1+j+j1+h+h1, data = q_nas, trControl = train_control, method = "lm")
cv_quad_ns14 <- train(a ~ b+b1+f+f1+d+d1+I+I1+n+n1+o+o1+e+e1+m+m1+p+p1+l+l1+k+k1+j+j1+h+h1+c+c1, data = q_nas, trControl = train_control, method = "lm")
cv_quad_ns15 <- train(a ~ b+b1+f+f1+d+d1+I+I1+n+n1+o+o1+e+e1+m+m1+p+p1+l+l1+k+k1+j+j1+h+h1+c+c1+g+g1, data = q_nas, trControl = train_control, method = "lm")


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
frame_ns3<- add_row(frame_ns3, r_sq = summary(Quad10)$r.squared, adj_r_sq = summary(Quad10)$adj.r.squared, cv_r_sq = mean(cv_quad_ns10$resample$Rsquared))
frame_ns3<- add_row(frame_ns3, r_sq = summary(Quad11)$r.squared, adj_r_sq = summary(Quad11)$adj.r.squared, cv_r_sq = mean(cv_quad_ns11$resample$Rsquared))
frame_ns3<- add_row(frame_ns3, r_sq = summary(Quad12)$r.squared, adj_r_sq = summary(Quad12)$adj.r.squared, cv_r_sq = mean(cv_quad_ns12$resample$Rsquared))
frame_ns3<- add_row(frame_ns3, r_sq = summary(Quad13)$r.squared, adj_r_sq = summary(Quad13)$adj.r.squared, cv_r_sq = mean(cv_quad_ns13$resample$Rsquared))
frame_ns3<- add_row(frame_ns3, r_sq = summary(Quad14)$r.squared, adj_r_sq = summary(Quad14)$adj.r.squared, cv_r_sq = mean(cv_quad_ns14$resample$Rsquared))
frame_ns3<- add_row(frame_ns3, r_sq = summary(Quad15)$r.squared, adj_r_sq = summary(Quad15)$adj.r.squared, cv_r_sq = mean(cv_quad_ns15$resample$Rsquared))

#Plotting The Value
plot(frame_ns3$adj_r_sq,type="l",col="red",main = "QuadPLOT",ylab="Variation",ylim = c(0,1))
lines(frame_ns3$r_sq,col="green")
lines(frame_ns3$cv_r_sq,col="Blue")
legend(2,1,legend = c("adj_r_square","r_square","cv_r_square"),col=c("red","green","blue"),lty = 1:2,cex = 0.8)

