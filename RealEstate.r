R-Project

DataSet:-RealEstate

#Forward Selection
 fwd_mod <- lm(price ~ 1, data = s)
 step(fwd_mod, direction = "forward", scope = formula(price ~ trans_date + age + dist + store + lat + long + price))

#LinearModel(On Every Value)

lm1<-lm(price~dist,data=s)
lm2<-lm(price~dist+store,data=s)
lm4<-lm(price~dist+store+age,data = s)
lm5<-lm(price~dist+store+age+lat,data = s)
lm6<-lm(price~dist+store+age+lat+trans_date,data = s)
lm7<-lm(price~dist+store+age+lat+trans_date+long,data = s)


#Cross-Validation
custom2<-trainControl(method="repeatedcv",number=10,verboseIter = T)

cv1<-train(price~dist,s,method='lm',trControl=custom2)
mean(cv1$resample$Rsquared)


cv2<-train(price~dist+store,s,method='lm',trControl=custom2)
mean(cv2$resample$Rsquared)

cv3<-train(price~dist+store+age,s,method='lm',trControl=custom2)
mean(cv3$resample$Rsquared)

cv4<-train(price~dist+store+age+lat,s,method='lm',trControl=custom2)
mean(cv4$resample$Rsquared)

cv5<-train(price~dist+store+age+lat+trans_date,s,method='lm',trControl=custom2)
mean(cv5$resample$Rsquared)

cv6<-train(price~dist+store+age+lat+trans_date+long,s,method='lm',trControl=custom2)
mean(cv6$resample$Rsquared)


#Storing All The Values In DataFrame:-
df_error<-data.frame("adj_r_square" = double(0), "r_square" = double(0), "cv_r_square" = double(0))
df_error <- add_row(df_error, adj_r_square = summary(lm)$adj.r.squared, r_square = summary(lm)$r.squared, cv_r_square = mean(cv1$resample$Rsquared))



#Plotting The Value
plot(df_error$adj_r_square,type="l",col="red",main = "LinearPLOT",ylab="Variation",ylim = c(0,1))
lines(df_error$r_square,col="green")
lines(df_error$cv_r_square,col="Blue")
legend(2,1,legend = c("adj_r_square","r_square","cv_r_square"),col=c("red","green","blue"),lty = 1:2,cex = 0.8)


------------------------------------------------------------------------------------

#Ridge Regression On RealEstate

ridge1<-lmridge(price~dist+store,s,K=c(0.1,0.001))
ridge2<-lmridge(price~dist+store+age,s,K=c(0.1,0.001))
ridge3<-lmridge(price~dist+store+age+lat,s,K=c(0.1,0.001))
ridge4<-lmridge(price~dist+store+age+lat+trans_date,s,K=c(0.1,0.001))
ridge5<-lmridge(price~dist+store+age+lat+trans_date+long,s,K=c(0.1,0.001))

#Cross Validation On Ridge


cv7<-train(price~dist+store,s,method='ridge',trControl=custom2)
cv8<-train(price~dist+store+age,s,method='ridge',trControl=custom2)
cv9<-train(price~dist+store+age+lat,s,method='ridge',trControl=custom2)
cv10<-train(price~dist+store+age+lat+trans_date,s,method='ridge',trControl=custom2)
cv11<-train(price~dist+store+age+lat+trans_date+long,s,method='ridge',trControl=custom2)


#Values In DataFrame

df_error<-data.frame("adj_r_square" = double(0), "r_square" = double(0), "cv_r_square" = double(0))
df_error <- add_row(df_error, adj_r_square = max(rstats1(ridge2)$adjR2), r_square =max(rstats1(ridge2)$R2), cv_r_square = mean(cv8$resample$Rsquared))
df_error <- add_row(df_error, adj_r_square = max(rstats1(ridge3)$adjR2), r_square =max(rstats1(ridge3)$R2), cv_r_square = mean(cv9$resample$Rsquared))
df_error <- add_row(df_error, adj_r_square = max(rstats1(ridge4)$adjR2), r_square =max(rstats1(ridge4)$R2), cv_r_square = mean(cv10$resample$Rsquared))
df_error <- add_row(df_error, adj_r_square = max(rstats1(ridge5)$adjR2), r_square =max(rstats1(ridge5)$R2), cv_r_square = mean(cv11$resample$Rsquared))

#Plotting The Points:-

plot(df_error$adj_r_square,type="l",col="red",main = "RidgePLOT",ylab="Variation",ylim = c(0,1))
lines(df_error$r_square,col="green")
lines(df_error$cv_r_square,col="Blue")
legend(2,1,legend = c("adj_r_square","r_square","cv_r_square"),col=c("red","green","blue"),lty = 1:2,cex = 0.8)
----------------------------------------------------------------------------------

#LassoRegression On RealEstate
x <- (s$dist)
x<-cbind(s$store,x)
lasso_mod1 <- lars(x, s$price, type = 'lasso')

x <- (s$dist)
x<-cbind(s$store,x)
x<-cbind(s$age,x)
lasso_mod2 <- lars(x, s$price, type = 'lasso')


x <- (s$dist)
x<-cbind(s$store,x)
x<-cbind(s$age,x)
x<-cbind(s$lat,x)
lasso_mod3<-lars(x,s$price,type='lasso')

x <- (s$dist)
x<-cbind(s$store,x)
x<-cbind(s$age,x)
x<-cbind(s$lat,x)
x<-cbind(s$trans_date,x)
lasso_mod4<-lars(x,s$price,type='lasso')

x<-(s$dist)
x<-cbind(s$store,x)
x<-cbind(s$age,x)
x<-cbind(s$lat,x)
x<-cbind(s$trans_date,x)
x<-cbind(s$long,x)
lasso_mod5<-lars(x,s$price,type='lasso')

#Cross Validation On Lasso:-
cv_lasso_model1<-train(price~dist+store,s,method="lasso",trControl=custom2)
cv_lasso_model2<-train(price~dist+store+age,s,method="lasso",trControl=custom2)
cv_lasso_model3<-train(price~dist+store+age+lat,s,method="lasso",trControl=custom2)
cv_lasso_model4<-train(price~dist+store+age+lat+trans_date,s,method="lasso",trControl=custom2)
cv_lasso_model5<-train(price~dist+store+age+lat+trans_date+long,s,method="lasso",trControl=custom2)

#Putting The Values in DataFrame:-
df_error <- add_row(df_error, r_square =max((lasso_mod1)$R2), cv_r_square = mean(cv_lasso_model1$resample$Rsquared))
df_error <- add_row(df_error, r_square =max((lasso_mod2)$R2), cv_r_square = mean(cv_lasso_model2$resample$Rsquared))
df_error <- add_row(df_error, r_square =max((lasso_mod3)$R2), cv_r_square = mean(cv_lasso_model3$resample$Rsquared))
df_error <- add_row(df_error, r_square =max((lasso_mod4)$R2), cv_r_square = mean(cv_lasso_model4$resample$Rsquared))
df_error <- add_row(df_error, r_square =max((lasso_mod5)$R2), cv_r_square = mean(cv_lasso_model5$resample$Rsquared))

#Plotting The Values.
plot(df_error$r_square,type="l",col="red",main = "LassoPLOT",ylab="Variation",ylim = c(0,1))
lines(df_error$cv_r_square,col="Blue")
legend(2,1,legend = c("r_square","cv_r_square"),col=c("red","blue"),lty = 1:2,cex = 0.8)

-----------------------------------------------------------------------------------------------------------------------


#QuadRegression On Real_Estate

q_d <- s  #Giving A New Name To The Object

q_d$dist_square <- q_d$dist^2
q_d$store_square<-q_d$store^2
q_d$age_square<-q_d$age^2
q_d$lat_square<-q_d$lat^2
q_d$trans_date_square<-q_d$trans_date^2
q_d$long_square<-q_d$long^2

#Regression Models

Quad1<-lm(price~dist+dist_square,data=q_d)
Quad2<-lm(price~dist+dist_square+store+store_square,data=q_d)
Quad3<-lm(price~dist+dist_square+store+store_square+age+age_square,data=q_d)
Quad4<-lm(price~dist+dist_square+store+store_square+age+age_square+lat+lat_square,data=q_d)
Quad5<-lm(price~dist+dist_square+store+store_square+age+age_square+lat+lat_square+trans_date+trans_date_square,data=q_d)
Quad6<-lm(price~dist+dist_square+store+store_square+age+age_square+lat+lat_square+trans_date+trans_date_square+long+long_square,data=q_d)




#Cross-Validation On Real_Estate For Quad Regression
cross_quad1<- train(price~dist+dist_square,q_d,method="lm",trControl=custom2)
cross_quad2<- train(price~dist+dist_square+store+store_square,q_d,method="lm",trControl=custom2)
cross_quad3<- train(price~dist+dist_square+store+store_square+age+age_square,q_d,method="lm",trControl=custom2)
cross_quad4<- train(price~dist+dist_square+store+store_square+age+age_square+lat+lat_square,q_d,method="lm",trControl=custom2)
cross_quad5<- train(price~dist+dist_square+store+store_square+age+age_square+lat+lat_square+trans_date+trans_date_square,q_d,method="lm",trControl=custom2)
cross_quad6<- train(price~dist+dist_square+store+store_square+age+age_square+lat+lat_square+trans_date+trans_date_square+long+long_square,q_d,method="lm",trControl=custom2)


#Data Frame For Quad-Regression:-
df_error <-data.frame("adj_r_square" = double(0), "r_square" = double(0), "cv_r_square" = double(0))
df_error <- add_row(df_error, adj_r_square = summary(Quad1)$adj.r.squared, r_square = summary(Quad1)$r.squared, cv_r_square = mean(cross_quad1$resample$Rsquared))
df_error <- add_row(df_error, adj_r_square = summary(Quad2)$adj.r.squared, r_square = summary(Quad2)$r.squared, cv_r_square = mean(cross_quad2$resample$Rsquared))
df_error <- add_row(df_error, adj_r_square = summary(Quad3)$adj.r.squared, r_square = summary(Quad3)$r.squared, cv_r_square = mean(cross_quad3$resample$Rsquared))
df_error <- add_row(df_error, adj_r_square = summary(Quad4)$adj.r.squared, r_square = summary(Quad4)$r.squared, cv_r_square = mean(cross_quad4$resample$Rsquared))
df_error <- add_row(df_error, adj_r_square = summary(Quad5)$adj.r.squared, r_square = summary(Quad5)$r.squared, cv_r_square = mean(cross_quad5$resample$Rsquared))
df_error <- add_row(df_error, adj_r_square = summary(quad_mod6)$adj.r.squared, r_square = summary(quad_mod6)$r.squared, cv_r_square = mean(cross_quad6$resample$Rsquared))



#Plotting The Points:-
plot(df_error$adj_r_square,type="l",col="red",main = "QuadPLOT",ylab="Variation",ylim = c(0,1))
lines(df_error$r_square,col="green")
lines(df_error$cv_r_square,col="Blue")
legend(2,1,legend = c("adj_r_square","r_square","cv_r_square"),col=c("red","green","blue"),lty = 1:2,cex = 0.8)
--------------------------------------------------------------------------------------------------

#Response Surface

r_df<-s

#Regression

rs_md1<-rsm(price~SO(dist,store),data=r_df)
rs_md2<-rsm(price~SO(dist,store,age),data=r_df)
rs_md3<-rsm(price~SO(dist,store,age,lat),data=r_df)
rs_md4<-rsm(price~SO(dist,store,age,lat,trans_date),data=r_df)
rs_md5<-rsm(price~SO(dist,store,age,lat,trans_date,long),data=r_df)


#DataFrame

df_error<-data.frame("adj_r_square" = double(0), "r_square" = double(0))

df_error <- add_row(df_error, adj_r_square = summary(rs_md1)$adj.r.squared, r_square = summary(rs_md1)$r.squared)
df_error <- add_row(df_error, adj_r_square = summary(rs_md2)$adj.r.squared, r_square = summary(rs_md2)$r.squared)
df_error <- add_row(df_error, adj_r_square = summary(rs_md3)$adj.r.squared, r_square = summary(rs_md3)$r.squared)
df_error <- add_row(df_error, adj_r_square = summary(rs_md4)$adj.r.squared, r_square = summary(rs_md4)$r.squared)
df_error <- add_row(df_error, adj_r_square = summary(rs_md5)$adj.r.squared, r_square = summary(rs_md5)$r.squared)


#Plotting Values:-


plot(df_error$adj_r_square,type="l",col="red",main = "ReponseSurfacePLOT",ylab="Variation",ylim = c(0,1))
lines(df_error$r_square,col="green")
legend(2,1,legend = c("adj_r_square","r_square"),col=c("red","green"),lty = 1:2,cex = 0.8)