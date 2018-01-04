##################################################################################
#                                                                                #
#    eLinguistics.net -> Regression model from calibation data                   #
#                        Processing a distances to age estimation                #
#                                                                                #
#    Vincent Beaufils, 15.12.2017                                                #
#                                                                                #
##################################################################################


setwd("c:/Forschung/")


#########################################################################
# Plot regression model & and estimate age of nodes

library(ggplot2)


Distances <- c(10.26,27.71,29.08,40.21,48.17,48.48,87.06,87.90)
Ages <- c(1400,2200,2200,3000,3000,3500,9000,9000)
dataLg <- as.data.frame(cbind(Ages,Distances))
Model <- lm(Ages ~ poly(Distances,2), data=dataLg)
#pred1 <- predict(Model, newdata=dataLgPredict,interval='confidence')
coef(Model)
resid(Model)

fit <- lm(Ages ~ poly(Distances,2), data=dataLg)
prd <- as.data.frame(Distances)
err <- predict(fit, newdata = prd, se.fit = TRUE)

prd$lci <- err$fit - 1.95 * err$se.fit
prd$fit <- err$fit
prd$uci <- err$fit + 1.95 * err$se.fit

ggplot(prd, aes(x = Distances, y = fit)) +
  theme_bw() +
  geom_line() +
  #geom_smooth() +
  geom_line(aes(y=prd$lci), color = "red", linetype = "dashed")+
  geom_line(aes(y=prd$uci), color = "red", linetype = "dashed")+
  geom_point(data = dataLg, aes(x = Distances, y = Ages))

######################################################
# Predict from new distance data
# Proto Germanic, PIE, Eurasiatic

Distances <- read.csv(file = "Distances.csv",header=F)$V1
dataLgPredict <- as.data.frame(Distances)
pred1 <- predict(Model, newdata=dataLgPredict,interval='confidence')
write.csv2(file="Dating.csv", pred1)
