# Part A. Background and data exploration

wine <- read.csv("C:/UC/Stat Modelling/Final Project/winequality-red.csv", 
                 sep = ";" )

# Check NA values and structure of the data
sum(is.na(wine))
str(wine)
summary(wine)

library(ggplot2)
library(gridExtra)
out1 <- ggplot(wine, aes(y= fixed.acidity)) + 
  geom_boxplot(outlier.colour="red",outlier.size=2)

out2 <- ggplot(wine, aes(y= volatile.acidity)) + 
  geom_boxplot(outlier.colour="red",outlier.size=2)

out3 <- ggplot(wine, aes(y= citric.acid)) + 
  geom_boxplot(outlier.colour="red",outlier.size=2)

out4 <- ggplot(wine, aes(y= residual.sugar)) + 
  geom_boxplot(outlier.colour="red",outlier.size=2)

out5 <- ggplot(wine, aes(y= chlorides)) + 
  geom_boxplot(outlier.colour="red",outlier.size=2)

out6 <- ggplot(wine, aes(y = free.sulfur.dioxide)) + 
  geom_boxplot(outlier.colour="red",outlier.size=2)

out7 <- ggplot(wine, aes(y= total.sulfur.dioxide)) + 
  geom_boxplot(outlier.colour="red",outlier.size=2)

out8 <- ggplot(wine, aes(y= density)) + 
  geom_boxplot(outlier.colour="red",outlier.size=2)

  
out9 <- ggplot(wine, aes(y= pH)) + 
    geom_boxplot(outlier.colour="red",outlier.size=2)
  
out10 <- ggplot(wine, aes(y= sulphates)) + 
  geom_boxplot(outlier.colour="red",outlier.size=2)

out11 <- ggplot(wine, aes(y= alcohol)) + 
  geom_boxplot(outlier.colour="red",outlier.size=2)

out12 <- ggplot(wine, aes(y= quality)) + 
  geom_boxplot(outlier.colour="red",outlier.size=2)

h1 <- grid.arrange(out1, out2, out3, out4, out5, out6, nrow = 2, ncol = 3)
h2 <- grid.arrange(out7, out8, out9, out10, out11, out12, nrow = 2, ncol = 3)

# Part B: Visualization and initial models for a binary response
# Q1 PartB 
wine$excellent <- ifelse(wine$quality >= 7, 1, 0)
wine$excellent <- as.factor(wine$excellent)
library(dplyr)
w <- wine$excellent
summary(w)

str(wine) 
ggplot(wine, aes(x = excellent)) + geom_bar()
pie2 <- data.frame(excellent = wine$excellent)
library("lessR")
PieChart(excellent, hole = 0, values = "%", data = wine, fill = c("darkblue", "lightblue"), main = "Distribution of Excellent Wine")

g1 <- ggplot(wine, aes(x = fixed.acidity, fill = (excellent))) +
  geom_bar(position="dodge")

g2 <- ggplot(wine, aes(x = volatile.acidity, fill = (excellent))) +
  geom_bar(position="dodge")

g3 <- ggplot(wine, aes(x = citric.acid, fill = (excellent))) +
  geom_bar(position="dodge")

g4 <- ggplot(wine, aes(x = residual.sugar, fill = (excellent))) +
  geom_bar(position="dodge")

g5 <- ggplot(wine, aes(x = chlorides, fill = (excellent))) +
  geom_bar(position="dodge")

g6 <- ggplot(wine, aes(x = free.sulfur.dioxide, fill = (excellent))) +
  geom_bar(position="dodge")

g7 <- ggplot(wine, aes(x = total.sulfur.dioxide, fill = (excellent))) +
  geom_bar(position="dodge")

g8 <- ggplot(wine, aes(x = density, fill = (excellent))) +
  geom_bar(position="dodge")

g9 <- ggplot(wine, aes(x = pH, fill = (excellent))) +
  geom_bar(position="dodge")

g10 <- ggplot(wine, aes(x = sulphates, fill = (excellent))) +
  geom_bar(position="dodge")

g11 <- ggplot(wine, aes(x = alcohol, fill = (excellent))) +
  geom_bar(position="dodge")

grid.arrange(g1, g2, g3, g4, g5, g6, ncol=3, nrow = 2)

grid.arrange(g7, g8, g9, g10, g11, ncol=3, nrow = 2)

# Fit a linear model on excellent
wine2 <- wine[,c(1:12)]
str(wine2)

c <- data.frame(cor(wine2))
round(c,2)

# Therefore there is no major collinearity between the numerical variables   

# fitting linear models
wine_lm <- wine
wine_lm$excellent <- as.numeric(wine_lm$excellent)
wine_lm <- wine_lm[-12]

lm_full <- lm(excellent ~ . , data = wine_lm)
summary(lm_full)  

lm_zero <- lm(excellent ~ 1, data = wine_lm)

lm_step <- step(lm_zero, scope = list(lower = lm_zero, upper = lm_full),
                direction = "both", k = log(nrow(wine_lm)))
lm_forward <- step(lm_zero, scope = list(lower = lm_zero, upper = lm_full),
                direction = "forward", k = log(nrow(wine_lm)))

# fitting logistic models
# dropping quality
wine_glm <- wine[-12]
str(wine_glm)
log_full <- glm(excellent ~ ., family = "binomial", data = wine_glm)
summary(log_full)

summary(lm_full)

# Part C
# Free sulphur dioxide and total sulphur dioxide are collinear
# therefore we have to drop one of the variable. 
# I will drop free sulphur dioxide, since it was not significant in the above steps
str(wine_glm)
wine_glm <- wine_glm[-6]
log_full2 <- glm(excellent ~ ., data = wine_glm, family = "binomial")
log_zero <- glm(excellent ~ 1, family = "binomial", data = wine_glm)
log_step_BIC <- step(log_zero, scope = list(lower = log_zero, upper = log_full2),
                direction = "both", k = log(nrow(wine_glm)))

log_step_AIC <- step(log_zero, scope = list(lower = log_zero, upper = log_full2),
                 direction = "forward", k = 2)
AIC(log_step_AIC)
BIC(log_step_BIC)
BIC(log_step_AIC)

# Chi Square test
log_chi <- drop1(log_full2 ,test = "Chi")

# Part C: 2
log_final <- glm(excellent ~ alcohol + volatile.acidity + sulphates + total.sulfur.dioxide + 
                   chlorides + fixed.acidity + residual.sugar + density, 
                 data = wine_glm, family = "binomial")
summary(log_final)

# Part C: 3
logpredprob <- predict(log_final,type ="response")
log_predprob_cut <- ifelse(logpredprob < 0.5, "no", "yes")
cut_dataframe <- data.frame(wine_glm, logpredprob, log_predprob_cut)
tab1 <- xtabs(~ excellent + log_predprob_cut, cut_dataframe)
tab1
specificity1 <- tab1[1,1]/(tab1[1,1] + tab1[1,2])
sensitivity1 <- tab1[2,2]/(tab1[2,1] + tab1[2,2])
specificity1
sensitivity1

#We can increase the cut-off probability to reduce FP
thresh <- seq(0.01, 0.5, 0.01)
sensitivity <- specificity <- rep(NA, length(thresh))
for(j in seq(along = thresh)) {
  pp <- ifelse(cut_dataframe$logpredprob < thresh[j], "no", "yes")
  xx <- xtabs(~ excellent + pp, cut_dataframe)
  specificity[j] <- xx[1,1]/(xx[1,1]+xx[1,2])
  sensitivity[j] <- xx[2,2]/(xx[2,1]+xx[2,2])
  }

par(mfrow = c(1,2))
matplot(thresh, cbind(sensitivity,specificity),type="l", 
        xlab ="Threshold", ylab ="Proportional", lty =1:2)
plot(1 - specificity, sensitivity, type="l", xlim = c(0,1), ylim = c(0,1)); abline(0,1, lty=2)
library(pROC)
auc(cut_dataframe$excellent, logpredprob)

# Prediction for the 1st and 268th bottle
pred_1st_bottle <- predict(log_final, wine_glm[1,], type = "link", se.fit = TRUE)
library(faraway)
p1 <- ilogit(pred_1st_bottle$fit)
p1_ci <- ilogit(c(pred_1st_bottle$fit - 1.96 * pred_1st_bottle$se.fit, 
                  pred_1st_bottle$fit + 1.96 * pred_1st_bottle$se.fit))
p1
p1_ci

pred_268_bottle <- predict(log_final, wine_glm[268,], type = "link", se.fit = TRUE)
p2 <- ilogit(pred_268_bottle$fit)
p2_ci <- ilogit(c(pred_268_bottle$fit - 1.96 * pred_268_bottle$se.fit, 
                  pred_268_bottle$fit + 1.96 * pred_268_bottle$se.fit))
p2
p2_ci

# Part D. Link functions and Dispersion Parameters
probit_final <- glm(excellent ~ alcohol + volatile.acidity + sulphates + 
      total.sulfur.dioxide + chlorides + fixed.acidity + residual.sugar + 
      density, family = binomial(link = probit), data = wine_glm)

clog_final <- glm(excellent ~ alcohol + volatile.acidity + sulphates + 
                      total.sulfur.dioxide + chlorides + fixed.acidity + residual.sugar + 
                      density, family = binomial(link = cloglog), data = wine_glm)
summary(log_final)
summary(probit_final)
summary(clog_final)
AIC(log_final)
AIC(probit_final)
AIC(clog_final)
BIC(log_final)
BIC(probit_final)
BIC(clog_final)

probitpredprob <- predict(probit_final,type ="response")
probit_predprob_cut <- ifelse(probitpredprob < 0.5, "no", "yes")
cut_dataframe <- data.frame(wine_glm, probitpredprob, probit_predprob_cut)
tab2 <- xtabs(~ excellent + probit_predprob_cut, cut_dataframe)
tab2
specificity2 <- tab2[1,1]/(tab2[1,1] + tab2[1,2])
sensitivity2 <- tab2[2,2]/(tab2[2,1] + tab2[2,2])
specificity2
sensitivity2

thresh <- seq(0.01, 0.5, 0.01)
sensitivity_probit <- specificity_probit <- rep(NA, length(thresh))
for(j in seq(along = thresh)) {
  pp_probit <- ifelse(cut_dataframe$probitpredprob < thresh[j], "no", "yes")
  xx_probit <- xtabs(~ excellent + pp_probit, cut_dataframe)
  specificity_probit[j] <- xx_probit[1,1]/(xx_probit[1,1]+xx_probit[1,2])
  sensitivity_probit[j] <- xx_probit[2,2]/(xx_probit[2,1]+xx_probit[2,2])
}

clogpredprob <- predict(clog_final,type ="response")
clog_predprob_cut <- ifelse(clogpredprob < 0.5, "no", "yes")
cut_dataframe <- data.frame(wine_glm, clogpredprob, clog_predprob_cut)
tab3 <- xtabs(~ excellent + clog_predprob_cut, cut_dataframe)
tab3
specificity3 <- tab3[1,1]/(tab3[1,1] + tab3[1,2])
sensitivity3 <- tab3[2,2]/(tab3[2,1] + tab3[2,2])
specificity3
sensitivity3

thresh <- seq(0.01, 0.5, 0.01)
sensitivity_clog <- specificity_clog <- rep(NA, length(thresh))
for(j in seq(along = thresh)) {
  pp_clog <- ifelse(cut_dataframe$clogpredprob < thresh[j], "no", "yes")
  xx_clog <- xtabs(~ excellent + pp_clog, cut_dataframe)
  specificity_clog[j] <- xx_clog[1,1]/(xx_clog[1,1]+xx_clog[1,2])
  sensitivity_clog[j] <- xx_clog[2,2]/(xx_clog[2,1]+xx_clog[2,2])
}

# Combined ROCs
par(mfrow = c(1,1))

plot(1 - specificity, sensitivity, type = 'l', col = 'red');
lines(1 - specificity_probit, sensitivity_probit, col = 'green')
lines(1 - specificity_clog, sensitivity_clog, col = 'blue')
legend(0.6, 0.7, legend = c("Logit", "Probit", "Cloglog"), 
       col = c("red", "green", "blue"), lty = 1:2, cex = 0.8)


# AUC of the curves
auc(cut_dataframe$excellent, logpredprob)
auc(cut_dataframe$excellent, probitpredprob)
auc(cut_dataframe$excellent, clogpredprob)

sigma.squared <- sum(residuals(probit_final, type = "pearson")^2)/(nrow(wine_glm) - 11)
summary(probit_final, dispersion = sigma.squared)
summary(probit_final)


# Part E
wine2 <- read.csv("C:/UC/Stat Modelling/Final Project/winequality-red.csv", 
                 sep = ";" )
str(wine2)
summary(wine2)
hist(wine2$quality)

quality <- as.factor(wine2$quality)
summary(quality)

library(VGAM)
k <- cor(wine2, method="kendall")
k[3]
k <- data.frame(k)
summary(k)
k$quality
rownames(k)
krank <- k[order(-k$quality),]
krank <- krank[12]

multi.model <- vglm(quality ~ alcohol + volatile.acidity + sulphates + 
                      total.sulfur.dioxide + chlorides + fixed.acidity + residual.sugar + 
                      density, data = wine2, family = cumulative(parallel = TRUE))
summary(multi.model)


# Part E 7.
first_bottle_multi <- wine2[1,]
class(first_bottle_multi)
library(VGAM)
first_bottle_multi <- predict(multi.model, newdata = wine2[1,], "response")
ifelse(sum(first_bottle_multi[,1:4]) > 0.5,"Not Excellent","Excellent")


twosixtyeight_bottle_multi <- predict(multi.model, wine2[268,],type ='response')
ifelse(sum(twosixtyeight_bottle_multi[,1:4]) > 0.5,"Not Excellent","Excellent")




