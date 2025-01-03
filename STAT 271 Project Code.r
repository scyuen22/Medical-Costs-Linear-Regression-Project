# STAT 271 Project

# Read CSV file
med <- read.csv("C:/Users/sherw/OneDrive/Documents/UFV/Fall 2023/STAT 271 Intro to Data Analysis and Statistical Modeling/Project/medical_cost.csv")

attach(med)

# Develop full model with all explainer variables to predict charges
model.full <- lm(charges ~ age + sex + bmi + children + smoker + region, data = med)
summary(model.full)

library(olsrr)

ols_step_forward_p(model.full)
ols_step_backward_p(model.full)

# Backward Stepwise Regression
model.init <- step(model.full, direction = "backward")
summary(model.init)

# Best Subsets

ols_step_best_subset(model.full)

library(leaps)


AIC(model.full, model.init)
BIC(model.full, model.init)

# Diagnostic Plots

par(mfrow=c(2,2))
plot(model.init)	# Patterns in both plots

residual <- residuals(model.init)
fit <- fitted(model.init)
plot(fit, residual)

hist(residual)	# Histogram skewed

n <- length(charges)
order <- c(1:n)
plot(order, residual, xlab = "Observation Order", ylab = "Residuals", main = "y = Price") 
abline(0,0, col="red")

library(car)
vif(model.init)



# Possible transformations - log, log10, sqrt

tcharges <- log(charges)

model.trf <- lm(tcharges ~ age + sex + bmi + children + smoker + region, data = med)
summary(model.trf)

AIC(model.trf)
BIC(model.trf)
ols_step_best_subset(model.trf)

model.trf1 <- lm(tcharges ~ sqrt(age) + sex + sqrt(bmi) + sqrt(children) + smoker + region, data = med)
summary(model.trf1)

AIC(model.trf1)
BIC(model.trf1)
ols_step_best_subset(model.trf1)