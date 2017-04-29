library(tidyverse)
library(corrplot)

df.full <- lm(RentRate ~ ., data = comm_prop)
summary(df.full)
cor(comm_prop)
corrplot(cor(comm_prop), method="ellipse")

#removed the 3 variables that don't seem to have much impact
dfLessMin <- lm(RentRate ~ Age + OperExp + W2MiDT, data = comm_prop)
summary(dfLessMin)

#Adjust for multicollinearity and add back in SqFt
df_model <- lm(RentRate ~ Age + OperExp + W2MiDT + SqFt, data = comm_prop)
summary(df_model)

ggplot(comm_prop, aes(x=W2MiDT, y=..count..)) +
  geom_bar()

#residual stuff
full.df <- fortify(df_model)
ggplot(full.df, aes(x=.fitted, y=.resid)) +
  geom_point() +
  geom_hline(yintercept=0, linetype=2) +
  labs(x="Fitted Values", y="Residuals")

# Check for normality
qqPlot(df_model$residuals, pch=16)
shapiro.test(df_model$residuals)
pairs(comm_prop[-c(4,6)], pch=16)

# Check for NCV
ncvTest(df_model)

# Check for multicollinearity
vif(df_model)

# Plot of the fit
ggplot(full.df, aes(x=.fitted, y=RentRate)) +
  geom_point(size=2) +
  geom_smooth(method="lm", color="red", lwd=1.5) +
  labs(x="Rent Predicted", y="Rent Actual")

new_RentRate <- data.frame(Age=9, OperExp=13, VacRate=0, SqFt=40, Taxes=540, W2MiDT='No')
predict(df_model, newdata=new_RentRate, interval="predict")

library(MASS)
model.full.int <- lm(RentRate ~ (Age + OperExp + VacRate + SqFt + W2MiDT)^2, data=comm_prop) 
# multply all variables by all variables
# becareful about more variables than obsevations
# 6 to 10 observations for each variable
summary(model.full.int)
ori_pred <- predict(model.full.int, newdata=new_RentRate, interval="predict")

model_all <- lm(RentRate ~ Age + OperExp + VacRate + SqFt + W2MiDT + Age:OperExp + Age:VacRate + Age:SqFt + Age:W2MiDT + OperExp:VacRate + OperExp:SqFt + OperExp:W2MiDT + VacRate:SqFt + VacRate:W2MiDT + SqFt:W2MiDT, data=comm_prop)
summary(model_all)

#remove VacRate:W2MiDT
model_1 <- lm(RentRate ~ Age + OperExp + VacRate + SqFt + W2MiDT + Age:OperExp + Age:VacRate + Age:SqFt + Age:W2MiDT + OperExp:VacRate + OperExp:SqFt + OperExp:W2MiDT + VacRate:SqFt + SqFt:W2MiDT, data=comm_prop)
summary(model_1)

#remove VacRate:SqFt
model_2 <- lm(RentRate ~ Age + OperExp + VacRate + SqFt + W2MiDT + Age:OperExp + Age:VacRate + Age:SqFt + Age:W2MiDT + OperExp:VacRate + OperExp:SqFt + OperExp:W2MiDT + SqFt:W2MiDT, data=comm_prop)
summary(model_2)

#remove OpenExp:SqFt
model_3 <- lm(RentRate ~ Age + OperExp + VacRate + SqFt + W2MiDT + Age:OperExp + Age:VacRate + Age:SqFt + Age:W2MiDT + OperExp:VacRate + OperExp:W2MiDT + SqFt:W2MiDT, data=comm_prop)
summary(model_3)

#remove Age:SqFt
model_4 <- lm(RentRate ~ Age + OperExp + VacRate + SqFt + W2MiDT + Age:OperExp + Age:VacRate + Age:W2MiDT + OperExp:VacRate + OperExp:W2MiDT + SqFt:W2MiDT, data=comm_prop)
summary(model_4)

#remove Age:W2MiDT
model_5 <- lm(RentRate ~ Age + OperExp + VacRate + SqFt + W2MiDT + Age:OperExp + Age:VacRate + OperExp:VacRate + OperExp:W2MiDT + SqFt:W2MiDT, data=comm_prop)
summary(model_5)

#remove OpenExp:VacRate
model_6 <- lm(RentRate ~ Age + OperExp + VacRate + SqFt + W2MiDT + Age:OperExp + Age:VacRate + OperExp:W2MiDT + SqFt:W2MiDT, data=comm_prop)
summary(model_6)

#remove Age:OpenExp
model_7 <- lm(RentRate ~ Age + OperExp + VacRate + SqFt + W2MiDT + Age:VacRate + OperExp:W2MiDT + SqFt:W2MiDT, data=comm_prop)
summary(model_7)

predict(model_7, newdata=new_RentRate, interval="predict")
