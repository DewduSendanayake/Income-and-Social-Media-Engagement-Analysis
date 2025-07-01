# Group 53
# TPSM Mini Hackathon
# Topic 4: Relationship Between Income and Social Media Behavior


setwd("C:/Users/it22107596/Desktop/Group 53")

data <- read.csv("Time-Wasters on Social Media.csv")
str(data)

head(data)

colSums(is.na(data))

summary(data)


#------------------------- QUESTION 01 ------------------------------
# Do users with higher addiction level tend to have lower income?

# Linear regression

model_a <- lm(Income ~ Addiction.Level, data = data)
summary(model_a)

# Visualization

png("Addiction Level vs Income.png")
plot(data$Addiction.Level, data$Income,
     xlab = "Addiction level",
     ylab = "Income",
     main = "Income vs Addiction Level",
     pch = 16,
     cex = 0.6)
abline(model_a, col = "blue")
dev.off()

# Correlation Test

cor.test(data$Addiction.Level, data$Income)




#------------------------- QUESTION 02 ------------------------------
# Is session intensity negatively associated with income?

# Linear Regression

model_b <- lm(Income ~ Number.of.Sessions, data = data)
summary(model_b)

# Visualization

png("Session Intensity vs Income.png")
plot(data$Number.of.Sessions, data$Income,
     xlab = "Session Intensity",
     ylab = "Income",
     main = "Income vs Session Intensity",
     pch = 16,
     cex = 0.6)
abline(model_b, col = "red")
dev.off()

# Correlation Test
cor.test(data$Number.of.Sessions, data$Income)




#------------------------- QUESTION 03 ------------------------------
# Does income vary significantly by platform used?

# Visualization

png("Income by Platform.png")
boxplot(Income ~ Platform, data = data,
        xlab = "Platform",
        ylab = "Income",
        main = "Income Distribution by Platform",
        col = rainbow(length(unique(data$Platform))))
dev.off()

# ANOVA Test

model_c <- aov(Income ~ Platform, data = data)
summary(model_c)




#------------------------- QUESTION 04 ------------------------------
# Is there a difference in income levels between those with and without debt?

# Visualization

png("Income vs Debt Status.png")
boxplot(Income ~ Debt, data = data,
        main = "Income by Debt Status",
        xlab = "Debt Status",
        ylab = "Income",
        col = c("blue", "pink"),
        names = c("No Debt", "Has Debt"))
dev.off()

#T Test

t.test(Income ~ Debt, data = data)




#------------------------- QUESTION 05 ------------------------------
# Does owning property mediate the relationship between addiction and income?

## Addiction Level ➝ Income (direct effect)

# Visualization

png("Addiction vs Income.png")
plot(data$Addiction.Level, data$Income,
     xlab = "Addiction Level",
     ylab = "Income",
     main = "Income vs Addiction Level",
     pch = 16, cex = 0.6)
abline(lm(Income ~ Addiction.Level, data = data), col = "green")
dev.off()

# Correlation

cor.test(data$Addiction.Level, data$Income)

# Linear regression

model_d1 <- lm(Income ~ Addiction.Level, data = data)
summary(model_d1)


## Addiction Level ➝ Owns.Property (predictor ➝ mediator)

# Logistic regression

model_d2 <- glm(Owns.Property ~ Addiction.Level, data = data, family = binomial)
summary(model_d2)


## Owns.Property ➝ Income, while controlling for Addiction Level (mediator ➝ outcome)

# Mediation model

model_d3 <- lm(Income ~ Addiction.Level + Owns.Property, data = data)
summary(model_d3)


