# Change this to your working directory containing the competition data
setwd("C:/Users/jeff.keller/Desktop/RFDilemmaZone")

# packages needed (run the line below to install)
# install.packages(c("randomForest", "ggplot2", "reshape2", "scales", "data.table))
library(randomForest)
library(ggplot2)
library(reshape2)
library(scales)
library(data.table)

# Load the data
load(file = "driving_data.rdata")
head(data)

# Summarize the data
summary(data)


### Calculate a response variable
data$outcome <- 0

# Stopped in time or went through in time
e <- data[, "Stop.Dist"] >= 0
e[is.na(e)] <- FALSE
data[, "outcome"][e | (is.na(data[, "Stop.Time"]) & data[, "Stopline.Time"] < data[, "Event2.Time"])] <- "safe"

# Ran the light
e <- data[, "Stop.Dist"] < 0
e[is.na(e)] <- FALSE
data[, "outcome"][e | (is.na(data[, "Stop.Time"]) & data[, "Stopline.Time"] >= data[, "Event2.Time"])] <- "unsafe"

data[, "outcome"] <- as.factor(data[, "outcome"])
table(data[, "outcome"])
prop.table(table(data[, "outcome"]))


### Calculate some additional variables

# If accel.dir is na, it means they didn't move their foot
data[, "Accel.Dir"] <- as.character(data[, "Accel.Dir"])
data[, "Accel.Dir"][is.na(data[, "Accel.Dir"])] <- "None"
data[, "Accel.Dir"] <- factor(data[, "Accel.Dir"])

data[, "Accel.Min"][is.na(data[, "Accel.Min"])] <- 0
data[, "Accel.Max"][is.na(data[, "Accel.Max"])] <- 0

# Structure of the data
str(data)


### Estimate a random forest model

# Setting a random seed ensures a reproducable model
set.seed(1987)

# Run the model with default settings
rf.all <- randomForest(formula = outcome ~ Event1.Vel + Event1.Dist +
                       Event2.Vel + Age + Device + Treatment + Gender + Accel.Min + Accel.Max,
                       data = data,
                       na.action = na.fail,
                       importance = TRUE,
                       do.trace = 50)

# Training Data Miss-classification Rate
y <- data[, "outcome"]
y_hat <- predict(object = rf.all, newdata = data)
table(y, y_hat) # 0%! (Over-fitting)

# Model summary statistics and plots
err.rate <- melt(rf.all$err.rate) # convert to long format
names(err.rate) <- c("ntree", "Measure", "rate")
levels(err.rate[, "Measure"]) <- c("Overall", "Safe", "Unsafe")

# OOB Error Rates (Figure 1)
ggplot(err.rate, aes(x = ntree, y = rate)) + theme_bw() +
  geom_line(aes(group = Measure, color = Measure)) +
  xlab("Number of Trees") + ylab("OOB Error Rate") + ggtitle("Out of Bag Error Rates") +
  scale_y_continuous(labels = percent, lim = c(0, 1))



### K-fold cross validation to tune the mtry parameter
k <- 10  # 10 folds
p <- 1:9 # there are 9 indicators, so we could try up to 9 at each node split
folds <- sample(1:k, nrow(data), replace = TRUE)
results <- matrix(0, nrow = k * length(p), ncol = 6)
colnames(results) <- c("m", "fold", "CV False Positive", "CV False Negative", "CV Total Error Rate", "Total Error Rate")
results[,1] <- rep(1:9, each = k)
results[,2] <- 1:k
for (m in p) {
  for (i in c(1:k)) {
    include <- !folds == i
    rf <- randomForest(formula = outcome ~ Event1.Vel + Event1.Dist +
                         Event2.Vel + Age + Device + Treatment + Gender + Accel.Min + Accel.Max,
                       data = data,
                       na.action = na.fail,
                       subset = include,
                       mtry = m)
    
    pred <- predict(object = rf, newdata = data[!include,])
    confusion <- table(actual = data[!include, "outcome"], predicted = pred)
    
    results[(m-1)*k+i, 3] <- confusion[1,2] / sum(confusion[1,])
    results[(m-1)*k+i, 4] <- confusion[2,1] / sum(confusion[2,])
    results[(m-1)*k+i, 5] <- sum(confusion[c(2,3)]) / sum(confusion)
    results[(m-1)*k+i, 6] <- 1 - sum(diag(rf[["confusion"]][, -3]))/sum(rf[["confusion"]][, -3])
  }
}

results <- data.table(results)
plot(results[, mean(`Total Error Rate`), by = m], type = "l", ylim = c(0.15, 0.19),
     ylab = "Classification Error Rate")
lines(results[, mean(`CV Total Error Rate`), by = m], col = "blue")
legend(x = 6, y = 0.19, legend = c("OOB", "Cross-Validation"), col = c("black", "blue"), lty = c(1, 1), box.col = "white")



### Rerun the main model with the tuned mtry parameter
rf.all <- randomForest(formula = outcome ~ Event1.Vel + Event1.Dist +
                         Event2.Vel + Age + Device + Treatment + Gender + Accel.Min + Accel.Max,
                       data = data,
                       na.action = na.fail,
                       importance = TRUE,
                       do.trace = 50,
                       mtry = 5)


# Confusion matrix (Table 2)
rf.all[["confusion"]]


# Variable Importance (Figure 2)
varImpPlot(rf.all)


### Sensitivity analysis (Table 3)
# A single baseline record (a "median" record)
newentry <- list()
newentry$Event1.Vel <- 40
newentry$Event1.Dist <- 200
newentry$Event2.Vel <- 10
newentry$Accel.Min <- -18
newentry$Accel.Max <- 14
newentry$Age <- factor("M", levels = c("Y", "M", "O"))
newentry$Gender <- factor("M", levels = c("M", "F"))
newentry$Treatment <- factor("B", levels = c("B", "O", "I"))
newentry$Device <- factor("HH", levels = c("HF", "HH", "HS"))
newentry <- data.frame(newentry)
newentry

alpha <- 0.05

# baseline
y_hat <- predict(object = rf.all, newdata = newentry, type = "prob")
p_hat <- y_hat[,2]
p_hat
n <- rf.all$ntree
moe <- sqrt(p_hat * (1 - p_hat) / n) * qnorm(1 - alpha / 2)
moe


# -5 mph Event 1
newentry$Event1.Vel <- 35
y_hat <- predict(object = rf.all, newdata = newentry, type = "prob")
p_hat <- y_hat[,2]
p_hat
n <- rf.all$ntree
moe <- sqrt(p_hat * (1 - p_hat) / n) * qnorm(1 - alpha / 2)
moe

# +5 mph Event 1
newentry$Event1.Vel <- 45
y_hat <- predict(object = rf.all, newdata = newentry, type = "prob")
p_hat <- y_hat[,2]
p_hat
n <- rf.all$ntree
moe <- sqrt(p_hat * (1 - p_hat) / n) * qnorm(1 - alpha / 2)
moe

# -8 mph Event 2
newentry$Event1.Vel <- 40
newentry$Event2.Vel <- 2
y_hat <- predict(object = rf.all, newdata = newentry, type = "prob")
p_hat <- y_hat[,2]
p_hat
n <- rf.all$ntree
moe <- sqrt(p_hat * (1 - p_hat) / n) * qnorm(1 - alpha / 2)
moe

# -4 mph Event 2
newentry$Event2.Vel <- 6
y_hat <- predict(object = rf.all, newdata = newentry, type = "prob")
p_hat <- y_hat[,2]
p_hat
n <- rf.all$ntree
moe <- sqrt(p_hat * (1 - p_hat) / n) * qnorm(1 - alpha / 2)
moe

# Young
newentry$Event2.Vel <- 10
newentry$Age <- factor("Y", levels = c("Y", "M", "O"))
y_hat <- predict(object = rf.all, newdata = newentry, type = "prob")
p_hat <- y_hat[,2]
p_hat
n <- rf.all$ntree
moe <- sqrt(p_hat * (1 - p_hat) / n) * qnorm(1 - alpha / 2)
moe

# Old
newentry$Age <- factor("O", levels = c("Y", "M", "O"))
y_hat <- predict(object = rf.all, newdata = newentry, type = "prob")
p_hat <- y_hat[,2]
p_hat
n <- rf.all$ntree
moe <- sqrt(p_hat * (1 - p_hat) / n) * qnorm(1 - alpha / 2)
moe

# Female
newentry$Age <- factor("M", levels = c("Y", "M", "O"))
newentry$Gender <- factor("F", levels = c("M", "F"))
y_hat <- predict(object = rf.all, newdata = newentry, type = "prob")
p_hat <- y_hat[,2]
p_hat
n <- rf.all$ntree
moe <- sqrt(p_hat * (1 - p_hat) / n) * qnorm(1 - alpha / 2)
moe

# Outgoing Call
newentry$Gender <- factor("M", levels = c("M", "F"))
newentry$Treatment <- factor("O", levels = c("B", "O", "I"))
y_hat <- predict(object = rf.all, newdata = newentry, type = "prob")
p_hat <- y_hat[,2]
p_hat
n <- rf.all$ntree
moe <- sqrt(p_hat * (1 - p_hat) / n) * qnorm(1 - alpha / 2)
moe

# Incoming Call
newentry$Treatment <- factor("I", levels = c("B", "O", "I"))
y_hat <- predict(object = rf.all, newdata = newentry, type = "prob")
p_hat <- y_hat[,2]
p_hat
n <- rf.all$ntree
moe <- sqrt(p_hat * (1 - p_hat) / n) * qnorm(1 - alpha / 2)
moe

# Headset Device + Outgoing Call
newentry$Treatment <- factor("O", levels = c("B", "O", "I"))
newentry$Device <- factor("HS", levels = c("HF", "HH", "HS"))
y_hat <- predict(object = rf.all, newdata = newentry, type = "prob")
p_hat <- y_hat[,2]
p_hat
n <- rf.all$ntree
moe <- sqrt(p_hat * (1 - p_hat) / n) * qnorm(1 - alpha / 2)
moe

# Handheld Device
newentry$Device <- factor("HH", levels = c("HF", "HH", "HS"))
y_hat <- predict(object = rf.all, newdata = newentry, type = "prob")
p_hat <- y_hat[,2]
p_hat
n <- rf.all$ntree
moe <- sqrt(p_hat * (1 - p_hat) / n) * qnorm(1 - alpha / 2)
moe

# Hands-Free Device
newentry$Device <- factor("HF", levels = c("HF", "HH", "HS"))
y_hat <- predict(object = rf.all, newdata = newentry, type = "prob")
p_hat <- y_hat[,2]
p_hat
n <- rf.all$ntree
moe <- sqrt(p_hat * (1 - p_hat) / n) * qnorm(1 - alpha / 2)
moe
