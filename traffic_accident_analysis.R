# =====================================================================
# Traffic Accident Data Analysis Using R
# Name: Jignesh        Register No: 717825Z125
# Script for all slides of the presentation (run top to bottom)
# =====================================================================
# One-time package install (remove the # and run once if needed):
# install.packages(c("ggplot2", "caret", "pROC", "e1071", "car"))

# ---------------------------------------------------------------------
# STEP 0  Create the simulated dataset (traffic_accident_data.csv)
# Skip this step if you already have your own accident CSV file with
# the same column names.
# ---------------------------------------------------------------------
set.seed(2024)
n <- 3000
accident_id <- 1:n
accident_date <- as.character(as.Date("2022-01-01") + sample(0:1094, n, replace = TRUE))
hour <- round(pmin(23, pmax(0, ifelse(runif(n) < 0.5, rnorm(n, 9, 3), rnorm(n, 18, 3.5)))))
weather <- sample(c("Clear","Rain","Fog","Storm"), n, TRUE, prob = c(.6,.22,.12,.06))
road_type <- sample(c("Highway","Urban","Rural"), n, TRUE, prob = c(.25,.45,.30))
wet_w <- weather %in% c("Rain","Storm")
road_condition <- ifelse(wet_w,
  sample(c("Wet","Dry","Damaged"), n, TRUE, prob = c(.7,.1,.2)),
  sample(c("Dry","Wet","Damaged"), n, TRUE, prob = c(.8,.05,.15)))
light_condition <- ifelse(hour >= 7 & hour <= 17, "Daylight",
                   ifelse(hour %in% c(6,18,19), "Dusk", "Dark"))
vehicle_type <- sample(c("Two-wheeler","Car","Bus","Truck"), n, TRUE, prob = c(.4,.4,.08,.12))
driver_age <- round(pmin(80, pmax(18, rnorm(n, 38, 12))))
driver_gender <- sample(c("Male","Female"), n, TRUE, prob = c(.75,.25))
speed_limit <- ifelse(road_type == "Highway", sample(c(80,100), n, TRUE),
               ifelse(road_type == "Urban", sample(c(30,40,50), n, TRUE),
                      sample(c(50,60,80), n, TRUE)))
vehicle_speed <- round(pmin(140, pmax(10, speed_limit * runif(n, .6, 1.4) + rnorm(n, 0, 4))))
alcohol_involved <- sample(c("Yes","No"), n, TRUE, prob = c(.12,.88))
num_vehicles <- sample(1:4, n, TRUE, prob = c(.35,.45,.15,.05))
safety_gear_used <- sample(c("Yes","No"), n, TRUE, prob = c(.7,.3))
lin <- -3.9 + 0.035*vehicle_speed + 0.9*(alcohol_involved=="Yes") + 0.7*(safety_gear_used=="No") +
  0.45*(light_condition=="Dark") + 0.35*(road_condition=="Wet") + 0.3*(weather=="Storm") +
  0.012*(driver_age-38) + 0.3*(vehicle_type=="Two-wheeler")
high <- rbinom(n, 1, plogis(lin))
severity <- ifelse(high == 1, ifelse(runif(n) < 0.3, "Fatal", "Serious"), "Slight")
lambda <- exp(-1.4 + 0.014*vehicle_speed + 0.22*num_vehicles + 0.35*(alcohol_involved=="Yes") +
              0.2*(safety_gear_used=="No") + 0.15*(vehicle_type=="Bus")) + 0.9*high
num_casualties <- rpois(n, lambda)
d <- data.frame(accident_id, accident_date, hour, weather, road_type, road_condition,
  light_condition, vehicle_type, driver_age, driver_gender, speed_limit, vehicle_speed,
  alcohol_involved, num_vehicles, safety_gear_used, num_casualties, severity,
  stringsAsFactors = FALSE)
d$vehicle_speed[sample(n, 20)] <- NA
d$driver_age[sample(n, 15)] <- NA
d$weather[sample(n, 10)] <- NA
d$road_condition[sample(n, 8)] <- NA
d <- rbind(d, d[sample(n, 15), ])
d <- d[sample(nrow(d)), ]
write.csv(d, "traffic_accident_data.csv", row.names = FALSE)

# ---------------------------------------------------------------------
# Q1  Dataset: loading and anatomy (slide 4)
# ---------------------------------------------------------------------
library(ggplot2)
library(caret)
library(pROC)
library(e1071)

data <- read.csv(
  "traffic_accident_data.csv",
  stringsAsFactors = FALSE)
str(data)
dim(data)

# ---------------------------------------------------------------------
# Q2  Data structures in R (slide 6)
# ---------------------------------------------------------------------
speeds <- data$vehicle_speed[1:5]  # vector

num_matrix <- as.matrix(data[1:3,
  c("driver_age", "vehicle_speed",
    "num_casualties")])            # matrix

accident <- list(
  speed   = data$vehicle_speed[1],
  weather = data$weather[1],
  hours   = c(8, 14, 22))          # list

levels(factor(data$severity))      # factor
class(data); dim(data)             # data frame
speeds
num_matrix
str(accident)

# ---------------------------------------------------------------------
# Q2  Control structures (slide 7)
# ---------------------------------------------------------------------
speed <- 85
if (speed >= 80) {
  print("High speed")
} else if (speed >= 50) {
  print("Moderate speed")
} else {
  print("Low speed")
}

for (col in c("driver_age",
              "vehicle_speed")) {
  cat(col, mean(data[[col]], na.rm = TRUE), "\n")
}

count <- 0; i <- 1
while (count < 3) {
  if (data$severity[i] == "Fatal")
    count <- count + 1
  i <- i + 1
}
print(i - 1)

# ---------------------------------------------------------------------
# Q2  Functions (slide 7)
# ---------------------------------------------------------------------
speed_category <- function(s) {
  ifelse(s < 50, "Low",
  ifelse(s < 80, "Moderate", "High"))
}

rmse <- function(actual, pred) {
  sqrt(mean((actual - pred)^2))
}

data$speed_cat <-
  speed_category(data$vehicle_speed)
table(data$speed_cat)

# ---------------------------------------------------------------------
# Q2  Lists and data frames (slide 8)
# ---------------------------------------------------------------------
info <- list(rows = nrow(data),
             cols = ncol(data),
             col_names = names(data))
str(info)

fatal <- subset(data,
  severity == "Fatal")

aggregate(cbind(vehicle_speed,
  num_casualties) ~ severity,
  data = data, FUN = mean)

head(data[order(-data$num_casualties),
     c("accident_id",
       "num_casualties")], 5)

# ---------------------------------------------------------------------
# Q3  Stage 1: data cleaning (slide 9)
# ---------------------------------------------------------------------
colSums(is.na(data))
for (col in names(data)) {
  if (anyNA(data[[col]])) {
    if (is.numeric(data[[col]])) {
      data[[col]][is.na(data[[col]])] <-
        median(data[[col]], na.rm = TRUE)
    } else {
      m <- names(which.max(table(data[[col]])))
      data[[col]][is.na(data[[col]])] <- m
    }
  }
}
sum(duplicated(data))
data <- data[!duplicated(data), ]
sum(is.na(data)); dim(data)

# ---------------------------------------------------------------------
# Q3  Statistical problems (slide 10)
# ---------------------------------------------------------------------
table(data$severity)
prop.table(table(data$severity))

q <- quantile(data$vehicle_speed,
              c(.25, .75))
iqr <- q[2] - q[1]
sum(data$vehicle_speed > q[2] + 1.5 * iqr)

skewness(data$num_casualties)

cor(data$vehicle_speed,
    data$speed_limit)

# ---------------------------------------------------------------------
# Q3  Hypothesis tests (slide 11)
# ---------------------------------------------------------------------
data$high_severity <- factor(
  ifelse(data$severity %in%
    c("Serious", "Fatal"), "High", "Low"),
  levels = c("Low", "High"))

t.test(vehicle_speed ~ high_severity,
       data = data)

chisq.test(table(data$alcohol_involved,
                 data$high_severity))

cor.test(data$vehicle_speed,
         data$num_casualties)

# ---------------------------------------------------------------------
# Q3  Stage 2: data visualization (slide 12)
# ---------------------------------------------------------------------
ggplot(data, aes(x = high_severity,
                 fill = high_severity)) +
  geom_bar(width = 0.55) +
  geom_text(stat = "count",
    aes(label = after_stat(count)),
    vjust = -0.5) +
  scale_fill_manual(values = c(
    "Low" = "#2E86C1",
    "High" = "#C0392B")) +
  labs(title = "Accident Severity",
       x = "Severity",
       y = "Number of Accidents") +
  theme_minimal() +
  theme(legend.position = "none")

# ---------------------------------------------------------------------
# Q3  Stage 3: data preparation and splitting (slide 13)
# ---------------------------------------------------------------------
data$accident_id   <- NULL
data$accident_date <- NULL
char_cols <- sapply(data, is.character)
data[char_cols] <- lapply(
  data[char_cols], as.factor)

set.seed(123)
idx <- createDataPartition(
  data$high_severity, p = 0.8,
  list = FALSE)
train_data <- data[idx, ]
test_data  <- data[-idx, ]
dim(train_data); dim(test_data)
prop.table(table(train_data$high_severity))

# ---------------------------------------------------------------------
# Q4  Regression 1: simple linear regression (slide 14)
# ---------------------------------------------------------------------
model1 <- lm(num_casualties ~
               vehicle_speed,
             data = train_data)
summary(model1)

ggplot(train_data,
  aes(vehicle_speed, num_casualties)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm")

# ---------------------------------------------------------------------
# Q4  Regression 2 and 3: multiple and stepwise regression (slide 15)
# ---------------------------------------------------------------------
model2 <- lm(num_casualties ~
  vehicle_speed + speed_limit +
  driver_age + num_vehicles +
  alcohol_involved + weather +
  road_condition + safety_gear_used,
  data = train_data)
summary(model2)

library(car)
vif(model2)

model3 <- step(model2,
  direction = "both", trace = 0)
summary(model3)
AIC(model2, model3)

# ---------------------------------------------------------------------
# Q4  Regression 4: logistic regression (slide 16)
# ---------------------------------------------------------------------
model4 <- glm(high_severity ~
  vehicle_speed + driver_age +
  alcohol_involved + weather +
  road_condition + light_condition +
  safety_gear_used + vehicle_type,
  data = train_data,
  family = binomial)

summary(model4)
exp(coef(model4))

# ---------------------------------------------------------------------
# Q5  Prediction: regression models on test data (slide 17)
# ---------------------------------------------------------------------
p1 <- predict(model1, test_data)
p3 <- predict(model3, test_data)

r2 <- function(a, p)
  1 - sum((a - p)^2) /
      sum((a - mean(a))^2)

rmse(test_data$num_casualties, p1)
rmse(test_data$num_casualties, p3)
r2(test_data$num_casualties, p3)

# ---------------------------------------------------------------------
# Q5  Prediction: logistic model on test data (slide 17)
# ---------------------------------------------------------------------
prob <- predict(model4, test_data,
                type = "response")
pred <- factor(
  ifelse(prob > 0.5, "High", "Low"),
  levels = c("Low", "High"))

confusionMatrix(pred,
  test_data$high_severity,
  positive = "High")

auc(roc(test_data$high_severity, prob))

