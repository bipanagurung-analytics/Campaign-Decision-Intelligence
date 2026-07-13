# Data preprocessing (Using Chapter 3)
# Dataset: VoterPersuasion.csv
rm(list = ls())
set.seed(123)

# Importing CSV Data
voter <- read.csv("VoterPersuasion.csv")

# Quick look
dim(voter)
head(voter, 5)

# Exploring Structure of Data
str(voter)

# Exploring Numeric Variables (summary)
summary(voter)

# I saw X is just row index and VOTER_ID is only an identifier, so removing them 
voter$X <- NULL
voter$VOTER_ID <- NULL

# I saw character columns, so converting to factor for later models
char_vars <- names(voter)[sapply(voter, is.character)]
for (v in char_vars) { voter[[v]] <- factor(voter[[v]])}

# Basic target check
table(voter$MOVED_A)
prop.table(table(voter$MOVED_A))

# Checking missing Data 
# Missing values by column
na_by_col <- colSums(is.na(voter))
na_by_col[na_by_col > 0]

# Missing values by rows
sum(rowSums(is.na(voter)) > 0)

# Duplicate checks 
# If VOTER_ID exists, check duplicates
if ("VOTER_ID" %in% names(voter)) {
  sum(duplicated(voter$VOTER_ID))}


# Separating numeric and categorical variables
num_cols <- names(voter)[sapply(voter, is.numeric)]
cat_cols <- names(voter)[sapply(voter, function(x) is.character(x) || is.factor(x))]

# viewing results
length(num_cols)
length(cat_cols)

# Central Tendency (Mean/Median) example on AGE
if ("AGE" %in% names(voter)) {
  mean(voter$AGE, na.rm = TRUE)
  median(voter$AGE, na.rm = TRUE)
  summary(voter$AGE)}

# Spread: Quantiles + Five-number summary + IQR
if ("AGE" %in% names(voter)) {
  quantile(voter$AGE, na.rm = TRUE)
  IQR(voter$AGE, na.rm = TRUE)
  quantile(voter$AGE, probs = c(0.2, 0.6), na.rm = TRUE)}


# Setting plots colors  
BOX_COLS <- c("No" = "#A9D6E5",   
              "Yes" = "#F4A261") 

HIST_FILL <- "#A9D6E5"
HIST_EDGE <- "#1B4965"

GAIN_LINE   <- "#2E86AB"   
BASELINE    <- "#9CA3AF"   
LIFT_BAR    <- "#F4A261"   
LABEL_COLOR <- "#1F2937"

par(col.main = "#1F2937",
    col.lab  = "#1F2937",
    col.axis = "#1F2937")

# Boxplot for AGE (numeric distribution/outliers) 
if ("AGE" %in% names(voter)) {
  boxplot(voter$AGE, main = "Boxplot: AGE", ylab = "Age",
          col = HIST_FILL, border = HIST_EDGE)}

# Example: ploting a few numeric propensity variables if present 
prop_vars <- c("E_PELIG", "G_PELIG", "PP_PELIG", "AP_PELIG")
prop_vars <- prop_vars[prop_vars %in% names(voter)]
for (v in prop_vars) {
  boxplot(voter[[v]], main = paste("Boxplot:", v), ylab = v,
          col = HIST_FILL, border = HIST_EDGE)}

# Histograms (numeric distributions) 
if ("AGE" %in% names(voter)) {
  hist(voter$AGE, main = "Histogram: AGE", xlab = "Age",
       col = HIST_FILL, border = HIST_EDGE)}

for (v in prop_vars) {
  hist(voter[[v]], main = paste("Histogram:", v), xlab = v,
       col = HIST_FILL, border = HIST_EDGE)}

# Exploring Categorical Variables (counts/proportions)
# Example: PARTY variables are binary so using tables
bin_vars <- c("PARTY_D", "PARTY_I", "PARTY_R", "GENDER_F", "GENDER_M", "KIDS")
bin_vars <- bin_vars[bin_vars %in% names(voter)]

# I saw KIDS is NOT binary (values like 3–31), so keeping only true binary indicators
bin_vars <- c("PARTY_D", "PARTY_I", "PARTY_R", "GENDER_F", "GENDER_M")
bin_vars <- bin_vars[bin_vars %in% names(voter)]

for (v in bin_vars) {
  print(v)
  print(table(voter[[v]]))
  print(prop.table(table(voter[[v]])))}

# I found that KIDS will be treated as numeric, so summarizing it as numeric 
if ("KIDS" %in% names(voter)) {
  summary(voter$KIDS)
  boxplot(voter$KIDS, main = "Boxplot: KIDS", ylab = "KIDS",
          col = HIST_FILL, border = HIST_EDGE)
  hist(voter$KIDS, main = "Histogram: KIDS", xlab = "KIDS",
       col = HIST_FILL, border = HIST_EDGE)}

# Party flag consistency check
if (all(c("PARTY_D", "PARTY_I", "PARTY_R") %in% names(voter))) {
  table(voter$PARTY_D + voter$PARTY_I + voter$PARTY_R)}

# I discovered OPP_SEX has invalid values and MOVED_AD/opposite cause data leakage so removing them
voter$OPP_SEX <- NULL
voter$opposite <- NULL
voter$MOVED_AD <- NULL

# Exploring Relationships Between Variables
# Example 1: categorical/binary predictor vs outcome
if ("PARTY_I" %in% names(voter)) {
  tab <- table(voter$MOVED_A, voter$PARTY_I)
  print(tab)
  print(prop.table(tab, margin = 2))}

# Example 2: numeric predictor vs outcome (comparing distributions) 
if ("AGE" %in% names(voter)) {
  boxplot(AGE ~ MOVED_A, data = voter, main = "AGE by MOVED_A",
          xlab = "MOVED_A", ylab = "AGE",
          col = BOX_COLS, border = HIST_EDGE)}

# Example 3: message framing vs outcome
msg_vars <- c("MESSAGE_A", "MESSAGE_A_REV")
msg_vars <- msg_vars[msg_vars %in% names(voter)]
for (v in msg_vars) {
  tab <- table(voter$MOVED_A, voter[[v]])
  print(v)
  print(tab)
  print(prop.table(tab, margin = 2))}

# Predictor relevance
# A) Numeric predictors: comparing distributions by MOVED_A 
num_check <- c("AGE","MED_HH_INC","REG_DAYS","E_PELIG","G_PELIG","PP_PELIG","AP_PELIG")
num_check <- num_check[num_check %in% names(voter)]

for (v in num_check) {
  print(v)
  print(tapply(voter[[v]], voter$MOVED_A, summary))
  boxplot(voter[[v]] ~ voter$MOVED_A, main = paste(v, "by MOVED_A"),
          xlab = "MOVED_A", ylab = v,
          col = BOX_COLS, border = HIST_EDGE)}

# B) Binary predictors: persuasion rate (MOVED_A=1) inside each group
bin_check <- c("PARTY_D","PARTY_I","PARTY_R","GENDER_F","GENDER_M","MESSAGE_A","MESSAGE_A_REV")
bin_check <- bin_check[bin_check %in% names(voter)]

for (v in bin_check) {
  tab <- table(voter$MOVED_A, voter[[v]])
  print(v)
  print(tab)
  print(prop.table(tab, margin = 2))           
  print(prop.table(tab, margin = 2)["1", ])    }

# C) Lifestyle interest variables (my business goal #2) 
life_vars <- c("BOOKBUYERI","FAMILYMAGA","RELIGIOUSC","POLITICALC","HEALTHFITN","FINANCIALM","DOITYOURSE","CULINARYIN","GARDENINGM")
life_vars <- life_vars[life_vars %in% names(voter)]

for (v in life_vars) {
  print(v)
  print(tapply(voter[[v]], voter$MOVED_A, summary))
  boxplot(voter[[v]] ~ voter$MOVED_A, main = paste(v, "by MOVED_A"),
          xlab = "MOVED_A", ylab = v,
          col = BOX_COLS, border = HIST_EDGE)}

# Data Partitioning 
set.seed(123)

# Splitting the data into training and testing sets 70/30
n <- nrow(voter)
train_index <- sample(1:n, size = 0.70 * n)
train <- voter[train_index, ]
test  <- voter[-train_index, ]

# Checking sizes
dim(train)
dim(test)

# Checking outcome distribution to ensure both datasets remain comparable 
table(train$MOVED_A)
prop.table(table(train$MOVED_A))

table(test$MOVED_A)
prop.table(table(test$MOVED_A))

# Verifying that training and testing dataset contain identical predictors
names(train)
names(test)

# FINAL SAFETY CLEANUP (Before Modeling)
# Removing non-numeric predictors 
non_numeric <- names(train)[!sapply(train, is.numeric)]
non_numeric <- setdiff(non_numeric, "MOVED_A")

if (length(non_numeric) > 0) {
  train <- train[, !(names(train) %in% non_numeric)]
  test  <- test[,  !(names(test)  %in% non_numeric)]}

# Removing near-zero variance predictors 
library(caret)

nzv <- nearZeroVar(train)
if (length(nzv) > 0) {
  train <- train[, -nzv]
  test  <- test[,  -nzv]}


# Model 1 — Logistic Regression (full)
library(gains)
library(ggplot2)
library(gridExtra)

# A) FIXING WARNING (Rank-deficient / singularities)
# (1) Removing one from each perfect pair
if ("GENDER_M" %in% names(train)) train$GENDER_M <- NULL
if ("GENDER_M" %in% names(test))  test$GENDER_M  <- NULL

if ("MESSAGE_A_REV" %in% names(train)) train$MESSAGE_A_REV <- NULL
if ("MESSAGE_A_REV" %in% names(test))  test$MESSAGE_A_REV  <- NULL

# (2) Removing NL variables that cause perfect collinearity 
drop_nl <- c("NL5G", "NL3PR", "NL5AP", "NL2PP")
drop_nl <- drop_nl[drop_nl %in% names(train)]
if (length(drop_nl) > 0) {
  train[, drop_nl] <- NULL
  test[,  drop_nl] <- NULL}

# (3) Removing other factor/character predictors (keeping MOVED_A)
non_numeric <- names(train)[!sapply(train, is.numeric)]
non_numeric <- setdiff(non_numeric, "MOVED_A")
if (length(non_numeric) > 0) {
  train[, non_numeric] <- NULL
  test[, non_numeric]  <- NULL}

# B) Converting target ONLY at modeling time
train$MOVED_A <- factor(train$MOVED_A, levels = c(0,1), labels = c("No","Yes"))
test$MOVED_A  <- factor(test$MOVED_A,  levels = c(0,1), labels = c("No","Yes"))

# C) Cross-validation control
# Why ROC:
# - business need is targeting so ranking matters
trainControl_logit <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary)

# D) Fitting Logistic Regression model
logit_model <- train(
  MOVED_A ~ .,
  data = train,
  method = "glm",
  family = "binomial",
  trControl = trainControl_logit,
  metric = "ROC")

# Viewing model summary
summary(logit_model$finalModel)

# E) Predictions on test set
logit_pred_prob  <- predict(logit_model, test, type = "prob")  
logit_pred_class <- predict(logit_model, test)                 

# F) Confusion matrix
confusionMatrix(logit_pred_class, test$MOVED_A, positive = "Yes")

# G) Cumulative Gains Chart  
# Why Gains:
# - "If I contact top X% highest probability voters,
#   how many persuaded voters do I capture?"

# Converting outcome to numeric (1 = Yes, 0 = No)
actual <- ifelse(test$MOVED_A == "Yes", 1, 0)

# Dividing ranked predictions into deciles
gain_all <- gains(
  actual,
  logit_pred_prob$Yes,
  groups = 10 )

# Total number of actual YES voters
nactual <- sum(actual)

# Visualizing model’s cumulative capture of YES voters & random targeting baseline
g1 <- ggplot() +
  geom_line(aes(x = gain_all$cume.obs,
                y = gain_all$cume.pct.of.total * nactual),
            color = GAIN_LINE, linewidth = 1.2) +
  geom_line(aes(x = c(0, max(gain_all$cume.obs)),
                y = c(0, nactual)),
            color = BASELINE, linetype = "dashed") +
  labs(
    x = "# Cases (sorted by predicted probability)",
    y = "Cumulative # of actual YES",
    title = "Cumulative Gains Chart — Logistic Regression") +
  theme_minimal()


# H) Decile-wise Lift Chart 
# Why Lift:
# - compares targeting vs random selection

# Creating decile-level lift statistics
gain10 <- gains(
  actual,
  logit_pred_prob$Yes,
  groups = 10)

lift_df <- data.frame(
  Decile = 1:10,
  Depth = gain10$depth,       # Percentage of population in each decile
  Lift = gain10$lift / 100)   # Lift expressed as “times better than random”

# Visual comparison of lift across deciles
g2 <- ggplot(lift_df, aes(x = factor(Decile), y = Lift)) +
  geom_col(fill = LIFT_BAR, width = 0.7) +
  geom_text(aes(label = round(Lift, 2)),
            vjust = -0.3, size = 3, color = LABEL_COLOR) +
  labs(
    x = "Deciles (1 = highest probability group)",
    y = "Lift vs random",
    title = "Decile-wise Lift Chart — Logistic Regression"
  ) +
  ylim(0, max(lift_df$Lift) + 0.5) +
  theme_minimal()

# Side-by-side view of gains and lift
grid.arrange(g1, g2, ncol = 2)

# Model 2 — Reduced Logistic Regression (Reduced Predictor Set)
# Why reduced?
# - Cleaner model but still answers:
#   Q1) Who to target / exclude (probabilities + lift)
#   Q2) Which message works (MESSAGE_A effect + segment patterns later)

# If MOVED_A is still 0/1, converting to factor (needed for ROC + confusionMatrix)
if (is.numeric(train$MOVED_A)) {
  train$MOVED_A <- factor(train$MOVED_A, levels = c(0,1), labels = c("No","Yes"))
  test$MOVED_A  <- factor(test$MOVED_A,  levels = c(0,1), labels = c("No","Yes"))}

# Picking fewer predictors 
# - MESSAGE_A supports business Q2
# - Party/vote history/eligibility supports business Q1
keep_vars <- c(
  "MOVED_A",
  "MESSAGE_A",
  "PARTY_D","PARTY_I","PARTY_R",
  "VPP_08","VPR_08","VPR_10","VPR_12","VG_08","VG_10","VG_12",
  "AGE","MED_AGE","KIDS","MED_HH_INC","REG_DAYS","ED_4COL","MEDIANEDUC",
  "HH_ND","HH_NR","HH_NI",
  "E_PELIG","G_PELIG","PP_PELIG","AP_PELIG",
  "UPSCALEFEM","BOOKBUYERI","RELIGIOUSC","POLITICALC",
  "SET_NO")

# Keeping only variables that exist
keep_vars <- keep_vars[keep_vars %in% names(train)]

train_red <- train[, keep_vars]
test_red  <- test[,  keep_vars]

# Removing near-zero variance predictors
nzv2 <- nearZeroVar(train_red)
if (length(nzv2) > 0) {
  train_red <- train_red[, -nzv2]
  test_red  <- test_red[,  -nzv2]}

# FIXING for warning: "glm.fit: fitted probabilities numerically 0 or 1 occurred"
# Why it happens:
# - Some predictors (usually binary flags) almost perfectly predict MOVED_A
# - so detecting "separation" variables (0 cell in a 2x2 table) and dropping them
y_num <- ifelse(train_red$MOVED_A == "Yes", 1, 0)

cand <- setdiff(names(train_red), "MOVED_A")

# checking only binary predictors (0/1)
binary_cand <- cand[sapply(train_red[, cand, drop = FALSE], function(x) {
  ux <- unique(x)
  ux <- ux[!is.na(ux)]
  all(ux %in% c(0,1))})]

sep_vars <- c()
for (v in binary_cand) {
  tab <- table(y_num, train_red[[v]])
  # if any cell is zero -> separation risk
  if (any(tab == 0)) sep_vars <- c(sep_vars, v)}

# dropping only if found
if (length(sep_vars) > 0) {
  train_red <- train_red[, !(names(train_red) %in% sep_vars)]
  test_red  <- test_red[,  !(names(test_red)  %in% sep_vars)]}

# Cross-validation setup 

trControl2 <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary)

# Fitting reduced logistic regression
model2_logit <- train(
  MOVED_A ~ .,
  data = train_red,
  method = "glm",
  family = "binomial",
  trControl = trControl2,
  metric = "ROC")

# Viewing model summary
summary(model2_logit$finalModel)

# Evaluating on test set
pred2_prob  <- predict(model2_logit, test_red, type = "prob")
pred2_class <- predict(model2_logit, test_red)

# Confusion matrix
confusionMatrix(pred2_class, test_red$MOVED_A, positive = "Yes")

# Gains + Lift  
# Why: tells top X% captures how many YES
library(gains)
library(ggplot2)
library(gridExtra)

# Converting actual class labels to 0/1 for gains()
actual2 <- ifelse(test_red$MOVED_A == "Yes", 1, 0)

# Using to get cumulative gains across all ranked cases
gain2_all <- gains(actual2, pred2_prob$Yes, groups = length(actual2))

# To know total YES needed to convert % gains into counts
nactual2 <- sum(actual2)

# visualizing how fast YES cases are captured as we move down the ranked list
g1_m2 <- ggplot() +
  geom_line(aes(x = gain2_all$cume.obs,
                y = gain2_all$cume.pct.of.total * nactual2),
            color = GAIN_LINE, linewidth = 1.2) +
  geom_line(aes(x = c(0, max(gain2_all$cume.obs)),
                y = c(0, nactual2)),
            color = BASELINE, linetype = "dashed") +
  labs(x = "# Cases (sorted by predicted probability)",
       y = "Cumulative # of actual YES",
       title = "Cumulative Gains — Model 2 (Reduced Logistic)") +
  theme_minimal()

# splitting predictions into deciles to compare against random targeting
gain2_10 <- gains(actual2, pred2_prob$Yes, groups = 10)

# Why: lift shows how much better each decile performs than random
lift2_df <- data.frame(
  Decile = 1:10,
  Lift = gain2_10$lift / 100)

# Using it cause bar chart makes decile-level lift easy to interpret
g2_m2 <- ggplot(lift2_df, aes(x = factor(Decile), y = Lift)) +
  geom_col(fill = LIFT_BAR, width = 0.7) +
  geom_text(aes(label = round(Lift, 2)),
            vjust = -0.3, size = 3, color = LABEL_COLOR) +
  labs(x = "Deciles (1 = highest probability group)",
       y = "Lift vs random",
       title = "Decile Lift — Model 2 (Reduced Logistic)") +
  ylim(0, max(lift2_df$Lift) + 0.5) +
  theme_minimal()

# viewing gains and lift together for a complete ranking evaluation
grid.arrange(g1_m2, g2_m2, ncol = 2)

# Model 3 — Decision Tree (Classification) for MOVED_A
# Goal: predict who will be "Yes" (moved/persuaded) so I can target better

library(rpart)
library(dplyr)
set.seed(123)

# Using reduced dataset from Model 2 if it exists, else using full train/test
if (exists("train_red") && exists("test_red")) {
  train_tree <- train_red
  test_tree  <- test_red
} else {
  train_tree <- train
  test_tree  <- test}

# MOVED_A must be factor No/Yes (needed for ROC + confusionMatrix)

if (is.numeric(train_tree$MOVED_A)) {
  train_tree$MOVED_A <- factor(train_tree$MOVED_A, levels = c(0,1), labels = c("No","Yes"))
  test_tree$MOVED_A  <- factor(test_tree$MOVED_A,  levels = c(0,1), labels = c("No","Yes"))
} else {
  train_tree$MOVED_A <- factor(train_tree$MOVED_A, levels = c("No","Yes"))
  test_tree$MOVED_A  <- factor(test_tree$MOVED_A,  levels = c("No","Yes"))}

# 1) CV setup (ROC because I need to care about ranking/targeting)
trControl3 <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary)

# 2) Fitting classification tree 

grid_cp <- expand.grid(cp = seq(0.0005, 0.02, by = 0.001))

model3_tree <- train(
  MOVED_A ~ .,
  data = train_tree,
  method = "rpart",
  metric = "ROC",
  trControl = trControl3,
  tuneGrid = grid_cp,
  parms = list(split = "gini"))

# Best cp
model3_tree$bestTune
print(model3_tree)

# Model 3: Decision Tree plot
library(rpart.plot)

rpart.plot(
  model3_tree$finalModel,
  type = 2,
  extra = 104,
  fallen.leaves = TRUE,
  main = "Model 3: Decision Tree for MOVED_A")


# Variable importance 
varImp(model3_tree)

# 3) Evaluating on test set
pred3_prob  <- predict(model3_tree, test_tree, type = "prob")
pred3_class <- predict(model3_tree, test_tree)

confusionMatrix(pred3_class, test_tree$MOVED_A, positive = "Yes")

# 4) Gains + Lift  
# Why: decision tree gives few distinct probabilities (leaf-based),
# so gains() may complain. I create deciles by rank (10 groups).
scored <- data.frame(
  actual = ifelse(test_tree$MOVED_A == "Yes", 1, 0),
  p_yes  = pred3_prob$Yes)

# Deciles (1 = highest probability group)
scored$decile <- ntile(desc(scored$p_yes), 10)

# Lift table by decile
base_rate <- mean(scored$actual)
lift_tbl <- aggregate(actual ~ decile, data = scored, mean)
lift_tbl$lift <- lift_tbl$actual / base_rate

# Cumulative gains (sorted by predicted probability)
scored_sorted <- scored[order(-scored$p_yes), ]
scored_sorted$cume_cases <- 1:nrow(scored_sorted)
scored_sorted$cume_yes   <- cumsum(scored_sorted$actual)

# Total number of actual YES & cases
n_yes <- sum(scored_sorted$actual)
n_all <- nrow(scored_sorted)

# Gains curve shows how quickly YES cases are captured as we move down the ranked list
g1_m3 <- ggplot(scored_sorted, aes(x = cume_cases, y = cume_yes)) +
  geom_line(color = GAIN_LINE, linewidth = 1.2) +
  geom_abline(intercept = 0, slope = n_yes / n_all,
              linetype = "dashed", color = BASELINE) +
  labs(
    x = "# Cases (sorted by predicted probability)",
    y = "Cumulative # of actual YES",
    title = "Cumulative Gains — Model 3 (Decision Tree)") +
  theme_minimal()

# Lift comparison across deciles
g2_m3 <- ggplot(lift_tbl, aes(x = factor(decile), y = lift)) +
  geom_col(fill = LIFT_BAR, width = 0.7) +
  geom_text(aes(label = round(lift, 2)),
            vjust = -0.3, size = 3, color = LABEL_COLOR) +
  labs(
    x = "Deciles (1 = highest probability group)",
    y = "Lift vs random",
    title = "Decile Lift — Model 3 (Decision Tree)"
  ) +
  ylim(0, max(lift_tbl$lift) + 0.5) +
  theme_minimal()

# Displaying gains and lift together for model ranking evaluation
grid.arrange(g1_m3, g2_m3, ncol = 2)


# Comparison Table for Models 1–3 using Accuracy, AUC, and Lift (top 10%)
library(pROC)

# Model 1 (Logistic Full)
# Evaluating full model using confusion matrix, AUC, and lift
prob1   <- logit_pred_prob$Yes
pred1   <- logit_pred_class
actual1 <- ifelse(test$MOVED_A == "Yes", 1, 0)

cm1  <- confusionMatrix(pred1, test$MOVED_A, positive = "Yes")
auc1 <- as.numeric(pROC::auc(pROC::roc(test$MOVED_A, prob1, levels = c("No","Yes"), quiet = TRUE)))
lift1 <- gains(actual1, prob1, groups = 10)$lift[1] / 100


# Model 2 (Logistic Reduced)
# Same evaluation as Model 1 
prob2   <- pred2_prob$Yes
pred2   <- pred2_class
actual2 <- ifelse(test_red$MOVED_A == "Yes", 1, 0)

cm2  <- confusionMatrix(pred2, test_red$MOVED_A, positive = "Yes")
auc2 <- as.numeric(pROC::auc(pROC::roc(test_red$MOVED_A, prob2, levels = c("No","Yes"), quiet = TRUE)))
lift2 <- gains(actual2, prob2, groups = 10)$lift[1] / 100


# Model 3 (Decision Tree)
# Evaluated using the same metrics as logistic models
prob3   <- pred3_prob$Yes
pred3   <- pred3_class
actual3 <- ifelse(test_tree$MOVED_A == "Yes", 1, 0)

cm3  <- confusionMatrix(pred3, test_tree$MOVED_A, positive = "Yes")
auc3 <- as.numeric(pROC::auc(pROC::roc(test_tree$MOVED_A, prob3, levels = c("No","Yes"), quiet = TRUE)))
lift3 <- lift_tbl$lift[lift_tbl$decile == 1]

# Summary table for all models
compare_tbl <- data.frame(
  Model = c("Model 1: Logistic (Full)",
            "Model 2: Logistic (Reduced)",
            "Model 3: Decision Tree"),
  Accuracy = c(as.numeric(cm1$overall["Accuracy"]),
               as.numeric(cm2$overall["Accuracy"]),
               as.numeric(cm3$overall["Accuracy"])),
  Sensitivity_Yes = c(as.numeric(cm1$byClass["Sensitivity"]),
                      as.numeric(cm2$byClass["Sensitivity"]),
                      as.numeric(cm3$byClass["Sensitivity"])),
  Specificity_No = c(as.numeric(cm1$byClass["Specificity"]),
                     as.numeric(cm2$byClass["Specificity"]),
                     as.numeric(cm3$byClass["Specificity"])),
  AUC = c(auc1, auc2, auc3),
  Lift_Top10pct = c(lift1, lift2, lift3))

# Display comparison results
print(compare_tbl)

# AUC comparison plot
library(ggplot2)

auc_df <- data.frame(
  Model = c("Logistic (Full)", "Logistic (Reduced)", "Decision Tree"),
  AUC = c(auc1, auc2, auc3))

ggplot(auc_df, aes(x = Model, y = AUC, fill = Model)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = round(AUC, 3)), vjust = -0.4, size = 4) +
  ylim(0, 1) +
  labs(
    title = "Model Comparison Using AUC",
    y = "AUC",
    x = ""
  ) +
  theme_minimal() +
  theme(legend.position = "none")
