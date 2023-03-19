library(tidyverse)
library(skimr)      
library(ggthemes)
library(patchwork)  
library(corrplot)

library(rsample)    
library(DescTools)  
library(sjPlot) 
library(lattice)
library(caret) 
library(ParamHelpers)
library(mlr)        
library(rpart)      
library(rpart.plot)
library(ranger)    
library(R6)
library(lightgbm)  
library(gbm)

df <- read.csv("C:\\Users\\zhaom\\OneDrive\\桌面\\Classification\\heart_failure.csv",stringsAsFactors = TRUE)
#df <- read_csv(str_c(ROOT, "C:\\Users\\zhaom\\OneDrive\\桌面\\Classification\\heart_failure.csv"))
factors = c("anaemia","diabetes","high_blood_pressure","sex","smoking","fatal_mi")
df[factors] = lapply(df[factors],factor)
palette_ro = c("#FFE4C4", "#CD5B45", "#7FFFD4", "#6495ED", "#FFB90F", "#CD3333", "#7FFF00")

head (df)
skim(df)

glimpse(df)

df_n <- df 
df <- df %>%
  mutate_at(factors,as.factor)
            
p1 <- ggplot(df, aes(x = anaemia, fill = fatal_mi)) +
  geom_bar(stat = "count", position = "stack", show.legend = FALSE) +
  scale_x_discrete(labels  = c("0 (False)", "1 (True)"))+
  scale_fill_manual(values = c(palette_ro[2], palette_ro[7]),
                    name = "fatal_mi",
                    labels = c("0 (False)", "1 (True)")) +
  labs(x = "Anaemia") +
  theme_minimal(base_size = 12) 


p2 <- ggplot(df, aes(x = diabetes, fill = fatal_mi)) +
  geom_bar(stat = "count", position = "stack", show.legend = FALSE) +
  scale_x_discrete(labels  = c("0 (False)", "1 (True)")) +
  scale_fill_manual(values = c(palette_ro[2], palette_ro[7]),
                    name = "fatal_mi",
                    labels = c("0 (False)", "1 (True)")) +
  labs(x = "Diabetes") +
  theme_minimal(base_size = 12) 


p3 <- ggplot(df, aes(x = high_blood_pressure, fill = fatal_mi)) +
  geom_bar(stat = "count", position = "stack", show.legend = FALSE) +
  scale_x_discrete(labels  = c("0 (False)", "1 (True)")) +
  scale_fill_manual(values = c(palette_ro[2], palette_ro[7]),
                    name = "fatal_mi",
                    labels = c("0 (False)", "1 (True)")) +
  labs(x = "High blood pressure") +
  theme_minimal(base_size = 12) 


p4 <- ggplot(df, aes(x = sex, fill = fatal_mi)) +
  geom_bar(stat = "count", position = "stack", show.legend = FALSE) +
  scale_x_discrete(labels  = c("0 (Female)", "1 (Male)")) +
  scale_fill_manual(values = c(palette_ro[2], palette_ro[7]),
                    name = "fatal_mi",
                    labels = c("0 (False)", "1 (True)")) +
  labs(x = "Sex") +
  theme_minimal(base_size = 12) 


p5 <- ggplot(df, aes(x = smoking, fill = fatal_mi)) +
  geom_bar(stat = "count", position = "stack", show.legend = FALSE) +
  scale_x_discrete(labels  = c("0 (False)", "1 (True)")) +
  scale_fill_manual(values = c(palette_ro[2], palette_ro[7]),
                    name = "fatal_mi",
                    labels = c("0 (False)", "1 (True)")) +
  labs(x = "Smoking") +
  theme_minimal(base_size = 12) 


p6 <- ggplot(df, aes(x = fatal_mi, fill = fatal_mi)) +
  geom_bar(stat = "count", position = "stack", show.legend = TRUE) +
  scale_x_discrete(labels  = c("0 (False)", "1 (True)")) +
  scale_fill_manual(values = c(palette_ro[2], palette_ro[7]),
                    name = "fatal_mi",
                    labels = c("0 (False)", "1 (True)")) +
  labs(x = "fatal_mi") +
  theme_minimal(base_size = 12) 


((p1 + p2 + p3) / (p4 + p5 + p6)) +
  plot_annotation(title = "Distribution of the binary features and fatal_mi")            

c1<- ggplot(df, aes(x=age))+ geom_histogram(binwidth=5, colour="white", fill="darkseagreen2", alpha=0.8)+
  geom_density(eval(bquote(aes(y=..count..*5))),colour=palette_ro[1], fill=palette_ro[1], alpha=0.3)+ scale_x_continuous(breaks=seq(40,100,10))+geom_vline(xintercept = 60, linetype="dashed")+ annotate("text", x=50, y=45, label="Age <60", size=2.5, color="dark green") + annotate("text", x=80, y=45, label="Age >= 60", size=2.5, color="dark red") +labs(title="Age Distribution") + theme_minimal(base_size = 8)


c2<- ggplot(df, aes(x=creatinine_phosphokinase))+ geom_histogram(binwidth=100, colour="white", fill="mediumpurple2", alpha=0.8)+
  geom_density(eval(bquote(aes(y=..count..*150))),colour=palette_ro[2], fill=palette_ro[2], alpha=0.3)+ scale_x_continuous(breaks=seq(0,10000,1000))+geom_vline(xintercept = 120, linetype="dashed")+ annotate("text", x=0, y=100, label="CPK Normal", size=2.5, color="dark green") + annotate("text", x=1000, y=80, label="CPK Abnormal", size=2.5, color="dark red")+labs(title="Creatinine Phosphokinase Distribution") + theme_minimal(base_size = 8)

c3<- ggplot(df, aes(x=ejection_fraction))+ geom_histogram(binwidth=5, colour="white", fill="lightpink1", alpha=0.8)+
  geom_density(eval(bquote(aes(y=..count..*5))),colour=palette_ro[3], fill=palette_ro[3], alpha=0.3)+ scale_x_continuous(breaks=seq(0,80,10))+geom_vline(xintercept = 40, linetype="dashed")+geom_vline(xintercept = 75, linetype="dashed")+ annotate("text", x=20, y=30, label="Abnormal", size=2.5, color="dark red") + annotate("text", x=50, y=30, label="Normal", color="dark green")+  annotate("text", x=80, y=30, label="Abnormal", size=2.5, color="dark red")+labs(title="Ejection Fraction Distribution") + theme_minimal(base_size = 8)

c4<- ggplot(df, aes(x=platelets))+ geom_histogram(binwidth=20000, colour="white", fill="lightskyblue2", alpha=0.8)+
  geom_density(eval(bquote(aes(y=..count..*25000))),colour=palette_ro[4], fill=palette_ro[4], alpha=0.3)+
  geom_vline(xintercept = 150000, linetype="dashed")+geom_vline(xintercept = 450000, linetype="dashed")+ annotate("text", x=100000, y=30, label="Abnormal", size=2.5, color="dark red") + annotate("text", x=300000, y=30, label="Normal", color="dark green")+  annotate("text", x=500000, y=30, label="Abnormal", size=2.5, color="dark red")+labs(title="Platelets Count") + theme_minimal(base_size = 8)

c5<- ggplot(df, aes(x=serum_sodium))+ geom_histogram(binwidth=1, colour="white", fill="lightsalmon", alpha=0.8)+
  geom_density(eval(bquote(aes(y=..count..))),colour=palette_ro[5], fill=palette_ro[5], alpha=0.3)+
  geom_vline(xintercept = 135, linetype="dashed")+geom_vline(xintercept = 145, linetype="dashed")+ annotate("text", x=130, y=20, label="Abnormal", size=2.5, color="dark red") + annotate("text", x=142, y=20, label="Normal", color="dark green")+  annotate("text", x=148, y=20, label="Abnormal", size=2.5, color="dark red")+labs(title="Serum Sodium") + theme_minimal(base_size = 8)

c6<- ggplot(df, aes(x=serum_creatinine))+ geom_histogram(binwidth=0.2, colour="white", fill="lightgoldenrod", alpha=0.8)+
  geom_density(eval(bquote(aes(y=..count..*0.2))),colour=palette_ro[6], fill=palette_ro[6], alpha=0.3)+
  geom_vline(xintercept = 0.74, linetype="dashed")+geom_vline(xintercept = 1.35, linetype="dashed")+ annotate("text", x=0.05, y=20, label="Abnormal", size=2.5, color="dark red") + annotate("text", x=1, y=20, label="Normal", color="dark green")+  annotate("text", x=2.5, y=20, label="Abnormal", size=2.5, color="dark red")+labs(title="Serum Creatinine") + theme_minimal(base_size = 8)

(c1+c2+c3)/(c4+c5+c6)
plot_annotation(title = "Distribution of the numeric features")


set.seed(0)
df_split <- initial_split(df,prop = 0.8, strata = fatal_mi)
train <- training(df_split)
test <- testing(df_split)
head(train)

set.seed(0)
df_n_split <- initial_split(df_n, prop = 0.8,strata = fatal_mi)
train_n <- training(df_n_split)
test_n <- testing(df_n_split)
head(train_n)

task_train <- makeClassifTask(data=train, target="fatal_mi")
task_test <- makeClassifTask(data=test, target="fatal_mi")
task_df <- makeClassifTask(data=df, target="fatal_mi")

set.seed(123)
lrn_svm <- makeLearner("classif.svm", predict.type = "prob",
                       par.vals = list(kernel="linear",
                                       cost=0.1))
svm <- train(lrn_svm, task_train)

plotLearnerPrediction(lrn_svm, task_df, 
                      features = c("serum_creatinine", "ejection_fraction")) 

pred <- getPredictionResponse(predict(svm, task_test))
confusionMatrix(pred, test$fatal_mi, positive = "1")

acc_svm <- confusionMatrix(pred, test$fatal_mi)$overall["Accuracy"]
tpr_svm <- confusionMatrix(pred, test$fatal_mi)$byClass["Specificity"]

set.seed(0)
cart <- rpart(fatal_mi ~ .,
              data = train, method = "class",
              control=rpart.control(minsplit=10,
                                    minbucket=5,
                                    maxdepth=10,
                                    cp=0.02))
prp(cart,
    type = 4,
    extra = 101,
    nn = TRUE,
    tweak = 1.0,
    space = 0.1,
    col = "black",
    split.col = palette_ro[1],
    branch.col = palette_ro[2],
    fallen.leaves = FALSE,
    roundint = FALSE,
    box.col = c(palette_ro[3], palette_ro[4])[cart$frame$yval])

pred <- as.factor(predict(cart, newdata=test)[, 2] >= 0.5) %>%
  fct_recode("0" = "FALSE", "1" = "TRUE")
confusionMatrix(pred, test$fatal_mi, positive = "1")

acc_cart <- confusionMatrix(pred, test$fatal_mi)$overall["Accuracy"]
tpr_cart <- confusionMatrix(pred, test$fatal_mi)$byClass["Specificity"]


set.seed(0)
rf <- ranger(fatal_mi ~.,
             data = train,
             mtry = 2, num.trees = 500, write.forest=TRUE, importance = "permutation")

data.frame(variables = names(importance(rf, method = "janitza")),
           feature_importance = importance(rf, method = "janitza")) %>%
  ggplot(aes(x = feature_importance,
             y = reorder(variables, X = feature_importance))) +
  geom_bar(stat = "identity",
           fill = palette_ro[6],
           alpha=0.9) +
  labs(y = "features", title = "Feature Importance of Random Forest") +
  theme_minimal(base_size = 12)

pred <- predict(rf, data=test)$predictions
confusionMatrix(pred, test$fatal_mi, positive = "1")

acc_rf <- confusionMatrix(pred, test$fatal_mi)$overall["Accuracy"]
tpr_rf <- confusionMatrix(pred, test$fatal_mi)$byClass["Specificity"]


set.seed(0)
rf <- ranger(fatal_mi ~ age + ejection_fraction + serum_creatinine + serum_sodium,
             data = train,
             mtry = 3, num.trees = 100, write.forest=TRUE, importance = "permutation")

data.frame(variables = names(importance(rf, method = "janitza")),
           feature_importance = importance(rf, method = "janitza")) %>%
  ggplot(aes(x = feature_importance,
             y = reorder(variables, X = feature_importance))) +
  geom_bar(stat = "identity",
           width = 0.5,
           just = 0.5,
           fill = palette_ro[6],
           alpha=0.9) +
  labs(y = "features", title = "Feature importance of random forest") +
  theme_minimal(base_size = 12)
pred <- predict(rf, data=test)$predictions
confusionMatrix(pred, test$fatal_mi, positive = "1")

data.frame(algorithm = c( "SVM", "decision\ntree", "random\nforest"),
           accuracy = c( acc_svm, acc_cart, acc_rf)*100,
           recall = c( tpr_svm, tpr_cart, tpr_rf)*100) %>%
  pivot_longer(col = -algorithm, names_to = "metrics", values_to = "percent") %>%
  ggplot(aes(x = reorder(algorithm, X = percent),
             y = percent,
             fill = metrics)) +
  geom_bar(stat = "identity",
           position = "dodge",
           alpha=0.9) +
  geom_text(aes(group = metrics, label = str_c(sprintf("%2.1f", percent), "%")), 
            position = position_dodge(width = 0.9), vjust = -0.2) +
  scale_fill_manual(values = c(palette_ro[3], palette_ro[7])) +
  labs(x = "algorithm", title = "Metrics of different classifier models") +
  theme_minimal(base_size = 12)

