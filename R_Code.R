data2
str(data2)

#Objective1

#Creating construct variables

#a) Accuracy Variable 
#(Based on Recommendation_Relevance,Prediction_Accuracy,Less_Irrelevant_Recommendation,Behavior_Based_Recommendation_Improvement)
data2$Accuracy <- rowMeans(data2[, c(
  "Recommendation_Relevance",
  "Prediction_Accuracy",
  "Less_Irrelevant_Recommendation",
  "Behavior_Based_Recommendation_Improvement"
)], na.rm = TRUE)
head(data2$Accuracy)

#b) Privacy Variable
data2$Privacy <- rowMeans(data2[, c(
  "Personal_Data_Usage_Concern",
  "Privacy_Compromise_Happens",
  "Interaction_Hesitation_Privacy_Concern",
  "Data_Misuse_Concern",
  "Data_Sharing_Discomfort"
)], na.rm = TRUE)
head(data2$Privacy)

#c) Usefulness Variable
data2$Usefulness <- rowMeans(data2[, c(
  "Helps_Useful_Content_Discovery",
  "Suggestion_Time_Saving",
  "Overall_Experience_Improvement",
  "Help_Purchase_Decision",
  "Suggestion_Valuable_Informative"
)], na.rm = TRUE)
head(data2$Usefulness)

#d) Perception Variable
data2$Perception <- rowMeans(data2[, c(
  "Positive_Perception",
  "Perceived_Benefit",
  "Experience_Enhancement",
  "Overall_Perception_Favorable",
  "Recommendation_Reliability"
)], na.rm = TRUE)
head(data2$Perception)

#Multiple Linear Regression model

#H01: Accuracy does not influence perception
#H02: Privacy does not influence perception
#H03: Usefulness does not influence perception
model1 <- lm(Perception ~ Accuracy + Privacy + Usefulness, data = data1)

summary(model1)

#Heatmap Correlation analysis
cor_matrix <- cor(data2[,c("Accuracy","Privacy","Usefulness","Perception")])
install.packages("corrplot")
library(corrplot)
corrplot(cor_matrix, method="color", addCoef.col="black")

# Objective 2
table(data2$'Age Group')
table(data2$Gender)
table(data2$'Educational qualification')

#Age Group Analysis (One-way ANOVA)
anova_age <- aov(Perception ~ `Age Group`, data = data2)

summary(anova_age)

#Gender Analysis (t-test)
t.test(Perception ~ Gender, data = data2)

# Education Analyis (ANOVA)
anova_edu <- aov(Perception ~ `Educational qualification`, data = data2)

summary(anova_edu)

#Visualizations

boxplot(Perception ~ `Age Group`, data = data2,
        main = "Perception across Age Groups",
        xlab = "Age Group",
        ylab = "Perception Score")

boxplot(Perception ~ Gender, data = data2,
        main = "Perception by Gender",
        xlab = "Gender",
        ylab = "Perception Score")

boxplot(Perception ~ `Educational qualification`, data = data2,
        main = "Perception by Education",
        xlab = "Education",
        ylab = "Perception Score")

# Objective 3
data2$EaseOfUse <- rowMeans(data2[, c(
  "Easy_Interaction",
  "Easy_Exploration",
  "Easily_Understandable",
  "Effortless_Navigation",
  "Simple_Features"
)])
summary(data2$EaseOfUse)

model4 <- lm(Perception ~ EaseOfUse, data = data2)

summary(model4)

#Visualization
plot(data2$EaseOfUse, data2$Perception,
     main="Ease of Use vs Perception",
     xlab="Ease of Use",
     ylab="Perception")

abline(lm(Perception ~ EaseOfUse, data=data2), col="blue")

# Objective 4 (Regression)
# Creating construct variables

data2$Accuracy <- rowMeans(data2[, c(
  "Recommendation_Relevance",
  "Prediction_Accuracy",
  "Less_Irrelevant_Recommendation",
  "Behavior_Based_Recommendation_Improvement"
)])

data2$Privacy <- rowMeans(data2[, c(
  "Personal_Data_Usage_Concern",
  "Privacy_Compromise_Happens",
  "Interaction_Hesitation_Privacy_Concern",
  "Data_Misuse_Concern",
  "Data_Sharing_Discomfort"
)])

data2$Usefulness <- rowMeans(data2[, c(
  "Helps_Useful_Content_Discovery",
  "Suggestion_Time_Saving",
  "Overall_Experience_Improvement",
  "Help_Purchase_Decision",
  "Suggestion_Valuable_Informative"
)])

data2$Perception <- rowMeans(data2[, c(
  "Positive_Perception",
  "Perceived_Benefit",
  "Experience_Enhancement",
  "Overall_Perception_Favorable",
  "Recommendation_Reliability"
)])

data2$Behaviour <- rowMeans(data2[, c(
  "Click_Intention",
  "Engagement_Level",
  "Spent_More_Time_Recommendations",
  "Purchase_Intention",
  "Trust_to_Act"
)])
names(data2)

#multiple linear regression
model2 <- lm(Behaviour ~ Accuracy + Privacy + Usefulness + Perception, data = data2)

summary(model2)

#Visualization

plot(data2$Perception, data2$Behaviour,
     main="Perception vs Behaviour",
     xlab="Perception",
     ylab="Behaviour")

abline(lm(Behaviour ~ Perception, data=data2), col="red")

plot(data2$EaseOfUse, data2$Perception,
     main="Ease of Use vs Perception",
     xlab="Ease of Use",
     ylab="Perception")

abline(lm(Perception ~ EaseOfUse, data=data2), col="blue")
