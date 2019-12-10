library(party)
library(caret)
library(C50)
library(tree)
library(gmodels)
library(knitr)
library(png)

fraudcheck = read.csv(file.choose())
View(fraudcheck)

#---------EDA-----------------

summary(fraudcheck)
table(fraudcheck$Taxable.Income)
str(fraudcheck)
hist(fraudcheck$Taxable.Income)
scale(fraudcheck$Taxable.Income)

risky_good = ifelse(fraudcheck$Taxable.Income <=30000, "risky","good")
FC = data.frame(fraudcheck,risky_good)

#splitting

FC_Train = FC[1:300,]

FC_Test = FC[301:600,]

#using party function for representation

png(file = "decision_tree".png)
opall_tree = ctree(risky_good ~ Undergrad + Marital.Status + City.Population + Work.Experience + Urban, data = FC)
summary(opall_tree)

plot(opall_tree)

#using training data 

png(file= "decision_tree.png")
op_tree = ctree(risky_good ~ Undergrad + Marital.Status + City.Population + Work.Experience + Urban , data = FC_Train)
summary(op_tree)
plot(op_tree)

pred_tree = as.data.frame(predict(op_tree,newdata = FC_Test))
pred_tree["final"] = NULL
pred_test_df = predict(op_tree,newdata= FC_Test)

mean(pred_test_df==FC_Test$risky_good)

CrossTable(FC_Test$risky_good,pred_test_df)

confusionMatrix(FC_Test$risky_good, pred_test_df)

