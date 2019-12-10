library(party)
library(caret)
library(C50)
library(tree)
library(gmodels)

company_data = read.csv(file.choose())

View(company_data)
summary(company_data)
data(company_data)
table(company_data$Sales)
scale(company_data$Sales)
plot(company_data$Sales)

high = ifelse(company_data$Sales<10, "No", "Yes")
CompD = data.frame(company_data, high)
CompD_train = CompD[1:200,]
CompD_test = CompD[201:400,]

#DT
op_tree = ctree(high ~ CompPrice + Income + Advertising + Population + Price + ShelveLoc + Age + Education + Urban + US, data = CompD_train)
summary(op_tree)

plot(op_tree)

#-------------Rpart------------------

library(rpart)
tree1 = rpart(high ~ CompPrice + Income + Advertising + Population + Price + ShelveLoc + Age + Education + Urban + US, data = CompD_train)
library(rpart.plot)
rpart.plot(tree1)



#probability = 0.6 for product sale to customer


pred_tree <- as.data.frame(predict(op_tree,newdata=CompD_test))
pred_tree["final"] <- NULL
pred_test_df <- predict(op_tree,newdata=CompD_test)


mean(pred_test_df==CompD$high)

CrossTable(CompD_test$high,pred_test_df)

confusionMatrix(CompD_test$high,pred_test_df)

CompD_tree_org <- tree(high~.-Sales,data=CompD)
summary(CompD_tree_org)

cd_tree <- tree(high~.-Sales,data=CompD_train)
summary(cd_tree)
plot(CompD_tree_org)
text(CompD_tree_org,pretty = 0)

#model Evaluation
pred_tree = as.data.frame(predict(cd_tree,newdata = CompD_test))
pred_tree["final"] = NULL
pred_test_df = predict(cd_tree,newdata = CompD_test)
pred_tree$final <- colnames(pred_test_df)[apply(pred_test_df,1,which.max)]

pred_tree$final <- as.factor(pred_tree$final)
summary(pred_tree$final)


summary(CompD_test$high)

mean(pred_tree$final==CompD$high)
CrossTable(CompD_test$high,pred_tree$final)
          
confusionMatrix(CompD_test$high,pred_tree$final)

