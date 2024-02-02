creditc$Fraud = factor(creditc$Fraud, levels = c(0,1))

head(creditc,7)

tail(creditc)

nrow(creditc)

ncol(creditc)

sum(is.na(creditc))

summary(creditc)

creditc$Zip[which(is.na(creditc$Zip))] = mean(is.na(creditc$Zip),na.rm = TRUE)

sum(is.na(creditc))

table(creditc$Fraud)

barplot(table(creditc$Fraud))

library(caret)

featurePlot(x = creditc[2:5], 
            y = creditc$Fraud, 
            plot = "box",
            strip=strip.custom(par.strip.text=list(cex=.7)),
            scales = list(x = list(relation="free"), 
                          y = list(relation="free")))


set.seed(123)

k = sample(1:nrow(creditc), 0.8*nrow(creditc))

train = creditc[k,]

test = creditc[-k,]

#Libraries

library(randomForest)

library(ROSE)

# Random over-sampling and Random under-sampling

sampling = ovun.sample(Fraud ~., data = train, method = "both",seed = 123)$data


table(sampling$Fraud)

barplot(table(sampling$Fraud))

classifier_both =randomForest(Fraud ~ Card + Year+ Amount + MCC, data = sampling)

fraud_pred_both = predict(classifier_both,test)

test$fraud_pred_both = fraud_pred_both

table(test$Fraud)

table(test$fraud_pred_both)




# Checking accuracy on test dataset

confusionMatrix(test$Fraud,test$fraud_pred_both)

#_________________________________________

#Random over-sampling

oversampling = ovun.sample(Fraud ~., data = train, method = "over",seed = 123)$data

table(oversampling$Fraud)

barplot(table(oversampling$Fraud))

classifier_over = randomForest(Fraud ~ Card + Year+ Amount + MCC, data = oversampling)

fraud_pred_over = predict(classifier_over,test)

test$fraud_pred_over = fraud_pred_over

table(test$Fraud)

table(test$fraud_pred_over)


# Checking accuracy on test dataset

confusionMatrix(test$Fraud,test$fraud_pred_over)



