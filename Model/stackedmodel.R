PFpls = read.csv("PFpls2.csv", header = TRUE)
PFknn = read.csv("PFknn2.csv", header = TRUE)
PFrf  = read.csv("PFrf2.csv", header = TRUE)
PFxgb  = read.csv("PFxgb2.csv", header = TRUE)
PFsvm = read.csv("PFsvmRadial2.csv", header = TRUE)
PFmlp = read.csv("PFmlp2.csv", header = TRUE)

DPF = data.frame(PFpls[,-1], PFknn[,-1], PFrf[,-1], PFxgb[,-1], PFsvm[,-1], PFmlp[,-1])
Dtr6D = data.frame(DPF[1: 1191, ], Activity = Dtr$Activity)
Dts6D = data.frame(DPF[1192: nrow(DPF) , ], Activity = Dts$Activity)


#########################################################

prediction <- data.frame()
testsetCopy <- data.frame()
auc = matrix(nrow = 10, ncol = 1)
set.seed(123)

for (h in 1:k){
train <- na.omit(subset(Dtr6D, id !=   c(h)))
test <-  na.omit(subset(Dtr6D, id  ==  c(h)))
M <- train(Activity ~ ., data = train,  method = "pls", trControl = trainControl(method = "cv", number = 5, classProbs =  TRUE,sampling = "down"), tuneLength = 5, metric=c("Accuracy"), na.action=na.exclude)
pred3 <- predict(M, test)
predprob <- predict(M, test, type="prob", se.fit=TRUE)
auc[h, ] = getROC_AUC(predprob[,1], as.numeric(as.factor(test[,ncol(test)])))$auc
prediction <- rbind(prediction, data.frame(pred3))
testsetCopy <- rbind(testsetCopy, as.data.frame(test[,ncol(test)]))
}

Dat <-  table(data.frame( prediction, testsetCopy))
result <- data.frame( prediction, testsetCopy)
ACCCV = (Dat[1] + Dat[4])/nrow(Dtr6D)
SNCV = sensitivity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
SPCV = specificity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
BACCCV = (SNCV + SPCV) *0.5
MCCCV = mcc(as.factor(result[,1]), as.factor(result[,2]))
AUCCV = mean(auc)

#########################################################
M <- train(Activity ~ ., data = Dtr6D,  method = "pls", trControl = trainControl(method = "cv", number = 5, classProbs =  TRUE,sampling = "down"), tuneLength = 5, metric=c("Accuracy"),na.action=na.exclude)
pred3 <- predict(M, Dts6D) 
predprob <- predict(M, Dts6D, type="prob", se.fit=TRUE)
AUCIND = getROC_AUC(predprob[,1], as.numeric(as.factor(Dts6D[,ncol(Dts6D)])))$auc
Dat <-  table(data.frame( pred3, Dts6D$Activity))
result <- data.frame( pred3, Dts6D$Activity)
ACCIND = (Dat[1] + Dat[4])/nrow(Dts6D)
SNIND = sensitivity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
SPIND = specificity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
BACCIND = (SNIND + SPIND) *0.5
MCCIND = mcc(as.factor(result[,1]), as.factor(result[,2]))

mPLS = cbind(ACCCV, BACCCV, SNCV, SPCV, MCCCV, AUCCV, ACCIND, BACCIND, SNIND, SPIND, MCCIND, AUCIND)


#########################################################

prediction <- data.frame()
testsetCopy <- data.frame()
auc = matrix(nrow = 10, ncol = 1)
set.seed(123)

for (h in 1:k){
train <- na.omit(subset(Dtr6D, id !=   c(h)))
test <-  na.omit(subset(Dtr6D, id  ==  c(h)))
M <- train(Activity ~ ., data = train,  method = "knn", trControl = trainControl(method = "cv", number = 5, classProbs =  TRUE,sampling = "down"), tuneLength = 5, metric=c("Accuracy"), na.action=na.exclude)
pred3 <- predict(M, test)
predprob <- predict(M, test, type="prob", se.fit=TRUE)
auc[h, ] = getROC_AUC(predprob[,1], as.numeric(as.factor(test[,ncol(test)])))$auc
prediction <- rbind(prediction, data.frame(pred3))
testsetCopy <- rbind(testsetCopy, as.data.frame(test[,ncol(test)]))
}

Dat <-  table(data.frame( prediction, testsetCopy))
result <- data.frame( prediction, testsetCopy)
ACCCV = (Dat[1] + Dat[4])/nrow(Dtr6D)
SNCV = sensitivity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
SPCV = specificity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
BACCCV = (SNCV + SPCV) *0.5
MCCCV = mcc(as.factor(result[,1]), as.factor(result[,2]))
AUCCV = mean(auc)

#########################################################
M <- train(Activity ~ ., data = Dtr6D,  method = "knn", trControl = trainControl(method = "cv", number = 5, classProbs =  TRUE,sampling = "down"), tuneLength = 5, metric=c("Accuracy"),na.action=na.exclude)
pred3 <- predict(M, Dts6D) 
predprob <- predict(M, Dts6D, type="prob", se.fit=TRUE)
AUCIND = getROC_AUC(predprob[,1], as.numeric(as.factor(Dts6D[,ncol(Dts6D)])))$auc
Dat <-  table(data.frame( pred3, Dts6D$Activity))
result <- data.frame( pred3, Dts6D$Activity)
ACCIND = (Dat[1] + Dat[4])/nrow(Dts6D)
SNIND = sensitivity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
SPIND = specificity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
BACCIND = (SNIND + SPIND) *0.5
MCCIND = mcc(as.factor(result[,1]), as.factor(result[,2]))

mkNN = cbind(ACCCV, BACCCV, SNCV, SPCV, MCCCV, AUCCV, ACCIND, BACCIND, SNIND, SPIND, MCCIND, AUCIND)

#########################################################

prediction <- data.frame()
testsetCopy <- data.frame()
auc = matrix(nrow = 10, ncol = 1)
set.seed(123)

for (h in 1:k){
train <- na.omit(subset(Dtr6D, id !=   c(h)))
test <-  na.omit(subset(Dtr6D, id  ==  c(h)))
M <- train(Activity ~ ., data = train,  method = "mlp", trControl = trainControl(method = "cv", number = 5, classProbs =  TRUE,sampling = "down"), tuneLength = 5, metric=c("Accuracy"), na.action=na.exclude)
pred3 <- predict(M, test)
predprob <- predict(M, test, type="prob", se.fit=TRUE)
auc[h, ] = getROC_AUC(predprob[,1], as.numeric(as.factor(test[,ncol(test)])))$auc
prediction <- rbind(prediction, data.frame(pred3))
testsetCopy <- rbind(testsetCopy, as.data.frame(test[,ncol(test)]))
}

Dat <-  table(data.frame( prediction, testsetCopy))
result <- data.frame( prediction, testsetCopy)
ACCCV = (Dat[1] + Dat[4])/nrow(Dtr6D)
SNCV = sensitivity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
SPCV = specificity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
BACCCV = (SNCV + SPCV) *0.5
MCCCV = mcc(as.factor(result[,1]), as.factor(result[,2]))
AUCCV = mean(auc)

#########################################################
M <- train(Activity ~ ., data = Dtr6D,  method = "mlp", trControl = trainControl(method = "cv", number = 5, classProbs =  TRUE,sampling = "down"), tuneLength = 5, metric=c("Accuracy"),na.action=na.exclude)
pred3 <- predict(M, Dts6D) 
predprob <- predict(M, Dts6D, type="prob", se.fit=TRUE)
AUCIND = getROC_AUC(predprob[,1], as.numeric(as.factor(Dts6D[,ncol(Dts6D)])))$auc
Dat <-  table(data.frame( pred3, Dts6D$Activity))
result <- data.frame( pred3, Dts6D$Activity)
ACCIND = (Dat[1] + Dat[4])/nrow(Dts6D)
SNIND = sensitivity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
SPIND = specificity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
BACCIND = (SNIND + SPIND) *0.5
MCCIND = mcc(as.factor(result[,1]), as.factor(result[,2]))

mMLP = cbind(ACCCV, BACCCV, SNCV, SPCV, MCCCV, AUCCV, ACCIND, BACCIND, SNIND, SPIND, MCCIND, AUCIND)

#########################################################

prediction <- data.frame()
testsetCopy <- data.frame()
auc = matrix(nrow = 10, ncol = 1)
set.seed(123)

for (h in 1:k){
train <- na.omit(subset(Dtr6D, id !=   c(h)))
test <-  na.omit(subset(Dtr6D, id  ==  c(h)))
M <- train(Activity ~ ., data = train, method = "svmRadial", trControl = trainControl(method = "cv", number = 5, classProbs =  TRUE,sampling = "down"), tuneLength = 5, metric=c("Accuracy"), na.action=na.exclude)
pred3 <- predict(M, test)
predprob <- predict(M, test, type="prob", se.fit=TRUE)
auc[h, ] = getROC_AUC(predprob[,1], as.numeric(as.factor(test[,ncol(test)])))$auc
prediction <- rbind(prediction, data.frame(pred3))
testsetCopy <- rbind(testsetCopy, as.data.frame(test[,ncol(test)]))
}

Dat <-  table(data.frame( prediction, testsetCopy))
result <- data.frame( prediction, testsetCopy)
ACCCV = (Dat[1] + Dat[4])/nrow(Dtr6D)
SNCV = sensitivity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
SPCV = specificity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
BACCCV = (SNCV + SPCV) *0.5
MCCCV = mcc(as.factor(result[,1]), as.factor(result[,2]))
AUCCV = mean(auc)

#########################################################
M <- train(Activity ~ ., data = Dtr6D, method = "svmRadial", trControl = trainControl(method = "cv", number = 5, classProbs =  TRUE,sampling = "down"), tuneLength = 5, metric=c("Accuracy"),na.action=na.exclude)
pred3 <- predict(M, Dts6D) 
predprob <- predict(M, Dts6D, type="prob", se.fit=TRUE)
AUCIND = getROC_AUC(predprob[,1], as.numeric(as.factor(Dts6D[,ncol(Dts6D)])))$auc
Dat <-  table(data.frame( pred3, Dts6D$Activity))
result <- data.frame( pred3, Dts6D$Activity)
ACCIND = (Dat[1] + Dat[4])/nrow(Dts6D)
SNIND = sensitivity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
SPIND = specificity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
BACCIND = (SNIND + SPIND) *0.5
MCCIND = mcc(as.factor(result[,1]), as.factor(result[,2]))

mSVM = cbind(ACCCV, BACCCV, SNCV, SPCV, MCCCV, AUCCV, ACCIND, BACCIND, SNIND, SPIND, MCCIND, AUCIND)

#########################################################

prediction <- data.frame()
testsetCopy <- data.frame()
auc = matrix(nrow = 10, ncol = 1)
set.seed(123)

for (h in 1:k){
train <- na.omit(subset(Dtr6D, id !=   c(h)))
test <-  na.omit(subset(Dtr6D, id  ==  c(h)))
M <- train(Activity ~ ., data = train,  method = "xgbTree", trControl = trainControl(method = "cv", number = 5, classProbs =  TRUE,sampling = "down"), tuneGrid = Grid, metric=c("Accuracy"), na.action=na.exclude)
pred3 <- predict(M, test)
predprob <- predict(M, test, type="prob", se.fit=TRUE)
auc[h, ] = getROC_AUC(predprob[,1], as.numeric(as.factor(test[,ncol(test)])))$auc
prediction <- rbind(prediction, data.frame(pred3))
testsetCopy <- rbind(testsetCopy, as.data.frame(test[,ncol(test)]))
}

Dat <-  table(data.frame( prediction, testsetCopy))
result <- data.frame( prediction, testsetCopy)
ACCCV = (Dat[1] + Dat[4])/nrow(Dtr6D)
SNCV = sensitivity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
SPCV = specificity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
BACCCV = (SNCV + SPCV) *0.5
MCCCV = mcc(as.factor(result[,1]), as.factor(result[,2]))
AUCCV = mean(auc)

#########################################################
M <- train(Activity ~ ., data = Dtr6D, method = "xgbTree", trControl = trainControl(method = "cv", number = 5, classProbs =  TRUE,sampling = "down"), tuneGrid = Grid, metric=c("Accuracy"),na.action=na.exclude)
pred3 <- predict(M, Dts6D) 
predprob <- predict(M, Dts6D, type="prob", se.fit=TRUE)
AUCIND = getROC_AUC(predprob[,1], as.numeric(as.factor(Dts6D[,ncol(Dts6D)])))$auc
Dat <-  table(data.frame( pred3, Dts6D$Activity))
result <- data.frame( pred3, Dts6D$Activity)
ACCIND = (Dat[1] + Dat[4])/nrow(Dts6D)
SNIND = sensitivity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
SPIND = specificity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
BACCIND = (SNIND + SPIND) *0.5
MCCIND = mcc(as.factor(result[,1]), as.factor(result[,2]))

mXGB = cbind(ACCCV, BACCCV, SNCV, SPCV, MCCCV, AUCCV, ACCIND, BACCIND, SNIND, SPIND, MCCIND, AUCIND)

######################################################
tunegrid <- expand.grid(.mtry= c(20),.ntree= c(100, 200, 300, 400, 500))

prediction <- data.frame()
testsetCopy <- data.frame()
auc = matrix(nrow = 10, ncol = 1)
set.seed(123)

for (h in 1:k){
train <- na.omit(subset(Dtr6D, id !=   c(h)))
test <-  na.omit(subset(Dtr6D, id  ==  c(h)))
M <- train(Activity ~ ., data = train,  method=customRF,  trControl = trainControl(method = "cv", number = 5, classProbs =  TRUE,sampling = "down"), tuneGrid=tunegrid, metric=c("Accuracy"), na.action=na.exclude)
pred3 <- predict(M, test)
predprob <- predict(M, test, type="prob", se.fit=TRUE)
auc[h, ] = getROC_AUC(predprob[,1], as.numeric(as.factor(test[,ncol(test)])))$auc
prediction <- rbind(prediction, data.frame(pred3))
testsetCopy <- rbind(testsetCopy, as.data.frame(test[,ncol(test)]))
}

Dat <-  table(data.frame( prediction, testsetCopy))
result <- data.frame( prediction, testsetCopy)
ACCCV = (Dat[1] + Dat[4])/nrow(Dtr6D)
SNCV = sensitivity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
SPCV = specificity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
BACCCV = (SNCV + SPCV) *0.5
MCCCV = mcc(as.factor(result[,1]), as.factor(result[,2]))
AUCCV = mean(auc)

#########################################################
M <- train(Activity ~ ., data = Dtr6D, method=customRF,  trControl = trainControl(method = "cv", number = 5, classProbs =  TRUE,sampling = "down"), tuneGrid=tunegrid, metric=c("Accuracy"),na.action=na.exclude)
pred3 <- predict(M, Dts6D) 
predprob <- predict(M, Dts6D, type="prob", se.fit=TRUE)
AUCIND = getROC_AUC(predprob[,1], as.numeric(as.factor(Dts6D[,ncol(Dts6D)])))$auc
Dat <-  table(data.frame( pred3, Dts6D$Activity))
result <- data.frame( pred3, Dts6D$Activity)
ACCIND = (Dat[1] + Dat[4])/nrow(Dts6D)
SNIND = sensitivity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
SPIND = specificity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
BACCIND = (SNIND + SPIND) *0.5
MCCIND = mcc(as.factor(result[,1]), as.factor(result[,2]))

mRF = cbind(ACCCV, BACCCV, SNCV, SPCV, MCCCV, AUCCV, ACCIND, BACCIND, SNIND, SPIND, MCCIND, AUCIND)


rbind(mkNN, mPLS, mMLP, mSVM, mRF, mXGB)