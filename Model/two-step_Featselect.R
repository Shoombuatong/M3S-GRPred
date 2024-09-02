PFpls = read.csv("PFpls2.csv", header = TRUE)
PFknn = read.csv("PFknn2.csv", header = TRUE)
PFrf  = read.csv("PFrf2.csv", header = TRUE)
PFxgb  = read.csv("PFxgb2.csv", header = TRUE)
PFsvm = read.csv("PFsvmRadial2.csv", header = TRUE)
PFmlp = read.csv("PFmlp2.csv", header = TRUE)


DPF = data.frame(PFpls[,-1], PFknn[,-1], PFrf[,-1], PFxgb[,-1], PFsvm[,-1], PFmlp[,-1])

Dtr = data.frame(DPF[1: 1191, ], Activity = Dtr$Activity)
Dts = data.frame(DPF[1192: nrow(DPF) , ], Activity = Dts$Activity)

############################################################
############## 2-step feature set determination ############
############################################################

ind= seq(5, 50, 5)
n = ncol(Dtr)-1
gini = matrix(nrow = n, ncol = 10)
meangini = matrix(nrow = n, ncol = 1)

for (i in 1:10){
RF<-randomForest( Activity ~., data= Dtr, ntree= 200, mtry=ind[i], importance=TRUE)
gini[,i] = RF$ importance [,4]
}

for (i in 1:n){
meangini[i,] = mean(gini[i,])
}

Dfeat = data.frame(meangini,t(Dtr[,-ncol(Dtr)]))
Dsort <- Dfeat[order(Dfeat$meangini,decreasing=TRUE),]
Dsort2 <- t(Dsort[,-1])

Dfeat = data.frame(meangini,t(Dts[,-ncol(Dts)]))
Dsort11 <- Dfeat[order(Dfeat$meangini,decreasing=TRUE),]
Dsort12 <- t(Dsort11[,-1])

############## Step 2 Optimal feature set determination #############

ind= seq(10, 150, 10)
ACCCV = matrix(nrow = length(ind), ncol = 1)
SNCV = matrix(nrow = length(ind), ncol = 1)
SPCV = matrix(nrow = length(ind), ncol = 1)
MCCCV = matrix(nrow = length(ind), ncol = 1)
AUCCV = matrix(nrow = length(ind), ncol = 1)
ACCIND = matrix(nrow = length(ind), ncol = 1)
SNIND = matrix(nrow = length(ind), ncol = 1)
SPIND = matrix(nrow = length(ind), ncol = 1)
MCCIND = matrix(nrow = length(ind), ncol = 1)
AUCIND = matrix(nrow = length(ind), ncol = 1)
auc = matrix(nrow = 10, ncol = 1)
set.seed(123)

for (i in 1:length(ind)){
internal = data.frame(Dsort2[, 1:ind[i]],  Activity = Dtr$Activity )
external = data.frame(Dsort12[, 1:ind[i]], Activity = Dts$Activity )

prediction <- data.frame()
testsetCopy <- data.frame()

############ Cross-validation ###################
for (h in 1:k){
train <-  na.omit(subset(internal, id !=   c(h)))
test <-   na.omit(subset(internal, id  ==  c(h)))
M <- train(Activity ~ ., data = train,  method = "svmRadial", trControl = trainControl(method = "cv", number = 5, classProbs =  TRUE,sampling = "down"), tuneLength = 5, metric=c("Accuracy"), na.action=na.exclude)
pred3 <- predict(M, test)
predprob <- predict(M, test, type="prob", se.fit=TRUE)
auc[h, ] = getROC_AUC(predprob[,1], as.numeric(as.factor(test[,ncol(test)])))$auc
prediction <- rbind(prediction, data.frame(pred3))
testsetCopy <- rbind(testsetCopy, as.data.frame(test[,ncol(test)]))
}

Dat <-  table(data.frame( prediction, testsetCopy))
result <- data.frame( prediction, testsetCopy)
ACCCV[i,] = (Dat[1] + Dat[4])/nrow(internal)
SNCV[i,] = sensitivity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
SPCV[i,] = specificity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
MCCCV[i,] = mcc(as.factor(result[,1]), as.factor(result[,2]))
AUCCV[i,] = mean(auc)

M <- train(Activity ~ ., data = internal,  method = "svmRadial", trControl = trainControl(method = "cv", number = 5, classProbs =  TRUE,sampling = "down"), tuneLength = 5, metric=c("Accuracy"), na.action=na.exclude)
pred3 <- predict(M, external)
predprob <- predict(M, external, type="prob", se.fit=TRUE)
AUCIND[i,] = getROC_AUC(predprob[,1], as.numeric(as.factor(test[,ncol(test)])))$auc
Dat <-  table(data.frame( pred3, external$Activity))
result <- data.frame( pred3, external$Activity)
ACCIND[i,] = (Dat[1] + Dat[4])/nrow(external)
SNIND[i,] = sensitivity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
SPIND[i,] = specificity(as.factor(result[,1]), as.factor(result[,2]))$measure[2]
MCCIND[i,] = mcc(as.factor(result[,1]), as.factor(result[,2]))
AUCIND[i,] = mean(auc)
}

findfeat = data.frame(ind, ACCCV, SNCV, SPCV, MCCCV, AUCCV, ACCIND, SNIND, SPIND, MCCIND, AUCIND)

findfeat[order(findfeat$MCCCV,decreasing=TRUE),] [1, ]
findfeat[order(findfeat$AUCCV,decreasing=TRUE),] [1, ]

write.csv(findfeat, "2stepfeat_SVM_recheck.csv", row.names=TRUE, na="")
