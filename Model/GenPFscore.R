#####################################################
##Cdes1 = read.csv("Pubchem.csv", header = TRUE)
##Cdes2 = read.csv("AP2DC.csv", header = TRUE)
##Cdes3 = read.csv("FP4C.csv", header = TRUE)
##Cdes4 = read.csv("MACCS.csv", header = TRUE)
##Cdes5 = read.csv("CDKExt.csv", header = TRUE)

#########################################################
###############    Descriptor 1   #######################
#########################################################

D1 = data.frame(Cdes1)
D = data.frame(D1, Activity = Class[,4])
pos <- subset(D, Activity == 'active')
neg <- subset(D, Activity == 'inactive')
set.seed(123)
trpos <- pos[tridpos2 , ]
trneg <- neg[tridneg2, ]
tspos <- pos[-tridpos2 , ]
tsneg <- neg[-tridneg2, ]
Dtr = na.omit(rbind(trpos, trneg))
Dts = na.omit(rbind(tspos, tsneg))
trpos <- pos[tridpos2 , ]
trneg <- neg[tridneg2, ]
tspos <- pos[-tridpos2 , ]
tsneg <- neg[-tridneg2, ]
Dtr = na.omit(rbind(trpos, trneg))
Dts = na.omit(rbind(tspos, tsneg))
Dtr$Activity <- as.factor(Dtr$Activity)
Dts$Activity <- as.factor(Dts$Activity)

PF1 = matrix(nrow = nrow(Dtr), ncol = 10)
PF2 = matrix(nrow = nrow(Dts), ncol = 10)
PFcv = matrix(nrow = nrow(Dtr), ncol = 1)
PFind = matrix(nrow = nrow(Dts), ncol = 1)

######################################################
##id3step <- sample(1:5,nrow(trpos),replace=TRUE)
trpos_sub1 <-  subset(trpos, id3step  ==  c(1))
trpos_sub2 <-  subset(trpos, id3step  ==  c(2))
trpos_sub3 <-  subset(trpos, id3step  ==  c(3))
trpos_sub4 <-  subset(trpos, id3step  ==  c(4))
trpos_sub5 <-  subset(trpos, id3step  ==  c(5))

################################################################
####################################################################
Dtr3step = rbind(trpos_sub1,trneg)

for (h in 1:k){
train <- subset(Dtr3step, idsub1 !=   c(h))
test <-  subset(Dtr3step, idsub1  ==  c(h))
M <- train(Activity ~ ., data = test,  method = "svmRadial", trControl = trainControl(method = "cv", number = 5, classProbs =  TRUE,sampling = "down"), tuneLength = 5, metric=c("Accuracy"), na.action=na.exclude)
PF1[, h] <- predict(M, Dtr, type="prob", se.fit=TRUE)[,1]
PF2[, h] <- predict(M, Dts, type="prob", se.fit=TRUE)[,1]
}

for (i in 1: nrow(Dtr)){
PFcv[i,] = mean(PF1[i,])
}

for (i in 1: nrow(Dts)){
PFind[i,] = mean(PF2[i,])
}

PFV11 = rbind(data.frame(P = PFcv), data.frame(P = PFind))

################################################################
####################################################################
Dtr3step = rbind(trpos_sub2,trneg)

for (h in 1:k){
train <- subset(Dtr3step, idsub1 !=   c(h))
test <-  subset(Dtr3step, idsub1  ==  c(h))
M <- train(Activity ~ ., data = test,  method = "svmRadial", trControl = trainControl(method = "cv", number = 5, classProbs =  TRUE,sampling = "down"), tuneLength = 5, metric=c("Accuracy"), na.action=na.exclude)
PF1[, h] <- predict(M, Dtr, type="prob", se.fit=TRUE)[,1]
PF2[, h] <- predict(M, Dts, type="prob", se.fit=TRUE)[,1]
}

for (i in 1: nrow(Dtr)){
PFcv[i,] = mean(PF1[i,])
}

for (i in 1: nrow(Dts)){
PFind[i,] = mean(PF2[i,])
}

PFV12 = rbind(data.frame(P = PFcv), data.frame(P = PFind))

################################################################
####################################################################
Dtr3step = rbind(trpos_sub3,trneg)

for (h in 1:k){
train <- subset(Dtr3step, idsub1 !=   c(h))
test <-  subset(Dtr3step, idsub1  ==  c(h))
M <- train(Activity ~ ., data = test,  method = "svmRadial", trControl = trainControl(method = "cv", number = 5, classProbs =  TRUE,sampling = "down"), tuneLength = 5, metric=c("Accuracy"), na.action=na.exclude)
PF1[, h] <- predict(M, Dtr, type="prob", se.fit=TRUE)[,1]
PF2[, h] <- predict(M, Dts, type="prob", se.fit=TRUE)[,1]
}

for (i in 1: nrow(Dtr)){
PFcv[i,] = mean(PF1[i,])
}

for (i in 1: nrow(Dts)){
PFind[i,] = mean(PF2[i,])
}

PFV13 = rbind(data.frame(P = PFcv), data.frame(P = PFind))


################################################################
####################################################################
Dtr3step = rbind(trpos_sub4,trneg)

for (h in 1:k){
train <- subset(Dtr3step, idsub1 !=   c(h))
test <-  subset(Dtr3step, idsub1  ==  c(h))
M <- train(Activity ~ ., data = test,  method = "svmRadial", trControl = trainControl(method = "cv", number = 5, classProbs =  TRUE,sampling = "down"), tuneLength = 5, metric=c("Accuracy"), na.action=na.exclude)
PF1[, h] <- predict(M, Dtr, type="prob", se.fit=TRUE)[,1]
PF2[, h] <- predict(M, Dts, type="prob", se.fit=TRUE)[,1]
}

for (i in 1: nrow(Dtr)){
PFcv[i,] = mean(PF1[i,])
}

for (i in 1: nrow(Dts)){
PFind[i,] = mean(PF2[i,])
}

PFV14 = rbind(data.frame(P = PFcv), data.frame(P = PFind))

################################################################
####################################################################
Dtr3step = rbind(trpos_sub5,trneg)

for (h in 1:k){
train <- subset(Dtr3step, idsub1 !=   c(h))
test <-  subset(Dtr3step, idsub1  ==  c(h))
M <- train(Activity ~ ., data = test,  method = "svmRadial", trControl = trainControl(method = "cv", number = 5, classProbs =  TRUE,sampling = "down"), tuneLength = 5, metric=c("Accuracy"), na.action=na.exclude)
PF1[, h] <- predict(M, Dtr, type="prob", se.fit=TRUE)[,1]
PF2[, h] <- predict(M, Dts, type="prob", se.fit=TRUE)[,1]
}

for (i in 1: nrow(Dtr)){
PFcv[i,] = mean(PF1[i,])
}

for (i in 1: nrow(Dts)){
PFind[i,] = mean(PF2[i,])
}

PFV15 = rbind(data.frame(P = PFcv), data.frame(P = PFind))
