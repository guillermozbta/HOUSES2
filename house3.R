train=read.csv("train.csv")
house=subset(train, GrLivArea<=4000)

test=read.csv("test.csv")
temp = read.csv("train.csv", stringsAsFactors=FALSE)
temp = subset(temp, GrLivArea<=4000)



house$isPool = as.factor(house$PoolArea > 0)
temp$BsmtQual[is.na(temp$BsmtQual)] = "O"

house$BsmtQual = as.factor(temp$BsmtQual)



temp$BsmtCond[is.na(temp$BsmtCond)] = "O"

house$BsmtCond = as.factor(temp$BsmtCond)



temp$BsmtExposure[is.na(temp$BsmtExposure)] = "O"

house$BsmtExposure = as.factor(temp$BsmtExposure)



temp$BsmtFinType1[is.na(temp$BsmtFinType1)] = "O"

house$BsmtFinType1 = as.factor(temp$BsmtFinType1)



temp$BsmtFinType2[is.na(temp$BsmtFinType2)] = "O"

house$BsmtFinType2 = as.factor(temp$BsmtFinType2)



temp$Electrical[is.na(temp$Electrical)] = "O"

house$Electrical = as.factor(temp$Electrical)



temp$GarageType[is.na(temp$GarageType)] = "O"

house$GarageType = as.factor(temp$GarageType)



temp$GarageQual[is.na(temp$GarageQual)] = "O"

house$GarageQual = as.factor(temp$GarageQual)



temp$Fence[is.na(temp$Fence)] = "O"

house$Fence = as.factor(temp$Fence)

temp = read.csv("test.csv", stringsAsFactors=FALSE)


temp$BsmtQual[is.na(temp$BsmtQual)] = "O"

test$BsmtQual = as.factor(temp$BsmtQual)



temp$BsmtCond[is.na(temp$BsmtCond)] = "O"

test$BsmtCond = as.factor(temp$BsmtCond)



temp$BsmtExposure[is.na(temp$BsmtExposure)] = "O"

test$BsmtExposure = as.factor(temp$BsmtExposure)



temp$BsmtFinType1[is.na(temp$BsmtFinType1)] = "O"

test$BsmtFinType1 = as.factor(temp$BsmtFinType1)



temp$BsmtFinType2[is.na(temp$BsmtFinType2)] = "O"

test$BsmtFinType2 = as.factor(temp$BsmtFinType2)



temp$Electrical[is.na(temp$Electrical)] = "O"

test$Electrical = as.factor(temp$Electrical)



temp$GarageType[is.na(temp$GarageType)] = "O"

test$GarageType = as.factor(temp$GarageType)



temp$GarageQual[is.na(temp$GarageQual)] = "O"

test$GarageQual = as.factor(temp$GarageQual)



temp$Fence[is.na(temp$Fence)] = "O"

test$Fence = as.factor(temp$Fence)


test$Exterior1st[is.na(test$Exterior1st)] = "VinylSd"
test$Exterior2nd[is.na(test$Exterior2nd)] = "VinylSd"
test$TotalBsmtSF[is.na(test$TotalBsmtSF)] = 988
test$KitchenQual[is.na(test$KitchenQual)] = "TA"
test$SaleType[is.na(test$SaleType)] = "WD"
test$BsmtUnfSF[is.na(test$BsmtUnfSF)] = 460
test$GarageCars[is.na(test$GarageCars)] = 2
test$Functional[is.na(test$Functional)] = "Typ"
test$BsmtFinSF1[is.na(test$BsmtFinSF1)] = 381
test$MSZoning[is.na(test$MSZoning)] = "RL"


house$LandContour = relevel(house$LandContour, "Lvl")

house$LandSlope = relevel(house$LandSlope, "Gtl")

house$Neighborhood = relevel(house$Neighborhood, "NAmes")

house$Condition1 = relevel(house$Condition1, "Norm")

house$BldgType = relevel(house$BldgType, "1Fam")

house$HouseStyle = relevel(house$HouseStyle, "1Story")

house$Exterior1st = relevel(house$Exterior1st, "VinylSd")

house$Exterior2nd = relevel(house$Exterior2nd, "VinylSd")

house$ExterQual = relevel(house$ExterQual, "TA")

house$ExterCond = relevel(house$ExterCond, "TA")

house$Foundation = relevel(house$Foundation, "PConc")

house$BsmtQual = relevel(house$BsmtQual, "TA")

house$BsmtCond = relevel(house$BsmtCond, "TA")

house$BsmtExposure = relevel(house$BsmtExposure, "No")

house$BsmtFinType1 = relevel(house$BsmtFinType1, "Unf")

house$BsmtFinType2 = relevel(house$BsmtFinType2, "Unf")

house$Heating = relevel(house$Heating, "GasA")

house$HeatingQC = relevel(house$HeatingQC, "Ex")

house$CentralAir = relevel(house$CentralAir, "Y")

house$Electrical = relevel(house$Electrical, "SBrkr")

house$KitchenQual = relevel(house$KitchenQual, "TA")

house$Functional = relevel(house$Functional, "Typ")

house$GarageType = relevel(house$GarageType, "Attchd")

house$GarageQual = relevel(house$GarageQual, "TA")

house$PavedDrive = relevel(house$PavedDrive, "Y")

house$SaleType = relevel(house$SaleType, "WD")

house$SaleCondition = relevel(house$SaleCondition, "Normal")

house$Fence = relevel(house$Fence, "O")


house$logPrice=log(house$SalePrice)
house$logGLA = log(house$GrLivArea)
test$logGLA = log(test$GrLivArea)
house$logLA = log(house$LotArea)
test$logLA = log(test$LotArea)

model1 = lm(logPrice~BsmtFinSF1+logLA+Functional+Neighborhood+Condition1+YearBuilt+YearRemodAdd+Exterior1st+BsmtQual+BsmtExposure+BsmtFinType1+TotalBsmtSF+CentralAir+logGLA+KitchenAbvGr+KitchenQual+GarageQual+SaleCondition+GarageCars+WoodDeckSF+OverallQual+X2ndFlrSF+Fireplaces+Porch+OverallCond+HouseStyle, data = house)

Predict1=predict(model1, test)
Predict1=exp(Predict1)
submit1 <- data.frame(Id = test$Id, SalePrice = Predict1)
write.csv(submit1, file = "house.csv", row.names = FALSE)
