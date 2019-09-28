train <- read.csv('/Users/venkatapochiraju/Documents/R-files/FDS/FDS/train.csv', stringsAsFactors=FALSE)
test <- read.csv('/Users/venkatapochiraju/Documents/R-files/FDS/FDS/test.csv', stringsAsFactors=FALSE)
# Removing outliers
#ggplot(train) +
#  geom_point(aes(x = GrLivArea, y = SalePrice))
## Outliers - GrLivArea>4000
#ggplot(train) +
#  geom_point(aes(x = LotFrontage, y = SalePrice))
## Outliers - LotFrontage>200
#ggplot(train) +
#  geom_point(aes(x = LotArea, y = SalePrice))
## Outliers - LotArea>100000
#ggplot(train) +
#  geom_point(aes(x = MasVnrArea, y = SalePrice))
## Outliers - MasVnrArea>1250
#ggplot(train) +
#  geom_point(aes(x = TotalBsmtSF, y = SalePrice))
## Outliers - TotalBsmtSF>6000
train[train$GrLivArea>4000,]$GrLivArea <- mean(train$GrLivArea)%>%as.numeric
#train[train$LotFrontage>200,]$LotFrontage <- mean(train$LotFrontage)%>%as.numeric
train[train$LotArea>100000,]$LotArea <- mean(train$LotArea)%>%as.numeric
#train[train$MasVnrArea>1250,]$MasVnrArea <- mean(train$MasVnrArea)%>%as.numeric
train[train$TotalBsmtSF>6000,]$TotalBsmtSF <- mean(train$TotalBsmtSF)%>%as.numeric
#train$SalePrice <- log(train$SalePrice)
test$SalePrice <- as.numeric(0)

fullDataset <- rbind(train,test)
colSums(is.na(fullDataset))
#table(fullDataset$LotFrontage)
# No linear feet of street connected to property
fullDataset$LotFrontage[is.na(fullDataset$LotFrontage)] <- 0
#table(fullDataset$MasVnrType)
table(fullDataset$MasVnrArea)
is.na(table(fullDataset$MasVnrArea))
# No Masonry veener area in the house
fullDataset$MasVnrArea[is.na(fullDataset$MasVnrArea)] <- 0
table(fullDataset$BsmtFinSF1)
# No basement SF area means there is no basement. So, we cannot replace it with mean
#fullDataset$BsmtQual[is.na(fullDataset$BsmtQual)] <- 0  -- not required as NA is in the permitted values
#fullDataset$BsmtCond[is.na(fullDataset$BsmtCond)] <- 0  -- not required as NA is in the permitted values
#fullDataset$BsmtExposure[is.na(fullDataset$BsmtExposure)] <- 0  -- not required as NA is in the permitted values
#fullDataset$BsmtFinType1[is.na(fullDataset$BsmtFinType1)] <- 0  -- not required as NA is in the permitted values
#fullDataset$BsmtFinType2[is.na(fullDataset$BsmtFinType2)] <- 0  -- not required as NA is in the permitted values
fullDataset$BsmtFinSF1[is.na(fullDataset$BsmtFinSF1)] <- 0
fullDataset$BsmtFinSF2[is.na(fullDataset$BsmtFinSF2)] <- 0
fullDataset$BsmtUnfSF[is.na(fullDataset$BsmtUnfSF)] <- 0
fullDataset$TotalBsmtSF[is.na(fullDataset$TotalBsmtSF)] <- 0
fullDataset$BsmtFullBath[is.na(fullDataset$BsmtFullBath)] <- 0
fullDataset$BsmtHalfBath[is.na(fullDataset$BsmtHalfBath)] <- 0
# Fireplaces - if na, that means no fireplace
#fullDataset$FireplaceQu[is.na(fullDataset$FireplaceQu)] <- 0  -- not required as NA is in the permitted values
# Garrage type - if na, means no garrage for the house
#fullDataset$GarageType[is.na(fullDataset$GarageType)] <- 0  -- not required as NA is in the permitted values
fullDataset$GarageYrBlt[is.na(fullDataset$GarageYrBlt)] <- 0
fullDataset$GarageCars[is.na(fullDataset$GarageCars)] <- 0
fullDataset$GarageArea[is.na(fullDataset$GarageArea)] <- 0
#ggplot(fullDataset) +
#    geom_point(aes(x = GarageYrBlt, y = SalePrice))
## There are one or more points in the graph that are in the future.
fullDataset$GarageYrBlt[fullDataset$GarageYrBlt > 2018] <- 2017
#ggplot(fullDataset) +
#    geom_point(aes(x = GarageYrBlt, y = SalePrice))
# Lets try to make all the variables numeric, looking at the list of data - data_description.txt
# Lets try to make all the variables numeric, looking at the list of data - data_description.txt
# MSZoning
table(fullDataset$MSZoning)
fullDataset$Zoning[fullDataset$MSZoning == 'C (all)'] <- 1
fullDataset$Zoning[fullDataset$MSZoning == 'FV'] <- 2
fullDataset$Zoning[fullDataset$MSZoning == 'RH'] <- 3
fullDataset$Zoning[fullDataset$MSZoning == 'RL'] <- 4
fullDataset$Zoning[fullDataset$MSZoning == 'RM'] <- 5
fullDataset$MSZoning <- NULL
# Street
table(fullDataset$Street)
fullDataset$RoadType[fullDataset$Street == 'Grvl'] <- 0
fullDataset$RoadType[fullDataset$Street == 'Pave'] <- 1
fullDataset$Street <- NULL
# Alley
table(fullDataset$Alley)
fullDataset$AlleyType[fullDataset$Alley == 'Grvl'] <- 0
fullDataset$AlleyType[fullDataset$Alley == 'Pave'] <- 1
fullDataset$Alley <- NULL
# LotShape
table(fullDataset$LotShape)
fullDataset$Shape[fullDataset$LotShape == 'Reg'] <- 0
fullDataset$Shape[fullDataset$LotShape != 'Reg'] <- 1  # 925 are regular, IR1, IR2, IR3 are a lot less
fullDataset$LotShape <- NULL
# LandCountour  
table(fullDataset$LandContour)
fullDataset$Flatness[fullDataset$LandContour == 'Lvl'] <- 0
fullDataset$Flatness[fullDataset$LandContour != 'Lvl'] <- 1 # 1131 are flat, others are a lot less
fullDataset$LandContour <- NULL
# Utilities
table(fullDataset$Utilities)
fullDataset$UtilType[fullDataset$Utilities != 'AllPub'] <- 0
fullDataset$UtilType[fullDataset$Utilities == 'AllPub'] <- 1
fullDataset$Utilities <- NULL
# LotConfig
table(fullDataset$LotConfig)
fullDataset$LotCon[fullDataset$LotConfig == 'Corner'] <- 1
fullDataset$LotCon[fullDataset$LotConfig == 'CulDSac'] <- 2
fullDataset$LotCon[fullDataset$LotConfig == 'FR2'] <- 3
fullDataset$LotCon[fullDataset$LotConfig == 'FR3'] <- 4
fullDataset$LotCon[fullDataset$LotConfig == 'Inside'] <- 5
fullDataset$LotConfig <- NULL
# LandSlope
table(fullDataset$LandSlope)
fullDataset$Slope[fullDataset$LandSlope != 'Gtl'] <- 0
fullDataset$Slope[fullDataset$LandSlope == 'Gtl'] <- 1
fullDataset$LandSlope <- NULL
# BldgType
table(fullDataset$BldgType)
fullDataset$BuildingType[fullDataset$BldgType == '1Fam'] <- 1
fullDataset$BuildingType[fullDataset$BldgType == 'TwnhsE'] <- 1
fullDataset$BuildingType[fullDataset$BldgType != '1Fam'] <- 0
fullDataset$BuildingType[fullDataset$BldgType != 'TwnhsE'] <- 0
fullDataset$BldgType <- NULL
# HouseStyle
table(fullDataset$HouseStyle)
#ggplot(fullDataset) +
#    geom_point(aes(x = HouseStyle, y = SalePrice))
fullDataset$HouseStyle[fullDataset$HouseStyle == '1.5Fin'] <- 1
fullDataset$HouseStyle[fullDataset$HouseStyle == '1.5Unf'] <- 2
fullDataset$HouseStyle[fullDataset$HouseStyle == '1Story'] <- 3
fullDataset$HouseStyle[fullDataset$HouseStyle == '2.5Fin'] <- 4
fullDataset$HouseStyle[fullDataset$HouseStyle == '2.5Unf'] <- 5
fullDataset$HouseStyle[fullDataset$HouseStyle == '2Story'] <- 6
fullDataset$HouseStyle[fullDataset$HouseStyle == 'SFoyer'] <- 7
fullDataset$HouseStyle[fullDataset$HouseStyle == 'SLvl'] <- 8
fullDataset$HouseStyle <- NULL
# RoofStyle
table(fullDataset$RoofStyle)
# As Gable, Hip, and Shed are almost of similar structure, lets group them together
fullDataset$RoofType[!fullDataset$RoofStyle %in% c('Gable','Hip', 'Shed')] <- 0
fullDataset$RoofType[fullDataset$RoofStyle %in% c('Gable','Hip', 'Shed')] <- 1
fullDataset$RoofStyle <- NULL
# RoofMatl
table(fullDataset$RoofMatl)
fullDataset$RoofMaterial[fullDataset$RoofMatl == 'ClyTile'] <- 1
fullDataset$RoofMaterial[fullDataset$RoofMatl == 'CompShg'] <- 2
fullDataset$RoofMaterial[fullDataset$RoofMatl == 'Membran'] <- 3
fullDataset$RoofMaterial[fullDataset$RoofMatl == 'Metal'] <- 4
fullDataset$RoofMaterial[fullDataset$RoofMatl == 'Roll'] <- 5
fullDataset$RoofMaterial[fullDataset$RoofMatl == 'Tar&Grv'] <- 6
fullDataset$RoofMaterial[fullDataset$RoofMatl == 'WdShake'] <- 7
fullDataset$RoofMaterial[fullDataset$RoofMatl == 'WdShngl'] <- 8
fullDataset$RoofMatl <- NULL
# MasVnrType
table(fullDataset$MasVnrType)
#cor(fullDataset$SalePrice,fullDataset)
fullDataset$MasonryType[fullDataset$MasVnrType == 'None'] <- 0
fullDataset$MasonryType[fullDataset$MasVnrType != 'None'] <- 1
fullDataset$MasVnrType <- NULL
# ExterQual
table(fullDataset$ExterQual)
fullDataset$ExtQuality[fullDataset$ExterQual == 'Ex'] <- 1
fullDataset$ExtQuality[fullDataset$ExterQual == 'Fa'] <- 2
fullDataset$ExtQuality[fullDataset$ExterQual == 'Gd'] <- 3
fullDataset$ExtQuality[fullDataset$ExterQual == 'TA'] <- 4
fullDataset$ExterQual <- NULL
# ExterCond
table(fullDataset$ExterCond)
fullDataset$ExtCond[fullDataset$ExterCond == 'Ex'] <- 1  
fullDataset$ExtCond[fullDataset$ExterCond == 'Fa'] <- 2  
fullDataset$ExtCond[fullDataset$ExterCond == 'Gd'] <- 3  
fullDataset$ExtCond[fullDataset$ExterCond == 'Po'] <- 4  
fullDataset$ExtCond[fullDataset$ExterCond == 'TA'] <- 5
fullDataset$ExterCond <- NULL
# Foundation
table(fullDataset$Foundation)
fullDataset$FoundationType[fullDataset$Foundation == 'BrkTil'] <- 1
fullDataset$FoundationType[fullDataset$Foundation == 'CBlock'] <- 2
fullDataset$FoundationType[fullDataset$Foundation == 'PConc'] <- 3
fullDataset$FoundationType[fullDataset$Foundation == 'Slab'] <- 4
fullDataset$FoundationType[fullDataset$Foundation == 'Stone'] <- 5
fullDataset$FoundationType[fullDataset$Foundation == 'Wood'] <- 6
fullDataset$Foundation <- NULL
# BsmtQual
table(fullDataset$BsmtQual)
fullDataset$BasementQual[fullDataset$BsmtQual == 'Ex'] <- 1
fullDataset$BasementQual[fullDataset$BsmtQual == 'Fa'] <- 2
fullDataset$BasementQual[fullDataset$BsmtQual == 'Gd'] <- 3
fullDataset$BasementQual[fullDataset$BsmtQual == 'TA'] <- 4
fullDataset$BasementQual[fullDataset$BsmtQual == 'NA'] <- 5
fullDataset$BsmtQual <- NULL
# BsmtCond
table(fullDataset$BsmtCond)
fullDataset$BasementCond[fullDataset$BsmtCond == 'Fa'] <- 1
fullDataset$BasementCond[fullDataset$BsmtCond == 'Gd'] <- 2
fullDataset$BasementCond[fullDataset$BsmtCond == 'Po'] <- 3
fullDataset$BasementCond[fullDataset$BsmtCond == 'TA'] <- 4
fullDataset$BasementCond[fullDataset$BsmtCond == 'NA'] <- 5
fullDataset$BsmtCond <- NULL
# BsmtExposure
table(fullDataset$BsmtExposure)
fullDataset$BasementExp[fullDataset$BsmtExposure == 'Av'] <- 1
fullDataset$BasementExp[fullDataset$BsmtExposure == 'Gd'] <- 2
fullDataset$BasementExp[fullDataset$BsmtExposure == 'Mn'] <- 3
fullDataset$BasementExp[fullDataset$BsmtExposure == 'No'] <- 4
fullDataset$BasementExp[fullDataset$BsmtExposure == 'NA'] <- 5
fullDataset$BsmtExposure <- NULL
# BsmtFinType1
table(fullDataset$BsmtFinType1)
fullDataset$BasementFin1[fullDataset$BsmtFinType1 == 'NA'] <- 1
fullDataset$BasementFin1[fullDataset$BsmtFinType1 == 'ALQ'] <- 2
fullDataset$BasementFin1[fullDataset$BsmtFinType1 == 'BLQ'] <- 3
fullDataset$BasementFin1[fullDataset$BsmtFinType1 == 'GLQ'] <- 4
fullDataset$BasementFin1[fullDataset$BsmtFinType1 == 'LwQ'] <- 5
fullDataset$BasementFin1[fullDataset$BsmtFinType1 == 'Rec'] <- 6
fullDataset$BasementFin1[fullDataset$BsmtFinType1 == 'Unf'] <- 7
fullDataset$BsmtFinType1 <- NULL
# BsmtFinType2
table(fullDataset$BsmtFinType2)
fullDataset$BasementFin2[fullDataset$BsmtFinType2 == 'NA'] <- 1
fullDataset$BasementFin2[fullDataset$BsmtFinType2 == 'ALQ'] <- 2
fullDataset$BasementFin2[fullDataset$BsmtFinType2 == 'BLQ'] <- 3
fullDataset$BasementFin2[fullDataset$BsmtFinType2 == 'GLQ'] <- 4
fullDataset$BasementFin2[fullDataset$BsmtFinType2 == 'LwQ'] <- 5
fullDataset$BasementFin2[fullDataset$BsmtFinType2 == 'Rec'] <- 6
fullDataset$BasementFin2[fullDataset$BsmtFinType2 == 'Unf'] <- 7
fullDataset$BsmtFinType2 <- NULL
# Heating
table(fullDataset$Heating)
fullDataset$HeatingGAS[!fullDataset$Heating %in% c("GasA", "GasW")] <- 0
fullDataset$HeatingGAS[fullDataset$Heating %in% c("GasA", "GasW")] <- 1
fullDataset$Heating <- NULL
# HeatingQC
table(fullDataset$HeatingQC)
fullDataset$HeatQuality[fullDataset$HeatingQC == "Ex"] <- 1
fullDataset$HeatQuality[fullDataset$HeatingQC == "Gd"] <- 2
fullDataset$HeatQuality[fullDataset$HeatingQC == "TA"] <- 3
fullDataset$HeatQuality[fullDataset$HeatingQC == "Fa"] <- 4
fullDataset$HeatQuality[fullDataset$HeatingQC == "Po"] <- 5
fullDataset$HeatingQC <- NULL
# CentralAir
table(fullDataset$CentralAir)
fullDataset$AC[fullDataset$CentralAir != 'Y'] <- 0
fullDataset$AC[fullDataset$CentralAir == 'Y'] <- 1
fullDataset$CentralAir <- NULL
# Electrical
table(fullDataset$Electrical)
fullDataset$ElectricalSystem[fullDataset$Electrical == 'NA' | fullDataset$Electrical == 'SBrkr'] <- 1
fullDataset$ElectricalSystem[fullDataset$Electrical != 'NA' & fullDataset$Electrical != 'SBrkr'] <- 0
fullDataset$Electrical <- NULL
# KitchenQual
table(fullDataset$KitchenQual)
fullDataset$KitchenQuality[fullDataset$KitchenQual == 'Ex'] <- 1
fullDataset$KitchenQuality[fullDataset$KitchenQual == 'Fa'] <- 2
fullDataset$KitchenQuality[fullDataset$KitchenQual == 'Gd'] <- 3
fullDataset$KitchenQuality[fullDataset$KitchenQual == 'TA'] <- 4
fullDataset$KitchenQuality[fullDataset$KitchenQual == 'Po'] <- 5
fullDataset$KitchenQual <- NULL
# Functional
table(fullDataset$Functional)
fullDataset$FunType[fullDataset$Functional != 'Typ'] <- 0
fullDataset$FunType[fullDataset$Functional == 'Typ'] <- 1
fullDataset$Functional <- NULL
# Fireplace
table(fullDataset$FireplaceQu)
fullDataset$FirePlaceQuality[fullDataset$FireplaceQu == 'Ex'] <- 1
fullDataset$FirePlaceQuality[fullDataset$FireplaceQu == 'Fa'] <- 2
fullDataset$FirePlaceQuality[fullDataset$FireplaceQu == 'Gd'] <- 3
fullDataset$FirePlaceQuality[fullDataset$FireplaceQu == 'TA'] <- 4
fullDataset$FirePlaceQuality[fullDataset$FireplaceQu == 'Po'] <- 5
fullDataset$FirePlaceQuality[fullDataset$FireplaceQu == 'NA'] <- 6
fullDataset$FireplaceQu <- NULL
# GarrageTyoe
table(fullDataset$GarageType)
fullDataset$GarrageType[fullDataset$GarageType == '2Types'] <- 1
fullDataset$GarrageType[fullDataset$GarageType == 'Attchd'] <- 2
fullDataset$GarrageType[fullDataset$GarageType == 'Basment'] <- 3
fullDataset$GarrageType[fullDataset$GarageType == 'BuiltIn'] <- 4
fullDataset$GarrageType[fullDataset$GarageType == 'CarPort'] <- 5
fullDataset$GarrageType[fullDataset$GarageType == 'Detchd'] <- 6
fullDataset$GarrageType[fullDataset$GarageType == 'NA'] <- 7
fullDataset$GarageType <- NULL
# GarageFinish
table(fullDataset$GarageFinish)
fullDataset$GarageReady[!fullDataset$GarageFinish %in% c("Fin", "RFn")] <- 0
fullDataset$GarageReady[fullDataset$GarageFinish %in% c("Fin", "RFn")] <- 1
fullDataset$GarageFinish <- NULL
# GarageQual
table(fullDataset$GarageQual)
fullDataset$GarageQuality[fullDataset$GarageQual == 'Ex'] <- 1
fullDataset$GarageQuality[fullDataset$GarageQual == 'Fa'] <- 2
fullDataset$GarageQuality[fullDataset$GarageQual == 'Gd'] <- 3
fullDataset$GarageQuality[fullDataset$GarageQual == 'TA'] <- 4
fullDataset$GarageQuality[fullDataset$GarageQual == 'Po'] <- 5
fullDataset$GarageQuality[fullDataset$GarageQual == 'NA'] <- 6
fullDataset$GarageQual <- NULL
# GarageCond
table(fullDataset$GarageCond)
fullDataset$GarageCondition[fullDataset$GarageCond == 'Ex'] <- 1
fullDataset$GarageCondition[fullDataset$GarageCond == 'Fa'] <- 2
fullDataset$GarageCondition[fullDataset$GarageCond == 'Gd'] <- 3
fullDataset$GarageCondition[fullDataset$GarageCond == 'TA'] <- 4
fullDataset$GarageCondition[fullDataset$GarageCond == 'Po'] <- 5
fullDataset$GarageCondition[fullDataset$GarageCond == 'NA'] <- 6
fullDataset$GarageCond <- NULL
# PavedDrive
table(fullDataset$PavedDrive)
fullDataset$drivewayPaved[fullDataset$PavedDrive != 'Y'] <- 0
fullDataset$drivewayPaved[fullDataset$PavedDrive == 'Y'] <- 1
fullDataset$drivewayPaved[is.na(fullDataset$PavedDrive)] <- 0
fullDataset$PavedDrive <- NULL
# PoolQC
table(fullDataset$PoolQC)
fullDataset$PoolQuality[fullDataset$PoolQC == 'Ex'] <- 1
fullDataset$PoolQuality[fullDataset$PoolQC == 'Fa'] <- 2
fullDataset$PoolQuality[fullDataset$PoolQC == 'Gd'] <- 3
fullDataset$PoolQuality[fullDataset$PoolQC == 'TA'] <- 4
fullDataset$PoolQuality[fullDataset$PoolQC == 'NA'] <- 5
fullDataset$PoolQC <- NULL
# Fence
table(fullDataset$Fence)
fullDataset$FencePrivate[fullDataset$Fence != 'GdPrv'] <- 0
fullDataset$FencePrivate[fullDataset$Fence == 'GdPrv'] <- 1
fullDataset$Fence <- NULL
# SaleType
table(fullDataset$SaleType)
fullDataset$SaleTyp[fullDataset$SaleType == 'WD' | fullDataset$SaleType == 'CWD' | fullDataset$SaleType == 'VWD'] <- 1
fullDataset$SaleTyp[fullDataset$SaleType == 'New'] <- 2
fullDataset$SaleTyp[fullDataset$SaleType == 'COD'] <- 3
fullDataset$SaleTyp[fullDataset$SaleType == 'Con' | fullDataset$SaleType == 'ConLw' | fullDataset$SaleType == 'ConLI' | fullDataset$SaleType == 'ConLD'] <- 4
fullDataset$SaleTyp[fullDataset$SaleType == 'Oth'] <- 5
fullDataset$SaleType <- NULL
# SaleCondition
table(fullDataset$SaleCondition)
fullDataset$SaleCond[fullDataset$SaleCondition == 'Abnorml'] <- 1
fullDataset$SaleCond[fullDataset$SaleCondition == 'AdjLand'] <- 2
fullDataset$SaleCond[fullDataset$SaleCondition == 'Alloca'] <- 3
fullDataset$SaleCond[fullDataset$SaleCondition == 'Family'] <- 4
fullDataset$SaleCond[fullDataset$SaleCondition == 'Normal'] <- 5
fullDataset$SaleCond[fullDataset$SaleCondition == 'Partial'] <- 6
fullDataset$SaleCondition <- NULL

colSums(is.na(fullDataset))
fullDataset <- fullDataset %>%
  select_if(is.numeric)
fullDataset_mean <- colMeans(fullDataset, na.rm = T)
fullDataset <- replace_na(fullDataset, replace = as.list(fullDataset_mean))
colSums(is.na(fullDataset))

train <- fullDataset[1:1460,]
test <- fullDataset[1461:2919,]

library(xgboost)
set.seed(123)
################ trying out xgboost for the same data
xgboost1 = xgboost(data = as.matrix(train), nfold = 5, nrounds = 2500, nthread = 10, label = as.matrix(train$SalePrice),  verbose = FALSE, objective = "reg:linear", eval_metric = "rmse", max_depth = 8, min_child_weight = 1.5, subsample = 0.5, colsample_bytree = 0.5, gamma = 0, eta = 0.01)
pred <- predict(xgboost1, newdata = as.matrix(test))
test$SalePrice <- pred
test %>%
  select(Id, SalePrice) %>%
  write.csv(file = '/Users/venkatapochiraju/Documents/R-files/FDS/FDS/mypred_1.csv', row.names = F)
# Kaggle Score = 0.13788