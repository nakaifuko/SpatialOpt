# Land use estimation

#install.packages("makedummies")
#install.packages("caret")

# package
library(caret)
library(ggplot2)
library("makedummies")

setwd("/Users/fukonakai/Documents/GitHub/optLanduse/")
data <- read.csv("LandPrice/in/Toyo_SocioEconomicData.csv", fileEncoding = 'utf-8')
colnames(data)[colnames(data) == "メッシ."] <- "ID"
original <- read.csv('Model/in/Toyohashi_area_x0_original.csv')

# Estimating model using land price data
# current land use
data$A29_005[which(data$A29_005=="")]<- NA

Uclass <- vector(length = length(data$people))
Uclass[which(data$Chosei==3)]<-3
Uclass[which(data$Shigaika==2)]<-2
Uclass[which(Uclass==0)]<-NA
Uclass = as.character(Uclass)
data2 <- cbind(data,Uclass)

#dummy https://markezine.jp/article/detail/20790
tmp <- dummyVars(~., data=data2) # please make sure '~.' is not '-.'
data2.dummy <- as.data.frame(predict(tmp, data2))

#工業，商業，住居まとめ
kogyo <- vector(length = length(data$people))
kogyo <- data2.dummy$A29_005工業専用地域+
  data2.dummy$A29_005工業地域+
  data2.dummy$A29_005準工業地域
#kogyo[is.na(kogyo)]<-0

shogyo  <- vector(length = length(data$people))
shogyo <- data2.dummy$A29_005近隣商業地域+
  data2.dummy$A29_005商業地域
#shogyo[is.na(shogyo)]<-0

jukyo <- vector(length = length(data$people))
jukyo  <- data2.dummy$A29_005準住居地域+
  data2.dummy$A29_005第一種住居地域+
  data2.dummy$A29_005第一種中高層住居専用地域+
  data2.dummy$A29_005第一種低層住居専用地域+
  data2.dummy$A29_005第二種住居地域+
  data2.dummy$A29_005第二種中高層住居専用地域+
  data2.dummy$A29_005第二種低層住居専用地域
#jukyo[is.na(jukyo)]<-0


tdata <- cbind(kogyo,shogyo,jukyo)
tdata2 <- as.data.frame(tdata)
colnames(tdata2) <- c("kogyo","shogyo","jukyo")
data3.dummy <- cbind(data2.dummy,tdata2)
data3.dummy$emRoad[is.na(data3.dummy$emRoad)]<-0

cordata3<- data3.dummy[,c("LandPrice",
                          "kogyo",
                          "shogyo",
                          "jukyo",
                          "emRoad",
                          "distance")]

cordata3_no_na <- na.omit(cordata3)
cor(cordata3_no_na)

plot(data3.dummy$LandPrice,data3.dummy$distance)

# regression model using all data
modelA.lm <- lm(log(LandPrice)~kogyo
               + shogyo
               + jukyo
               + emRoad
               + distance
               ,data = data3.dummy)

# regression model using all data
modelB.lm <- lm(log(LandPrice)~kogyo
                + shogyo
                + distance,
                data = data3.dummy)

summary(modelA.lm)
summary(modelB.lm)

#予測
LandPriceEstimate <- function(KogyoDummy, ShogyoDummy, JukyoDummy, emRoad, Distance) {
  i <- (modelA.lm$coefficients[1])#*
  x2 <- (modelA.lm$coefficients[2]* KogyoDummy) #*
  x3 <-  (modelA.lm$coefficients[3]* ShogyoDummy)#*
  x4 <- (modelA.lm$coefficients[4]* JukyoDummy)
  x5 <- (modelA.lm$coefficients[5]* emRoad)
  x6 <- (modelA.lm$coefficients[6]* Distance)#*
  y3 = i  + x2 + x3 + x6
  return(exp(y3))  # 結果を返す
}

# Estimate input
d3_no_na <- na.omit(data3.dummy[c('ID','LandPrice','Uclass2','Uclass3','emRoad','distance','kogyo','shogyo','jukyo')])
est_org <- LandPriceEstimate(d3_no_na$kogyo,
                              d3_no_na$shogyo,
                              d3_no_na$jukyo,
                              d3_no_na$emRoad,
                              d3_no_na$distance)

plot(est_org,d3_no_na$LandPrice, 
     xlim = c(0,200000), 
     ylim = c(0,200000),
     main = 'Data vs Predicted land price')

n <- length(d3_no_na$jukyo)
newVec1 <- rep(1, n)
newVec0 <- rep(0, n)

if_kogyo <- LandPriceEstimate(newVec1,
                              newVec0,
                              newVec0,
                              d3_no_na$emRoad,
                              d3_no_na$distance)

if_shogyo<- LandPriceEstimate(newVec0,
                              newVec1,
                              newVec0,
                              d3_no_na$emRoad,
                              d3_no_na$distance)

if_jukyo <- LandPriceEstimate(newVec0,
                              newVec0,
                              newVec1,
                              d3_no_na$emRoad,
                              d3_no_na$distance)

if_none <- LandPriceEstimate(newVec0,
                              newVec0,
                              newVec0,
                              d3_no_na$emRoad,
                              d3_no_na$distance)

noNA <- cbind(d3_no_na,est_org,if_jukyo,if_shogyo,if_kogyo,if_none)

merged_data <- merge(noNA,original, key = 'ID')

merged_data_selected <- merged_data[c('ID','distance','emRoad','LandPrice','NL','NS','NM','if_jukyo','if_shogyo','if_kogyo','if_none')]

original_noNA <- (merged_data_selected$NL*merged_data_selected$if_jukyo
  + merged_data_selected$NS*merged_data_selected$if_shogyo
  + merged_data_selected$NM*merged_data_selected$if_kogyo
  + 10000*merged_data_selected$if_none)

estimated_chika_noNA <- cbind(merged_data_selected,original_noNA)
write.csv(estimated_chika_noNA, "LandPrice/out/estimated_chika_noNA.csv")

# Expand the estimation to all grids
merged_data_all <- merge(data3.dummy,original, key = 'ID')

n <- length(merged_data_all$emRoad)
newVec1 <- rep(1, n)
newVec0 <- rep(0, n)

if_jukyo <- LandPriceEstimate(newVec0,
                                  newVec0,
                                  newVec1,
                                  merged_data_all$emRoad,
                                  merged_data_all$distance)

if_shogyo <- LandPriceEstimate(newVec0,
                                  newVec1,
                                  newVec0,
                                  merged_data_all$emRoad,
                                  merged_data_all$distance)

if_kogyo <- LandPriceEstimate(newVec1,
                                  newVec0,
                                  newVec0,
                                  merged_data_all$emRoad,
                                  merged_data_all$distance)

if_none <- LandPriceEstimate(newVec0,
                              newVec0,
                              newVec0,
                              merged_data_all$emRoad,
                              merged_data_all$distance)

merged_data_all <- cbind(merged_data_all,if_jukyo,if_shogyo,if_kogyo,if_none)

merged_data_selected_all <- merged_data_all[c('ID','distance','emRoad','LandPrice','NL','NS','NM','if_jukyo','if_shogyo','if_kogyo','if_none')]

original <- (merged_data_selected_all$NL*merged_data_selected_all$if_jukyo
                + merged_data_selected_all$NS*merged_data_selected_all$if_shogyo
                + merged_data_selected_all$NM*merged_data_selected_all$if_kogyo
                + 10000*merged_data_selected_all$if_none)

estimated_chika <- cbind(merged_data_selected_all,original)
write.csv(estimated_chika[c('ID','original','if_jukyo','if_shogyo','if_kogyo')], "LandPrice/out/estimated_chika.csv")

#Output
#https://rpubs.com/kurogomayaki/benkyoukai-2019-02-28-ch10
#install.packages("texreg") 
library(texreg)
screenreg(list(modelA.lm))

