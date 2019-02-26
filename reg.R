data <- read.csv("C:/Users/Ivy Lo/Desktop/TEMP/MSDtaste_new.csv")
dim(data)
head(data, 5)
colnames(data)[1] <- "ID"
##
str(data)
summary(data)
attach(data)
ID <- as.factor(ID)
albumID <- as.factor(albumID)
mode <- as.factor(mode)
unique(albumID) # 2730 levels
unique(songID) # 3922 levels
##
data_select <- data[, c(3, 7:15)]
##
par(mfrow = c(2, 3))
boxplot(log(duration)~mode, data,
        boxwex = 0.5, names = c("小音階", "大音階"),
        col = "#336699", border = "#264d73", ylim = c(2, 8) ,
        main = "音階與歌曲長度盒型圖",
        ylab = "log(歌曲長度)")

boxplot(keySignature~mode, data,
        boxwex = 0.5, names = c("小音階", "大音階"),
        col = "#336699", border = "#264d73", ylim = c(0, 11) ,
        main = "歌曲音階與音調盒形圖",
        ylab = "歌曲音調")
boxplot(energy~mode, data,
        boxwex = 0.5, names = c("小音階", "大音階"),
        col = "#336699", border = "#264d73",
        main = "歌曲音階與火爆程度盒形圖",
        ylab = "歌曲火爆程度")
boxplot(tempo~mode, data,
        boxwex = 0.5, names = c("小音階", "大音階"),
        col = "#336699", border = "#264d73",
        main = "歌曲音階與速度盒形圖",
        ylab = "歌曲速度")
boxplot(log(count)~mode, data,
        boxwex = 0.5, names = c("小音階", "大音階"),
        col = "#336699", border = "#264d73", 
        main = "歌曲音階與點閱盒形圖",
        ylab = "log(歌曲點閱率)")
counts <- table(mode, year)
percentage <- apply(counts, 2, function(x){x*1/sum(x)})
barplot(percentage[, 46:56], main = "近十年歌曲大小音階比例變化",
        xlab = "", ylab = "", ylim = c(0, 1), 
        col = c("#336699","#cc3300"),
        legend = c("小音階", "大音階"), beside = F)
##
# plot(mode, duration, col = "#ff9900", outlier = "n")
# boxplot(duration~mode, data,
#         boxwex = 0.5, names = c("小音階", "大音階"),
#         col = "#336699", border = "#264d73", ylim = c(10, 1500) ,
#         main = "音階與歌曲長度盒型圖",
#         ylab = "歌曲長度(秒)")
##

panel.pearson <- function(x, y, ...) {
  horizontal <- (par("usr")[1] + par("usr")[2]) / 2; 
  vertical <- (par("usr")[3] + par("usr")[4]) / 2; 
  text(horizontal, vertical, format(abs(cor(x,y)), digits=2), 
       cex = 1.8, col = "#336699") 
}
pairs(data_select[, -1], upper.panel = panel.pearson, pch = 19,  cex = 0.8, col = "#336699")
pairs(data_select, upper.panel = panel.pearson, pch = 19,  cex = 0.8, col = "#336699")

library(corrplot)
mycor <- cor(data_select[, -1])
corrplot(mycor, tl.col = "black")

# t-test
data_mode0 <- data[data$mode == 0,]
data_mode1 <- data[data$mode == 1,]

shapiro.test(duration) # not normal but n = 3991
hist(duration)
t.test(data_mode0$duration, data_mode1$duration) # reject

shapiro.test(keySignature) # not normal but n = 3991
hist(keySignature)
t.test(data_mode0$keySignature, data_mode1$keySignature) # reject

shapiro.test(energy) # not normal but n = 3991
hist(energy)
t.test(data_mode0$energy, data_mode1$energy) # not reject

shapiro.test(tempo) # not normal but n = 3991
hist(tempo)
t.test(data_mode0$tempo, data_mode1$tempo) # not reject

shapiro.test(count) # not normal but n = 3991
hist(count)
t.test(data_mode0$count, data_mode1$count) # not reject

model <- glm(mode ~ duration + keySignature + energy + tempo + count, family = "binomial")
summary(model) # same with t test
model_mode <- glm(mode ~ duration + keySignature, family = "binomial")
print(model_mode)
summary(model_mode)

# lm
par(mfrow = c(2, 2))
apply(is.na(data_select), 2, sum) # energy gots na
plot(energy, col = alpha("#336699", 0.5), pch = 20,
     main = "歌曲火爆程度")
summary(energy)
plot(data_select[is.na(energy), 1], col = alpha("#336699", 0.5), pch = 20,
     xlab = "", ylab = "count", main = "遺失歌曲火爆值的歌曲播放次數")
summary(data_select[is.na(energy), 1])
lm_energy <- lm(energy~., data_select)
pred_energy <- predict(lm_energy, data_select[, -5])
data_select$energy[is.na(energy)] <- pred_energy[is.na(energy)]

summary(data_select)
str(data_select)


plot(data_select$energy[is.na(energy)], pch = 20,
     xlab = "", ylab = "energy_imputation", 
     main = "歌曲火爆程度回歸補值",
     col = alpha("#336699", 0.5))
plot(data_select$energy,  pch = 20,   
     xlab = "", ylab = "energy", 
     main = "補值後的歌曲火爆程度",
     col = alpha("#336699", 0.5))
data_select[, c(2,7,8,10)] <- data.frame(apply(data_select[, c(2,7,8,10)], 2, scale))
# dim(data_lm)
# lm <- lm(count ~ ., data_select) # count not normal
# print(lm)
# count_log <- log(count)
# shapiro.test(count_log) # count_log not normal
# hist(count_log)
model_p <- glm(count ~ ., data_select, family = "poisson")
print(model_p)
summary(model_p)
library(coefplot)
coefplot(model_p, predictors = colnames(data_select)[-1])
# 檢查共線性
library(car)
vif(model_p) # vif < 10 無共線性
model_p2 <- glm(count ~ . + mode*duration + mode*keySignature , data_select, family = "poisson")
print(model_p2)
summary(model_p2)
coefplot(model_p2, 
         predictors = c(colnames(data_select)[-1], "duration:mode", "keySignature:mode"))
vif(model_p2)
# 檢查過度離散
# ri = (count - model_p$fitted.values)/sqrt()
# plot(ri, col = alpha("#336699", 0.5), pch = 16)
# quasipoisson
model_p3 <- glm(count ~ . + mode*duration + mode*keySignature , data_select, family = "quasipoisson")
print(model_p3)
summary(model_p3)
model_p4 <- glm(count ~ popularity + energy + year + mode*duration, data_select, family = "quasipoisson")
summary(model_p4)
coefplot(model_p4,
         predictors = names(model_p4$coefficients[-1]))
vif(model_p4) 
exp(coef(model_p4))




