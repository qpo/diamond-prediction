data = read.csv("Victorious_secrets_diamonds_score.csv", header=T)
data = read.csv("diamonds_build.csv", header=T)

m1 = lm(Listing ~ Carats + Color + Clarity + Cut + Depth + Table + Polish + Symmetry + Fluroescence + Region + Wholesaler_Size)
m2 = lm(Listing ~ Carats + Color + Clarity + Depth + Table + Fluroescence, data=data3)
m3 = lm(Listing ~ Carats + Color + Clarity + Fluroescence, data=data3)
summary(m3)

anova(m2, m3)

data2 = data1[which(sapply(X=data1$Color, FUN=nchar)==1),]
detach(data2)
attach(data2)

data3 =data1
data3$Color = as.factor(strtrim(data3$Color,1))
data3$Fluroescence  = as.factor(strtrim(data3$Fluroescence,1))

## MODEL 1

m3 = lm(Listing ~ Carats + Color + Clarity + Fluroescence, data=data)
summary(m3)





## MODEL 2

m3 = lm(Listing ~ Carats + Color + Fluroescence + Carats:Color + Carats:Clarity, data=train.set)
m4 = lm(Listing ~ Carats + Color + Fluroescence + Clarity + Carats:Color + Carats:Clarity, data=train.set)
summary(m4)
anova(m3, m4)

train.set = data[2000:7513,]
test.set = data[1:1999,]
test.set = test.set[-which(test.set$Color=='S'),]
test.set = test.set[-which(test.set$Color=='O'),]
test.set$Clarity[test.set$Clarity=='None']=NA

pred = predict(m3, newdata=test.set)
error= pred - test.set$Listing
mean(error, na.rm = T)
summary(error)
sd(error, na.rm = T)

## MODEL FOR LISTING->RETAIL

mc = lm(Retail ~ Listing, data=data)

m4 = lm(Listing ~ Carats + Color + Fluroescence + Clarity + Carats:Color + Carats:Clarity, data=data)

pred = read.csv("Victorious_secrets_diamonds_score.csv", header=T)
pred = pred[which(sapply(X=as.character(pred$Color), FUN=nchar)==1),]
pred = pred[-which(pred$Color=='W'),]
pred = pred[-which(pred$Color=='S'),]
pred = pred[-which(pred$Color=='Q'),]
pred$Fluroescence  = as.factor(strtrim(pred$Fluroescence,1))

offers = predict(m3, newdata=pred)
retail.predicted = predict(mc, data.frame(Listing=offers))

pred$Offer = offers
pred$Retail_pred = retail.predicted

write.csv(pred, "output.csv")

############3
hist(orig, xlim=c(-50000, 300000), ylim=c(0, 3000))
hist(data$Listing, add=T, border ='red', xlim=c(-50000, 300000), ylim=c(0, 3000))

mean(data$Listing-orig)

