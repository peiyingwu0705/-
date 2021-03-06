sample_submission = read.csv("./house_price_data/sample_submission.csv")
test <- read.csv("./house_price_data/test.csv")
train = read.csv("./house_price_data/train.csv")

dta <- read.table(file = "./house_price_data/sample_submission.csv", 
                  header = TRUE)
dta <-  read.csv("./house_price_data/test.csv")
dta <-  read.csv("./house_price_data/train.csv")

head(dta)
str(dta)
##共有81個變因,1460筆資料

summary(dta)

library(ggplot2)


##Part1:Overall Quality v.s Sale prices
ggplot(data=dta[!is.na(dta$SalePrice),], aes(x=factor(Street), y=SalePrice))+
  geom_boxplot(col='blue') + labs(x='Street') + coord_flip() +
  scale_y_continuous(breaks= seq(0, 800000, by=100000))
#以下函式計算95%信賴區間
with(dta , 
     tapply(SalePrice, Street,
            function(x) 
              c(mean(x) + c(-2, 2) * sd(x)/sqrt(length(x)))))

##用 t-test 檢驗房屋形式是否存在房價差異
#此函數會預設進行 Welch 校正，以處理兩樣本變異數不相同的問題
t.test(SalePrice ~ Street, data = dta)
#可加上參數 var.equal=TRUE 來假設變異數同值(不做Welch校正)
t.test(SalePrice ~ Street, data = dta, var.equal = TRUE)

###p>0.05,所以使用gravel pave或pave不影響房價



##Part2:House style & YearBuilt v.s Sale prices
library(Hmisc)
#排定欄位順序(order of factors)
dta$HouseStyle <- factor(dta$HouseStyle, 
                                 levels = c('1.5Fin',
                                            '1.5Unf',
                                            '1Story',
                                            '2.5Fin',
                                            '2.5Unf',
                                            '2Story',
                                            'SFoyer',
                                            'SLvl'
                                            ))

#看不同house style下的房價
tapply(dta$SalePrice, dta$HouseStyle, mean)

#同一house style下的房價平均數&信賴區間
ggplot(data = dta, 
       aes(x = HouseStyle, y = SalePrice)) +
  stat_summary(fun.data = 'mean_cl_boot', size = 1)+
  geom_hline(yintercept = mean(dta$SalePrice) , 
             linetype = 'dotted')+
  labs(x = 'HouseStyle', y = 'SalePrice') +coord_flip()
##2.5Fin的價格較高

##(m1)推測房屋形式和建造年份有關
anova(m1 <- lm(SalePrice ~ HouseStyle, data = dta))

ggplot(data = dta, 
       aes(group = HouseStyle, 
           y = SalePrice, x = YearBuilt)) +
  geom_point() +
  stat_smooth(method = 'lm', se = F) +
  stat_smooth(aes(group = HouseStyle, 
                  y = SalePrice, x = YearBuilt), 
              method = 'lm', se = F) + 
  facet_grid( . ~   HouseStyle) +
  labs(x = 'YearBuilt', y = 'SalePrice')
##p<0.05,房屋形式和建屋年份有關(可能每個階段流行不同的形式)

#利用ANOVA 檢驗假設是否正確
#(m2)把建屋年份加進模型
anova(m2 <- update(m1, . ~ . + 
                     YearBuilt, data = dta))

#(m3)扣除房屋形式以檢驗房價是否與年份關聯性較強而非房屋形式
anova(m3 <- update(m2, . ~ . - HouseStyle,  data = dta))
##從p<2.2e-16可得知年份對房價影響較大,房屋形式則較小

#將結果放在一個list中
res_lm <- lapply(list(m1, m2, m3), summary)
#比較在控制年份下house style對房價的影響
(res_lm[[2]]$r.sq - res_lm[[3]]$r.sq)/res_lm[[2]]$r.sq
anova(m3, m2)

#比較在控制房屋形式下，建造年分的效果
(res_lm[[2]]$r.sq - res_lm[[1]]$r.sq)/res_lm[[1]]$r.sq
anova(m1, m2)


##正式畫圖
require(coefplot)
m2 <- lm(SalePrice ~ HouseStyle + YearBuilt -1, data = dta)
coefplot(m2, xlab = 'estimated value', ylab = 'regression variable', title = 'reaction variable = SalePrice')

m2 <- lm(insurance$charges ~ insurance$children + insurance$age -1)
coefplot(m2, xlab = 'estimated value', ylab = 'regression variable', title = 'reaction variable = charges')




#把資料與迴歸分析的預測值、殘差與影響度放進資料

fit_m2 <- data.frame(dta[, c(17, 20, 81)], fitted = fitted(m2), resid = resid(m2),
                     infl = influence(m2)$hat)



#依數量疊合真實觀測值與預測值
ggplot(data = fit_m2, aes(x = dta$SalePrice, group = dta$HouseStyle )) +
  stat_density(geom = 'path', position = 'identity') +
  stat_density(geom = 'path', position = 'identity', aes(x = fitted)) +
  geom_vline(xintercept = c(with(dta, tapply(dta$SalePrice,dta$HouseStyle, mean))), linetype = 'dotted')+
  facet_grid(HouseStyle ~ .) +
  scale_x_continuous(breaks = seq(0, 60000, by = 10000))+
  labs(x = 'SalePrice', y = 'Probability density')



#看殘差分配,依房屋形式,檢視常態與變異數同質假設
ggplot(data = fit_m2, aes(x = scale(resid)), group = dta$HouseStyle ) +
  stat_density(geom = 'path', position = 'identity', aes(linetype = HouseStyle)) +
  scale_linetype_manual(values = 8:1) +
  guides(linetype = guide_legend(reverse = TRUE)) +
  labs(x = 'Standardized residual', y = 'Probability density') +
  theme(legend.position = c(1, .8))

#看看殘差的 Q-Q 圖，依房屋形式檢視常態假設
require(lattice)
qqmath(~ scale(resid) | HouseStyle, data = fit_m2, type = c('p', 'g', 'r'),
       xlab = 'Normal number of bits', ylab = 'Standardized residual', layout = c(3, 3),
       pch = '.', cex = 5)

#畫預測值與殘差的散佈圖,檢查線性與等分散假設
require(MASS)
ggplot(data = fit_m2, aes(x = fitted, y = scale(resid), group = HouseStyle )) +
  geom_point(pch = 20, size = 1) +
  stat_smooth(method = 'rlm', se = F) +
  facet_grid(HouseStyle ~ .) +
  labs(x = 'Mathematical prediction', y = 'Standardized residual')


#呈現影響值（影響估計結果過大的值）與標準化殘差
ggplot(data = fit_m2, aes(x = infl, y = scale(resid), group = HouseStyle)) +
  geom_text(aes(label = rownames(fit_m2)), cex = 0.6) +
  geom_hline(yintercept = 0, linetype = 'dotted') +
  facet_grid(HouseStyle ~ .) +
  labs(x = 'influence', y = 'Standardized residual')

#查看影響值
summary(influence(m2)$hat)

#Part3:分析資料集當中的其他變項是否和房價有關    
dta_SalePrice <- dta[, c('SalePrice', 'GarageArea', 
                          'YearBuilt', 'GrLivArea')]
#基本統計量
colMeans(dta_SalePrice)

#呈現兩兩散佈圖
require(heplots)  ## result = FALSE

scatterplotMatrix(~ SalePrice + GarageArea + YearBuilt + GrLivArea, data= dta_SalePrice,
                  pch = '.', cex = 2, smooth = FALSE, reg.line = FALSE, ellipse = TRUE,
                  diagonal = 'none', lower.panel = NULL)

#利用corrplot 套件，以圖形顯示相關性的大小
require(corrplot)
corrplot(cor(dta_SalePrice), method = 'ellipse', order = 'hclust', addrect = 4,
         type = 'upper', tl.pos = 'tp')

corrplot(cor(dta_SalePrice), add = TRUE, type = 'lower', method = 'number',
         order = 'hclust', col = 'black', diag = FALSE, tl.pos = 'n', cl.pos = 'n')


#放進三個解釋變項
summary(m4 <- lm(SalePrice ~ GarageArea + YearBuilt + GrLivArea, data = dta_SalePrice))


#看影響如何
require(coefplot)
coefplot(m4, predictors = c('GarageArea', 'YearBuilt', 'GrLivArea'),
         xlab = 'estimated value', ylab = 'Regression variable (remove intercept)', title = 'Reaction variable is SalePrice')

require(effects)
plot(allEffects(m4), main = '', ylim = c(30000, 500000), grid = T)

#利用 lm.beta 套件，計算標準化迴歸係數
library(lm.beta)
summary(lm.beta(m4))

#看看控制GarageArea & YearBuilt後，GrLivArea的效果
summary(m5 <- update(m4, . ~ . - GrLivArea , data = dta_SalePrice))

anova(m5, m4)

#SalePrice ~ HouseStyle + YearBuilt + GrLivArea + OverallQual
#dta[,81] ~ dta[,17] + dta[,20] + dta[,47] + dta[,18]

m5 <- lm(SalePrice ~ HouseStyle + YearBuilt + GrLivArea + OverallQual, data = dta)
fit_m5 <- data.frame(dta[, c(81, 17, 20, 47, 18)], fitted = fitted(m5), resid = resid(m5), infl = influence(m5)$hat)

ggplot(data = fit_m5, aes(x = SalePrice, group = HouseStyle )) +
  stat_density(geom = 'path', position = 'identity') +
  stat_density(geom = 'path', position = 'identity', aes(x = fitted)) +
  geom_vline(xintercept = c(with(dta, tapply(SalePrice, HouseStyle, mean))), linetype = 'dotted')+
  facet_grid(HouseStyle ~ .) +
  scale_x_continuous(breaks = seq(30000, 500000, by = 70000))+
  labs(x = 'SalePrice', y = 'Probability density')

library(psych)
pairs.panels(dta[c("YearBuilt", "GrLivArea", "OverallQual", "SalePrice")])

#Part4: YearBuilt & OverallQual vs house prices
attach(dta)
as.numeric(dta$YearBuilt)
plot(OverallQual,SalePrice,col=YearBuilt)
  

ggplot(data= dta , aes(x=OverallQual, y=SalePrice, colour =YearBuilt))+
  geom_point()















