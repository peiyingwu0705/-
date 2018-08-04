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
t.test(c ~ Street, data = dta)
#可加上參數 var.equal=TRUE 來假設變異數同值(不做Welch校正)
t.test(SalePrice ~ Street, data = dta, var.equal = TRUE)

###p>0.05,所以使用gravel pave或pave不影響房價



##Part2:House style & YearBuilt v.s Sale prices
library(Hmisc)
#排定欄位順序(order of factors)
dta$HouseStyle <- factor(dta$HouseStyle, 
                                 levels = c('1Story',
                                            '2Story',
                                            '1.5Fin',
                                            'SLvl', 
                                            'SFoyer',
                                            '2.5Unf',
                                            'NA'))

#看不同house style下的房價
tapply(dta$SalePrice, dta$HouseStyle, mean)

#同一house style下的房價平均數&信賴區間
ggplot(data = dta, 
       aes(x = HouseStyle, y = SalePrice)) +
  stat_summary(fun.data = 'mean_cl_boot', size = 1)+
  geom_hline(yintercept = mean(dta$SalePrice) , 
             linetype = 'dotted')+
  labs(x = 'HouseStyle', y = 'SalePrice') +coord_flip()
##2Story的價格較高

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



require(coefplot)
m2 <- lm(SalePrice ~ HouseStyle + YearBuilt -1)
coefplot(m2, xlab = 'estimated value', ylab = 'regression variable', title = 'reaction variable = SalePrice')

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















