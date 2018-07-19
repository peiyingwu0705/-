#Bar Graph


ggplot(data=nasa, aes(x = pressure))+geom_bar(fill ="blue",colour="blue")


data("iris")
iris

ggplot(data = iris, aes(x=Petal.Length, y=Petal.Width))+geom_bar(stat = "identity", fill="purple")

#

data("Animals")

ggplot(data=Animals, aes(x=body, y=brain, colour =body))+geom_point()


ggplot(data=iris, aes(x=Petal.Length, y=Petal.Width, colour =Species))+geom_point()

#Histogram

data("cats")

ggplot(data =cats, aes(x=Bwt, fill= Sex))+geom_histogram()
ggplot(data =cats, aes(x=Bwt, y=Hwt, fill= Sex))+geom_histogram(stat="identity")

#Boxplot
library(ggplot2)
library(MASS)
ggplot(data=Cars93, aes(x=Manufacturer, y=Price))+geom_boxplot(fill="orange", colour="gray")


#多變量

library(ggplot2)
library(GGally)
library(scales)

data("nasa")
nasa<-as.data.frame(nasa)
ggpairs(nasa,lower= list(continuous = wrap("points", shape = I('.'))),
        upper = list(combo = wrap("box", outlier.shape = I('.'))))

#ex
library(ggplot2)
library(GGally)
library(scales)
ggpairs(mtcars,lower= list(continuous = wrap("points", shape = I('.'))),
        upper = list(combo = wrap("box", outlier.shape = I('.'))))


diamond_samp <- diamonds[sample(1:length(diamonds$price), 10000), ]
ggpairs(diamond_samp,lower= list(continuous = wrap("points", shape = I('.'))),
        upper = list(combo = wrap("box", outlier.shape = I('.'))))
