library(dplyr) 
library(ggplot2) 
library(cowplot)
library(KernSmooth)
library(leaps)
library(GGally)
library(rpart)
library(randomForest)
library(ggdendro)
#library(instaR)
library(mclust)
library(caret)

#### Setup
#https://towardsdatascience.com/multi-class-classification-in-text-using-r-e6cf72ef1da3
#https://medium.com/@ODSC/build-a-multi-class-support-vector-machine-in-r-abcdd4b7dab6
forrest<-read.csv("/Users/michaelremington/Python Tutorials/kaggellogisticmodel2.csv")
forrest1<- forrest %>% mutate(Wilderness.Label=as.factor(Wilderness.Label),
                              Soil.Label=as.factor(Soil.Label),
                              Tree.Type=as.factor(Tree.Type))
set.seed(123)
row <- nrow(forrest1)
coll <- ncol(forrest1)
numTrain <- floor((2/3) * row)
numTest <- row - numTrain
training <- forrest1[sample(row, numTrain), ]
test <- forrest[sample(row, numTest), ]


heatmap<- forrest1 %>% group_by(Soil.Label, Tree.Type) %>% tally()
###### EDA 
gg<-ggpairs(forrest[1:9],columnLabels = c("Elevation",
                                          "Aspect",
                                          "Slope",
                                          "xToHydrology",
                                          "yToHydrology",
                                          "xToRoad",
                                          "Shade9am",
                                          "Shade3pm",
                                          "xToFire"))
gg + theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("ggpairsdataExploration.JPEG",width =12, height = 3)
p <- ggplot(heatmap, aes(Tree.Type,Soil.Label)) + geom_tile(aes(fill = n),
      colour = "white") + scale_fill_gradient(low = "white", high = "red")+
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            axis.text.y = element_text(size = 10, hjust = 1))
p + ggtitle('Soil Type Analysis')
ggsave("soiltypea.png")

####### Model Building

fit2 <- randomForest(Tree.Type~., data = training,mtry=6, proximity=TRUE)

fit2
importance(fit2)

var <- c('Elevation',
         'Soil Label',
         'Horizontal Distance To Roadways',
         'Horizontal Distance To Fire Points',
         'Wilderness Label',
         'Horizontal Distance To Hydrology',
         'Aspect',
         'Hillshade 9am',
         'Vertical Distance To Hydrology',
         'Hillshade 3pm',
         'Slope')
MeanDecreaseGini<-c('2332.6126',
                    '1466.4281',
                    '836.1174',
                    '674.8890',
                    '605.1261',
                    '558.8663',
                    '495.8722',
                    '482.4062',
                    '462.5681',
                    '405.9774',
                    '315.2771')
MeanDecreaseGini<- as.integer(MeanDecreaseGini)
imp<-data.frame(var,MeanDecreaseGini)

impg<-ggplot(data=imp, aes(x=reorder(var, -MeanDecreaseGini), y=MeanDecreaseGini)) +
      geom_bar(stat="identity", fill="steelblue")+
      geom_text(aes(label=MeanDecreaseGini), vjust=1.6, color="white", size=3.5)+
      xlab('Variable')+
      ylab('Mean Decreasing Gini')+
      ggtitle('Variable Importance Measure')+
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
impg
ggsave("importance1.png")

##### Accuracy and Model Tuning
oob.error.data <- data.frame(
  Trees=rep(1:nrow(fit2$err.rate), times=8),
  Type=rep(c("OOB",
             "Aspen",
             "Cottonwood/Willow",
             "Douglas-fir",
             "Krummholz",
             "Lodgepole Pine",
             "Ponderosa Pine",
             "Spruce/Fir"), each=nrow(fit2$err.rate)),
  Error=c(fit2$err.rate[,"OOB"],
          fit2$err.rate[,"Aspen"],
          fit2$err.rate[,"Cottonwood/Willow"],
          fit2$err.rate[,"Douglas-fir"],
          fit2$err.rate[,"Krummholz"],
          fit2$err.rate[,"Lodgepole Pine"],
          fit2$err.rate[,"Ponderosa Pine"],
          fit2$err.rate[,"Spruce/Fir"]))
error_rate<-ggplot(data=oob.error.data, aes(x=Trees, y=Error)) +
            geom_line(aes(color=Type))+
            ggtitle('Out Of Bag Error Analysis')
error_rate
ggsave("oob_error_rate_500_trees.png")
oob.values <- vector(length=10)
for(i in 1:10) {
  temp.model <- randomForest(Tree.Type~., data = training)
  oob.values[i] <- temp.model$err.rate[nrow(temp.model$err.rate),1]
}
oob.values
# [1] 0.1392857 0.1411706 0.1395833 0.1385913 0.1383929 0.1372024 0.1382937 0.1393849 0.1395833
#[10] 0.1372024
forrest.pred <- predict(fit2, test)

con <- cbind(test,forrest.pred)
pos_neg <-  ifelse(con$Tree.Type == con$forrest.pred, "positive_class", "negative_class") %>% as.factor()
con <- cbind(con,pos_neg)
t <- con %>% group_by(Tree.Type,pos_neg) %>% tally()
t2 <- t %>% group_by(Tree.Type) %>%  mutate(type_count = sum(n),
                                            acc = round((n/sum(n)),digits = 2)*100) %>% filter(pos_neg == 'positive_class') %>% select(Tree.Type,acc)
accuracygraph<-ggplot(data=t2, aes(x=reorder(Tree.Type, -acc), y=acc)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=acc), vjust=1.6, color="white", size=3.5)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  xlab('Tree Type') +
  ylab('Prediction Percent Accuaracy')+
  ggtitle('Prediction Analysis By Tree Type')
accuracygraph
ggsave("ModelAccuarcy.png")

fit2
  
accheatmap<- con %>% group_by(forrest.pred, Tree.Type) %>% tally()
p2 <- ggplot(accheatmap, aes(forrest.pred,Tree.Type)) + geom_tile(aes(fill = n),
                                                            colour = "white") + scale_fill_gradient(low = "white", high = "blue")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12, hjust = 1))
p2 + ggtitle('Accuaracy Analysis') + xlab('Predicted Type') +ylab('Actual Type')+  geom_text(aes(label=n), size=6)
ggsave("accheatmap.png")
#1 negative_class   235 4.7 % inaccuarcy
#2 positive_class  4805 95.3 % accuarcy
#%>% group_by(pos_neg)

4805


#############################
##
####### fit 1 variable reduction
##
##############################
#Hillshade_3pm+
#  Hillshade_9am+

fit <- randomForest(Tree.Type~
                      Elevation+
                      Soil.Label+
                      Horizontal_Distance_To_Roadways+ 
                      Aspect+
                      Slope+
                      Vertical_Distance_To_Hydrology+
                      Horizontal_Distance_To_Hydrology, data = training,proximity=TRUE)
fit

i<-importance(fit)
i
var2 <- c('Elevation',
         'Soil Label',
         'Horizontal Distance To Roadways',
         'Aspect',
         'Horizontal Distance To Hydrology',
         'Vertical Distance To Hydrology',
         'Slope')
MeanDecreaseGini2<-c('2823',
                    '1823',
                    '1130',
                    '857',
                    '742',
                    '650',
                    '540')
MeanDecreaseGini2<- as.integer(MeanDecreaseGini2)
imp2<-data.frame(var2,MeanDecreaseGini2)

impg2<-ggplot(data=imp2, aes(x=reorder(var2, -MeanDecreaseGini2), y=MeanDecreaseGini2)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=MeanDecreaseGini2), vjust=1.6, color="white", size=3.5)+
  theme_minimal()+
  xlab('Variable')+
  ylab('Mean Decreasing Gini')+
  ggtitle('Variable Importance Measure Model #2')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  xlab("Variable")
plot_grid(impg,impg2)

ggsave("imp2.png")

forrest.pred2 <- predict(fit, test)

con2 <- cbind(test,forrest.pred2)
pos_neg2 <-  ifelse(con2$Tree.Type == con2$forrest.pred2, "positive_class", "negative_class") %>% as.factor()
con2 <- cbind(con2,pos_neg2)
tnew <- con2 %>% group_by(Tree.Type,pos_neg2) %>% tally()
t2new <- tnew %>% group_by(Tree.Type) %>%  mutate(type_count = sum(n),
                                            acc = round((n/sum(n))*100), digits = 2) %>% filter(pos_neg2 == 'positive_class') %>% select(Tree.Type,acc)
con2 %>% group_by(pos_neg2) %>% tally()
accuracygraph2<-ggplot(data=t2new, aes(x=reorder(Tree.Type, -acc), y=acc)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=acc), vjust=1.6, color="white", size=3.5)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  xlab('Tree Type') +
  ylab('Prediction Precent Accuaracy')+
  ggtitle('Second Prediction Analysis')
accuracygraph2
#94.6%
plot_grid(accuracygraph,accuracygraph2)
ggsave("accuarcyGraph2.png")




