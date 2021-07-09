#multinomial
unique(menu_total.subset$Food_Category) #12 different categories of food type
#reduce number of categories into something more manageable
levels(menu_total.subset$Food_Category)[match(c("Desserts", "Baked Goods" ),
                              levels(menu_total.subset$Food_Category))] <- "Desserts"
levels(menu_total.subset$Food_Category)[match(c("Fried Potatoes", "Pizza", "Burgers"),
                                              levels(menu_total.subset$Food_Category))] <- "Fast Food"
levels(menu_total.subset$Food_Category)[match(c("Toppings & Ingredients", "Soup", "Salads"),
                                              levels(menu_total.subset$Food_Category))] <- "Soups & Salads"
menu_total.subset1 <- subset(x= menu_total.subset, select= c(Restaurant, Item_Name, Calories, Food_Category, Total_Fat,
                                                             Sodium, Carbohydrates, Protein) )
menu_total.subset1.na <- na.omit(menu_total.subset1) #138087 entries
### remove extremely large values
menu_total.subset1.na.2 <- subset(x= menu_total.subset1.na, Calories <2500 & Total_Fat < 80 & Sodium <3500
                                  & Carbohydrates <500 & Protein <100) #134003 entries

library(dplyr)
library(ggplot2)
igp <- mutate(menu_total.subset1.na.2, fat_range = cut_number(Total_Fat,4)) %>% group_by(fat_range, Food_Category) %>% summarise(count=n()) %>%  group_by(fat_range) %>% mutate(etotal=sum(count), proportion = count/etotal)
ggplot(igp, aes(x= fat_range, y=proportion, group=Food_Category, linetype= Food_Category)) +
  geom_line(aes(linetype=Food_Category, color=Food_Category))+
  geom_point(aes(color=Food_Category))+
  theme(legend.position="top")
igp2 <- mutate(menu_total.subset1.na.2, carb_range = cut_number(Carbohydrates,4)) %>% group_by(carb_range, Food_Category) %>% summarise(count=n()) %>%  group_by(carb_range) %>% mutate(etotal=sum(count), proportion = count/etotal)
ggplot(igp2, aes(x= carb_range, y=proportion, group=Food_Category, linetype= Food_Category)) +
  geom_line(aes(linetype=Food_Category, color=Food_Category))+
  geom_point(aes(color=Food_Category))+
  theme(legend.position="top")
igp3 <- mutate(menu_total.subset1.na.2, sod_range = cut_number(Sodium,4)) %>% group_by(sod_range, Food_Category) %>% summarise(count=n()) %>%  group_by(sod_range) %>% mutate(etotal=sum(count), proportion = count/etotal)
ggplot(igp3, aes(x= sod_range, y=proportion, group=Food_Category, linetype= Food_Category)) +
  geom_line(aes(linetype=Food_Category, color=Food_Category))+
  geom_point(aes(color=Food_Category))+
  theme(legend.position="top")
igp4 <- mutate(menu_total.subset1.na.2, protein_range = cut_number(Protein,4)) %>% group_by(protein_range, Food_Category) %>% summarise(count=n()) %>%  group_by(protein_range) %>% mutate(etotal=sum(count), proportion = count/etotal)
ggplot(igp4, aes(x= protein_range, y=proportion, group=Food_Category, linetype= Food_Category)) +
  geom_line(aes(linetype=Food_Category, color=Food_Category))+
  geom_point(aes(color=Food_Category))+
  theme(legend.position="top")

library(nnet)
mmod <- multinom(Food_Category ~Calories+ Total_Fat + Sodium + Carbohydrates +Protein, data= menu_total.subset1.na.2)
mmod.reduced <- step(mmod, trace = 1) #No variable should be subtracted from saturated model
summary(mmod.reduced)
calorie_levels <- 0:2500
df.preds <- data.frame(Calories= calorie_levels, 
                      Total_Fat=rep(mean(menu_total.subset1.na.2$Total_Fat)),
                      Sodium=rep(mean(menu_total.subset1.na.2$Sodium)),
                      Carbohydrates= rep(mean(menu_total.subset1.na.2$Carbohydrates)),
                     Protein= rep(mean(menu_total.subset1.na.2$Protein)
                     ))
preds <- data.frame(Calories= calorie_levels, predict(mmod.reduced, newdata=df.preds, type="probs"))
library(tidyr)
lpred <- gather(preds, Food_Category, probability,-Calories)
ggplot(lpred, aes(x=Calories, y=probability, group=Food_Category, linetype=Food_Category)) +
  geom_line(aes(linetype=Food_Category, color=Food_Category))+
  geom_point(aes(color=Food_Category))+
  theme(legend.position="top")
xtabs(~ predict(mmod.reduced) + menu_total.subset1.na.2$Food_Category) #55% success rate

#summary(mmod.reduced) ---- baseline: Appetizers
#logit coefficients are returned
#In other words, if your calories increase by one unit, your chances of staying in the appetizers category are 
#higher compared to staying in the Soup and Salads
#In english: As calories increase, there is a greater chance of you eating appetizers  than
# for Salads 
#likewise: If carbs increase by one unit, there is a greater chance that both beverages and desserts would more
# lkely to be eaten
# than for the appetizer group.
#Another: If fat increase by one unit, there is a greater chance you'll eat salads than for the
# appetizers 

#train and test settings
set.seed(1)
train.rows <- sample(1:nrow(menu_total.subset1.na), 0.8*nrow(menu_total.subset1.na))
train.multimod <- menu_total.subset1.na[train.rows,]
test.multimod <-  menu_total.subset1.na[-train.rows,]
mmod2 <- multinom(Food_Category ~Calories+ Total_Fat + Sodium + Carbohydrates +Protein, 
                  data= train.multimod)
pred.mmod2 <- predict(mmod2, test.multimod, "probs")
pred.mmod2.class <- predict(mmod2, test.multimod)
table(pred.mmod2.class, test.multimod$Food_Category)
#(45+659+7715+108+1368+4232+890)/nrow(test.multimod)
#[1] 0.5437396


#logistic
menu_total.subset2 <- subset(x=menu_total.subset, select=c(Restaurant, Item_Name, Food_Category, Serving_Size,
                                                           Calories, Total_Fat, Carbohydrates,
                                                           Sodium, Protein, Kids_Meal ))
menu_total.subset2.na <- na.omit(menu_total.subset2) #77,194 entries
#get rid of extreme values
menu_total.subset2.na.1 <- subset(menu_total.subset2.na, Serving_Size < 900 & Calories <2500 & Total_Fat < 80 & Sodium <3500
                                  & Carbohydrates <500 & Protein <100) #75,936 entries
lmod <- glm(Kids_Meal ~ Food_Category + Serving_Size + Calories +Total_Fat+ Carbohydrates + Sodium + Protein, 
            family=binomial, menu_total.subset2.na.1)
#There is no need to transform since there is normality assumption
lmod.reduced <- step(lmod)
summary(lmod.reduced) #no need to reduce saturated model
linpred <- predict(lmod.reduced)
predprob <- predict(lmod.reduced, type = "response")
menu_total.subset2.na.2 <- mutate(menu_total.subset2.na.1, predout= ifelse(predprob <0.5, "no", "yes"))
xtabs(~ Kids_Meal + predout, menu_total.subset2.na.2) #(72155+1)/(72155+1+2+3778) = 0.9502212

#------train/testing
train.logistic <- (menu_total.subset2.na.1$Calories >600)
test.logistic <- menu_total.subset2.na.1[!train.logistic,]
Kids_Status <- menu_total.subset2.na.1$Kids_Meal[!train.logistic]
dim(test.logistic) #66092   10 ----- about 87/13 split
glm.log <- glm( Kids_Meal~ Food_Category + Serving_Size + Calories +Total_Fat+ Carbohydrates + Sodium + Protein, 
                family=binomial, data= menu_total.subset2.na.1, subset=train.logistic)
glm.log.probs <- predict(glm.log, test.logistic, type="response")
contrasts(menu_total.subset2.na.1$Kids_Meal)
glm.log.pred <- rep("Kids Meal:Yes",66092 )
glm.log.pred[glm.log.probs >0.5] = "Kids Mean:No" #arbitrary cutoff probability
table(glm.log.pred, Kids_Status)
#     Kids_Status
#glm.log.pred       No   Yes
#Kids Meal:Yes 62358  3707
#Kids Mean:No     24     3
#success rate:  0.9436089
summary(glm.log)

#non-parametric regression
plot(menu_total.subset.nonpara$Calories ~ menu_total.subset.nonpara$Total_Fat)
plot(menu_total.subset.nonpara$Calories ~ menu_total.subset.nonpara$Carbohydrates)
plot(menu_total.subset.nonpara$Calories ~ menu_total.subset.nonpara$Sodium)
plot(menu_total.subset.nonpara$Calories ~ menu_total.subset.nonpara$Protein)
#candidate of choice is Calories ~ Protein

for (bw in c(2, 10, 20))
{
  with(menu_total.subset.nonpara, 
       {plot(Calories~Protein, col=gray(0.75))
        lines(ksmooth(Protein, Calories, "normal", bw))})
}#best is bandwidth=10

#use cross validation to pick a bandwidth
library(sm)
with(menu_total.subset.nonpara, sm.regression(Calories, Protein, h=h.select(Calories,Protein) ) )
#using splines
with(menu_total.subset.nonpara, {
  plot(Calories~ Protein, col=gray(0.75))
  lines(smooth.spline(Protein, Calories), lty=2) }) #much better

with(menu_total.subset.nonpara, {
  plot(Calories ~ Protein, col=gray(0.75))
  f <- loess(Calories~ Protein)
  i <-order(Protein)
  lines(f$x[i], f$fitted[i])
}) #somewhat betterthan bandwith=20

ggplot(menu_total.subset.nonpara, aes(x=Protein, y=Calories)) + geom_point(alpha=0.25) + geom_smooth(
  method="loess", span=1) + geom_line(aes(x=Protein, y=Calories), linetype=2)

#Discussion of Methods Faraway 14.6
#regular linear regression
linear1 <- lm(Calories~ Protein, menu_total.subset.nonpara)
plot(linear1$residuals, linear1$fitted.values) #hetero
linear2 <- lm(sqrt(Calories) ~ sqrt(Protein), menu_total.subset.nonpara)
plot(linear2$residuals, linear2$fitted.values)

#longitudinal
#get rid of extreme values of Calories 
menu12_17.na.2 <- subset(menu12_17.na, Calories <2000)
menu12_17.na.BB <- subset(menu12_17.na.2, Restaurant== "Burger King") #1,812 entries
first20 <- filter(menu12_17.na.BB, MenuItemID %in% c("7", "8", "11", "12", "14",
                                                    "18", "21", "25", "27", "278",
                                                    "280", "282", "284", "797", "874",
                                                    "876", "877", "879", "880", "882")) #corresponds to the 10 items that are grouped like this
ggplot(first20, aes(x=Year, y=Calories)) + geom_line() + facet_wrap(~MenuItemID)


ggplot(first20, aes(x=Year, y= Calories, group=MenuItemID)) + geom_line(aes(linetype=Food_Category, color=Food_Category))+
  geom_point(aes(color=Food_Category))+
  theme(legend.position="top") +
  facet_wrap(~Food_Category)

#for more points
first40 <- filter(menu12_17.na.BB, MenuItemID %in% c("7", "8", "11", "12", "14",
                                                    "18", "21", "25", "27", "278",
                                                     "280", "282", "284", "797", "874",
                                                     "876", "877", "879", "880", "882",
                                                    "1547", "1549", "1551", "1553", "1645",
                                                    "1646", "1648", "2128", "2129", "2130", 
                                                    "2131", "2790", "2829", "2956", "2973",
                                                   "2995", "2996", "3111", "3130", "3679") )

long.mod <- lm(Calories ~ Year, subset=(MenuItemID==7), menu12_17.na.BB)
library(lme4) #consider restarting R studio after installing this package
ml <-lmList(Calories~ Year | MenuItemID, first40)
intercepts <- sapply(ml, coef)[1,] #instead of looping
slopes <- sapply(ml, coef)[2,]
plot(intercepts, slopes, xlab="Intercept", ylab="Slope") #inverse relationship
