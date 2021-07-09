# dataset for each year; all with same number of var
menu2017 <- read.csv("/Users/Sou/Desktop/GLM Final/Menu2017.csv", stringsAsFactors = FALSE)
menu2016 <- read.csv("/Users/Sou/Desktop/GLM Final/Menu2016.csv", stringsAsFactors = FALSE)
menu2015 <- read.csv("/Users/Sou/Desktop/GLM Final/Menu2015.csv", stringsAsFactors = FALSE)
menu2014 <- read.csv("/Users/Sou/Desktop/GLM Final/Menu2014.csv", stringsAsFactors = FALSE)
menu2013 <- read.csv("/Users/Sou/Desktop/GLM Final/Menu2013.csv", stringsAsFactors = FALSE)
menu2012 <- read.csv("/Users/Sou/Desktop/GLM Final/Menu2012.csv", stringsAsFactors = FALSE)

#clean 2017
menu2017$Food_Category <- as.factor(menu2017$Food_Category)
menu2017$Kids_Meal_2017<- as.factor(menu2017$Kids_Meal_2017)
menu2017$Kids_Meal_2017 <- ifelse(menu2017$Kids_Meal_2017=="0", "No", "Yes")
menu2017$Limited_Time_Offer_2017 <- as.factor(menu2017$Limited_Time_Offer_2017)
menu2017$Limited_Time_Offer_2017 <- ifelse(menu2017$Limited_Time_Offer_2017=="0", "No", "Yes")
menu2017$Regional_2017 <- as.factor(menu2017$Regional_2017)
menu2017$Regional_2017 <- ifelse(menu2017$Regional_2017=="0", "No", "Yes")
menu2017$Shareable_2017 <- as.factor(menu2017$Shareable_2017)
menu2017$Shareable_2017 <- ifelse(menu2017$Shareable_2017=="0", "No", "Yes")

menu2017_subset <- subset(x=menu2017, select = c(Restaurant,Item_Name_2017, Food_Category, Serving_Size_2017,
                         Calories_2017, Total_Fat_2017, Saturated_Fat_2017,
                         Trans_Fat_2017, Cholesterol_2017, Sodium_2017,
                         Potassium_2017, Carbohydrates_2017, Dietary_Fiber_2017,
                         Sugar_2017, Protein_2017, Kids_Meal_2017, 
                         Limited_Time_Offer_2017, Regional_2017, Shareable_2017
                         ))
menu2017_subset$Kids_Meal_2017 <-as.factor(menu2017_subset$Kids_Meal_2017)
menu2017_subset$Limited_Time_Offer_2017 <-as.factor(menu2017_subset$Limited_Time_Offer_2017)
menu2017_subset$Regional_2017 <-as.factor(menu2017_subset$Regional_2017)
menu2017_subset$Shareable_2017 <- as.factor(menu2017_subset$Shareable_2017)
#remove observations with missing variables
menu2017_subset.na<- na.omit(menu2017_subset) #312 full obs.
#change column names
colnames(menu2017_subset)[4:19] <- c("Serving_Size", "Calories", "Total_Fat", "Saturated_Fat",
                                    "Trans_Fat", "Cholesterol", "Sodium", "Potassium",  "Carbohydrates",
                                    "Dietary_Fiber", "Sugar", "Protein", "Kids_Meal","Limited_Time_Offer",
                                    "Regional", "Shareable")
colnames(menu2017_subset)[2] <- "Item_Name"

#clean 2016
menu2016$Food_Category <- as.factor(menu2016$Food_Category)
menu2016$Kids_Meal_2016<- as.factor(menu2016$Kids_Meal_2016)
menu2016$Kids_Meal_2016 <- ifelse(menu2016$Kids_Meal_2016=="0", "No", "Yes")
menu2016$Limited_Time_Offer_2016 <- as.factor(menu2016$Limited_Time_Offer_2016)
menu2016$Limited_Time_Offer_2016 <- ifelse(menu2016$Limited_Time_Offer_2016=="0", "No", "Yes")
menu2016$Regional_2016 <- as.factor(menu2016$Regional_2016)
menu2016$Regional_2016 <- ifelse(menu2016$Regional_2016=="0", "No", "Yes")
menu2016$Shareable_2016 <- as.factor(menu2016$Shareable_2016)
menu2016$Shareable_2016 <- ifelse(menu2016$Shareable_2016=="0", "No", "Yes")
menu2016_subset <- subset(x=menu2016, select = c(Restaurant, Item_Name_2016, Food_Category, Serving_Size_2016,
                                                 Calories_2016, Total_Fat_2016, Saturated_Fat_2016,
                                                 Trans_Fat_2016, Cholesterol_2016, Sodium_2016,
                                                 Potassium_2016, Carbohydrates_2016, Dietary_Fiber_2016,
                                                 Sugar_2016, Protein_2016, Kids_Meal_2016, 
                                                 Limited_Time_Offer_2016, Regional_2016, Shareable_2016
))
menu2016_subset$Kids_Meal_2016 <-as.factor(menu2016_subset$Kids_Meal_2016)
menu2016_subset$Limited_Time_Offer_2016 <-as.factor(menu2016_subset$Limited_Time_Offer_2016)
menu2016_subset$Regional_2016 <-as.factor(menu2016_subset$Regional_2016)
menu2016_subset$Shareable_2016 <- as.factor(menu2016_subset$Shareable_2016)
menu2016_subset.na<- na.omit(menu2016_subset) #257 full obs.

colnames(menu2016_subset)[4:19] <- c("Serving_Size", "Calories", "Total_Fat", "Saturated_Fat",
                                     "Trans_Fat", "Cholesterol", "Sodium", "Potassium",  "Carbohydrates",
                                     "Dietary_Fiber", "Sugar", "Protein", "Kids_Meal","Limited_Time_Offer",
                                     "Regional", "Shareable")
colnames(menu2016_subset)[2] <- "Item_Name"

#2015
menu2015$Food_Category <- as.factor(menu2015$Food_Category)
menu2015$Kids_Meal_2015<- as.factor(menu2015$Kids_Meal_2015)
menu2015$Kids_Meal_2015 <- ifelse(menu2015$Kids_Meal_2015=="0", "No", "Yes")
menu2015$Limited_Time_Offer_2015 <- as.factor(menu2015$Limited_Time_Offer_2015)
menu2015$Limited_Time_Offer_2015 <- ifelse(menu2015$Limited_Time_Offer_2015=="0", "No", "Yes")
menu2015$Regional_2015 <- as.factor(menu2015$Regional_2015)
menu2015$Regional_2015 <- ifelse(menu2015$Regional_2015=="0", "No", "Yes")
menu2015$Shareable_2015 <- as.factor(menu2015$Shareable_2015)
menu2015$Shareable_2015 <- ifelse(menu2015$Shareable_2015=="0", "No", "Yes")
menu2015_subset <- subset(x=menu2015, select = c(Restaurant, Item_Name_2015, Food_Category, Serving_Size_2015,
                                                 Calories_2015, Total_Fat_2015, Saturated_Fat_2015,
                                                 Trans_Fat_2015, Cholesterol_2015, Sodium_2015,
                                                 Potassium_2015, Carbohydrates_2015, Dietary_Fiber_2015,
                                                 Sugar_2015, Protein_2015, Kids_Meal_2015, 
                                                 Limited_Time_Offer_2015, Regional_2015, Shareable_2015
))
menu2015_subset$Kids_Meal_2015 <-as.factor(menu2015_subset$Kids_Meal_2015)
menu2015_subset$Limited_Time_Offer_2015 <-as.factor(menu2015_subset$Limited_Time_Offer_2015)
menu2015_subset$Regional_2015 <-as.factor(menu2015_subset$Regional_2015)
menu2015_subset$Shareable_2015 <- as.factor(menu2015_subset$Shareable_2015)
menu2015_subset.na<- na.omit(menu2015_subset) #240 full obs.
colnames(menu2015_subset)[4:19] <- c("Serving_Size", "Calories", "Total_Fat", "Saturated_Fat",
                                     "Trans_Fat", "Cholesterol", "Sodium", "Potassium",  "Carbohydrates",
                                     "Dietary_Fiber", "Sugar", "Protein", "Kids_Meal","Limited_Time_Offer",
                                     "Regional", "Shareable")
colnames(menu2015_subset)[2] <- "Item_Name"

#2014
menu2014$Food_Category <- as.factor(menu2014$Food_Category)
menu2014$Kids_Meal_2014<- as.factor(menu2014$Kids_Meal_2014)
menu2014$Kids_Meal_2014 <- ifelse(menu2014$Kids_Meal_2014=="0", "No", "Yes")
menu2014$Limited_Time_Offer_2014 <- as.factor(menu2014$Limited_Time_Offer_2014)
menu2014$Limited_Time_Offer_2014 <- ifelse(menu2014$Limited_Time_Offer_2014=="0", "No", "Yes")
menu2014$Regional_2014 <- as.factor(menu2014$Regional_2014)
menu2014$Regional_2014 <- ifelse(menu2014$Regional_2014=="0", "No", "Yes")
menu2014$Shareable_2014 <- as.factor(menu2014$Shareable_2014)
menu2014$Shareable_2014 <- ifelse(menu2014$Shareable_2014=="0", "No", "Yes")
menu2014_subset <- subset(x=menu2014, select = c(Restaurant, Item_Name_2014, Food_Category, Serving_Size_2014,
                                                 Calories_2014, Total_Fat_2014, Saturated_Fat_2014,
                                                 Trans_Fat_2014, Cholesterol_2014, Sodium_2014,
                                                 Potassium_2014, Carbohydrates_2014, Dietary_Fiber_2014,
                                                 Sugar_2014, Protein_2014, Kids_Meal_2014, 
                                                 Limited_Time_Offer_2014, Regional_2014, Shareable_2014
))
menu2014_subset$Kids_Meal_2014 <-as.factor(menu2014_subset$Kids_Meal_2014)
menu2014_subset$Limited_Time_Offer_2014 <-as.factor(menu2014_subset$Limited_Time_Offer_2014)
menu2014_subset$Regional_2014 <-as.factor(menu2014_subset$Regional_2014)
menu2014_subset$Shareable_2014 <- as.factor(menu2014_subset$Shareable_2014)
menu2014_subset.na<- na.omit(menu2014_subset) #214 full obs.
colnames(menu2014_subset)[4:19] <- c("Serving_Size", "Calories", "Total_Fat", "Saturated_Fat",
                                     "Trans_Fat", "Cholesterol", "Sodium", "Potassium",  "Carbohydrates",
                                     "Dietary_Fiber", "Sugar", "Protein", "Kids_Meal","Limited_Time_Offer",
                                     "Regional", "Shareable")
colnames(menu2014_subset)[2] <- "Item_Name"

#2013
menu2013$Food_Category <- as.factor(menu2013$Food_Category)
menu2013$Kids_Meal_2013<- as.factor(menu2013$Kids_Meal_2013)
menu2013$Kids_Meal_2013 <- ifelse(menu2013$Kids_Meal_2013=="0", "No", "Yes")
menu2013$Limited_Time_Offer_2013 <- as.factor(menu2013$Limited_Time_Offer_2013)
menu2013$Limited_Time_Offer_2013 <- ifelse(menu2013$Limited_Time_Offer_2013=="0", "No", "Yes")
menu2013$Regional_2013 <- as.factor(menu2013$Regional_2013)
menu2013$Regional_2013 <- ifelse(menu2013$Regional_2013=="0", "No", "Yes")
menu2013$Shareable_2013 <- as.factor(menu2013$Shareable_2013)
menu2013$Shareable_2013 <- ifelse(menu2013$Shareable_2013=="0", "No", "Yes")
menu2013_subset <- subset(x=menu2013, select = c(Restaurant,Item_Name_2013, Food_Category, Serving_Size_2013,
                                                 Calories_2013, Total_Fat_2013, Saturated_Fat_2013,
                                                 Trans_Fat_2013, Cholesterol_2013, Sodium_2013,
                                                 Potassium_2013, Carbohydrates_2013, Dietary_Fiber_2013,
                                                 Sugar_2013, Protein_2013, Kids_Meal_2013, 
                                                 Limited_Time_Offer_2013, Regional_2013, Shareable_2013
))
menu2013_subset$Kids_Meal_2013 <-as.factor(menu2013_subset$Kids_Meal_2013)
menu2013_subset$Limited_Time_Offer_2013 <-as.factor(menu2013_subset$Limited_Time_Offer_2013)
menu2013_subset$Regional_2013 <-as.factor(menu2013_subset$Regional_2013)
menu2013_subset$Shareable_2013 <- as.factor(menu2013_subset$Shareable_2013)
menu2013_subset.na<- na.omit(menu2013_subset) #205 full obs.
colnames(menu2013_subset)[4:19] <- c("Serving_Size", "Calories", "Total_Fat", "Saturated_Fat",
                                     "Trans_Fat", "Cholesterol", "Sodium", "Potassium",  "Carbohydrates",
                                     "Dietary_Fiber", "Sugar", "Protein", "Kids_Meal","Limited_Time_Offer",
                                     "Regional", "Shareable")
colnames(menu2013_subset)[2] <- "Item_Name"

#2012
menu2012$Food_Category <- as.factor(menu2012$Food_Category)
menu2012$Kids_Meal_2012<- as.factor(menu2012$Kids_Meal_2012)
menu2012$Kids_Meal_2012 <- ifelse(menu2012$Kids_Meal_2012=="0", "No", "Yes")
menu2012$Limited_Time_Offer_2012 <- as.factor(menu2012$Limited_Time_Offer_2012)
menu2012$Limited_Time_Offer_2012 <- ifelse(menu2012$Limited_Time_Offer_2012=="0", "No", "Yes")
menu2012$Regional_2012 <- as.factor(menu2012$Regional_2012)
menu2012$Regional_2012 <- ifelse(menu2012$Regional_2012=="0", "No", "Yes")
menu2012$Shareable_2012 <- as.factor(menu2012$Shareable_2012)
menu2012$Shareable_2012 <- ifelse(menu2012$Shareable_2012=="0", "No", "Yes")
menu2012_subset <- subset(x=menu2012, select = c(Restaurant,Item_Name_2012, Food_Category, Serving_Size_2012,
                                                 Calories_2012, Total_Fat_2012, Saturated_Fat_2012,
                                                 Trans_Fat_2012, Cholesterol_2012, Sodium_2012,
                                                 Potassium_2012, Carbohydrates_2012, Dietary_Fiber_2012,
                                                 Sugar_2012, Protein_2012, Kids_Meal_2012, 
                                                 Limited_Time_Offer_2012, Regional_2012, Shareable_2012
))
menu2012_subset$Kids_Meal_2012 <-as.factor(menu2012_subset$Kids_Meal_2012)
menu2012_subset$Limited_Time_Offer_2012 <-as.factor(menu2012_subset$Limited_Time_Offer_2012)
menu2012_subset$Regional_2012 <-as.factor(menu2012_subset$Regional_2012)
menu2012_subset$Shareable_2012 <- as.factor(menu2012_subset$Shareable_2012)
menu2012_subset.na<- na.omit(menu2012_subset) #230 full obs.
colnames(menu2012_subset)[4:19] <- c("Serving_Size", "Calories", "Total_Fat", "Saturated_Fat",
                                     "Trans_Fat", "Cholesterol", "Sodium", "Potassium",  "Carbohydrates",
                                     "Dietary_Fiber", "Sugar", "Protein", "Kids_Meal","Limited_Time_Offer",
                                     "Regional", "Shareable")
colnames(menu2012_subset)[2] <- "Item_Name"

####
menu_total.subset <- rbind(menu2017_subset, menu2016_subset, menu2015_subset, menu2014_subset,
                           menu2013_subset, menu2012_subset)


####Non Parametric
menu_total.subset.nonpara <- na.omit(menu_total.subset) #1,458 entries
#this is a better representation so that we don't have too much overlapping and clusters

#Longitudinal
menu12 <-subset(menu2012, select=c(MenuItemID, Restaurant, Food_Category, Item_Name_2012,Calories_2012))
menu12$Year <- 2012
colnames(menu12)[4] <-"Item Name"
colnames(menu12)[5] <- "Calories"
menu13 <-subset(menu2013, select=c(MenuItemID, Restaurant, Food_Category, Item_Name_2013,Calories_2013))
menu13$Year <- 2013
colnames(menu13)[4] <-"Item Name"
colnames(menu13)[5] <- "Calories"
menu14 <-subset(menu2014, select=c(MenuItemID, Restaurant, Food_Category, Item_Name_2014,Calories_2014))
menu14$Year <- 2014
colnames(menu14)[4] <-"Item Name"
colnames(menu14)[5] <- "Calories"
menu15 <-subset(menu2015, select=c(MenuItemID, Restaurant, Food_Category, Item_Name_2015,Calories_2015))
menu15$Year <- 2015
colnames(menu15)[4] <-"Item Name"
colnames(menu15)[5] <- "Calories"
menu16 <-subset(menu2016, select=c(MenuItemID, Restaurant, Food_Category, Item_Name_2016,Calories_2016))
menu16$Year <- 2016
colnames(menu16)[4] <-"Item Name"
colnames(menu16)[5] <- "Calories"
menu17 <-subset(menu2017, select=c(MenuItemID, Restaurant, Food_Category, Item_Name_2017,Calories_2017))
menu17$Year <- 2017
colnames(menu17)[4] <-"Item Name"
colnames(menu17)[5] <- "Calories"
menu12_17 <- rbind(menu12, menu13, menu14, menu15, menu16, menu17)
menu12_17.na <- na.omit(menu12_17)
#reduce number of categories into something more manageable
levels(menu12_17.na$Food_Category)[match(c("Desserts", "Baked Goods" ),
                                       levels(menu12_17.na$Food_Category))] <- "Desserts"
levels(menu12_17.na$Food_Category)[match(c("Fried Potatoes", "Pizza", "Burgers"),
                                       levels(menu12_17.na$Food_Category))] <- "Fast Food"
levels(menu12_17.na$Food_Category)[match(c("Toppings & Ingredients", "Soup", "Salads"),
                                       levels(menu12_17.na$Food_Category))] <- "Soups & Salads"
menu12_17.na$Year <- as.numeric(menu12_17.na$Year)

