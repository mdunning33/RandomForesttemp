####Script for predicting response based on zip and geo level data ######
###Model is run through: Rscript --default-packages=base,utils,caret,stats,dplyr,randomForest model.R (file as arg)

args <- commandArgs(trailingOnly = TRUE)

# test if there is at least one argument: if not, return an error
if (length(args)==0) {
  stop("At least one argument must be supplied (input file).\n", call.=FALSE)
} else if (length(args)==1) {
  # default output file
  args[2] = "output2.csv"
}

## program...
##Read in input data
df_temp <- read.csv(args[1], header=TRUE)



#if no response rate per geocode, create one (used as dependent)
#create resp % by geocode
#df_geo <- df %>%
#  group_by(GEOCODE) %>%
#  summarise(geocode_resp = mean(resp_flag))

#append to input
#df_input <- left_join(df_input, df_geo, by = 'GEOCODE')

#Collapse table by geocode
#df_temp <- df_temp[!duplicated(df_temp$GEOCODE),]

#subset only columns used in model
#df_temp <- subset(df_temp, select = c(geocode_rte_resp:A11902))

##bucket response rates into classes for RandomForest Classifier Dependent
df_temp$geocode_resp_lvl <-cut(df_temp$geocode_rte_resp, c(0,0.01,0.015,0.02,0.03,0.05,0.1,1))

#Drop response rate column -- will overfit
df_temp <- df_temp[, -which(names(df_temp) %in% c("geocode_rte_resp"))]

##Refactoring dependent necessary after subsetting
#df_temp$geocode_resp_lvl <- as.factor(df_temp$geocode_resp_level)

#Training differnt model fits
#fit <- train(log(geocode_rte_resp) ~ ., data = df_input, method = "lm", na.action = na.exclude)
#fit <- rpart(geocode_resp_lvl ~ ., data = df_input, method = "class", na.action = na.exclude)
##Create model
control <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 7
metric <- "Accuracy"
set.seed(seed)
mtry <- sqrt(ncol(df_temp[,1:132]))
tunegrid <- expand.grid(.mtry=mtry)
df_temp$geocode_resp_lvl <- as.factor(df_temp$geocode_resp_lvl)
rf <- train(geocode_resp_lvl~., data=df_temp, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control, na.action = na.exclude)


#Storing Predictions
df_temp$predictions <- predict(rf)


#Writing Output
write.csv(df_temp, file=args[2], row.names=FALSE)
write.csv(summary(rf), "summary_stats.csv")
