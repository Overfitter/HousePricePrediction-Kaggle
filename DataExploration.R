###-------WOrking Directory & Dataset-------###
setwd("H:/Data Science Competitons/Kaggle/HousePricesPrediction")
train <- fread("H:/Data Science Competitons/Kaggle/HousePricesPrediction/train.csv")
test <- fread("H:/Data Science Competitons/Kaggle/HousePricesPrediction/test.csv")

###-------Load Libraries-------###
load.libraries <- c('data.table', 'testthat', 'gridExtra', 'corrplot', 'GGally', 'ggplot2', 'e1071', 'dplyr')
install.lib <- load.libraries[!load.libraries %in% installed.packages()]
for(libs in install.lib) install.packages(libs, dependences = TRUE)
sapply(load.libraries, require, character = TRUE)

###-------Variables Types-------###
cat_var <- names(train)[which(sapply(train, is.character))] #Categorical variables
cat_car <- c(cat_var, 'BedroomAbvGr', 'HalfBath', ' KitchenAbvGr','BsmtFullBath', 'BsmtHalfBath', 'MSSubClass')
numeric_var <- names(train)[which(sapply(train, is.numeric))]

###-------Structure of Data-------###
dim(train)
str(train)

###-------Missing Values (Numerical, Categorical)-------###
head(train)

colSums(sapply(train,is.na))

colSums(sapply(train[,.SD, .SDcols = cat_var], is.na))

colSums(sapply(train[,.SD, .SDcols = numeric_var], is.na))

#Visualization of missing data
plot_Missing <- function(data_in, title = NULL){
  temp_df <- as.data.frame(ifelse(is.na(data_in), 0, 1))
  temp_df <- temp_df[,order(colSums(temp_df))]
  data_temp <- expand.grid(list(x = 1:nrow(temp_df), y = colnames(temp_df)))
  data_temp$m <- as.vector(as.matrix(temp_df))
  data_temp <- data.frame(x = unlist(data_temp$x), y = unlist(data_temp$y), m = unlist(data_temp$m))
  ggplot(data_temp) + geom_tile(aes(x=x, y=y, fill=factor(m))) + scale_fill_manual(values=c("white", "black"), name="Missing\n(0=Yes, 1=No)") + theme_light() + ylab("") + xlab("") + ggtitle(title)
}


plot_Missing(train[,colSums(is.na(train)) > 0, with = FALSE])

# Let's gain some insight on the number of houses that were remodeled
sum(train[,'YearRemodAdd',with=FALSE]!=train[,'YearBuilt',with=F])
#696 remodeled
cat('Percentage of houses remodeled',sum(train[,'YearRemodAdd', with = FALSE] != train[,'YearBuilt', with = FALSE])/ dim(train)[1])
#Percentage of houses remodeled 0.4767123

train %>% select(YearBuilt, YearRemodAdd) %>%    mutate(Remodeled = as.integer(YearBuilt != YearRemodAdd)) %>% ggplot(aes(x= factor(x = Remodeled, labels = c( 'No','Yes')))) + geom_bar() + xlab('Remodeled') + theme_light()

###-----Summarize the numeric values and the structure of the data.-----###
summary(train[,.SD, .SDcols =numeric_var])
cat('Train has', dim(train)[1], 'rows and', dim(train)[2], 'columns.')
cat('Test has', dim(test)[1], 'rows and', dim(test)[2], ' columns.')
# The percentage of data missing in train.
sum(is.na(train)) / (nrow(train) *ncol(train))
# The percentage of data missing in test.
sum(is.na(test)) / (nrow(test) * ncol(test))

# Check for duplicated rows.
cat("The number of duplicated rows are", nrow(train) - nrow(unique(train)))

####Convert character to factors 
train[,(cat_var) := lapply(.SD, as.factor), .SDcols = cat_var]

train_cat <- train[,.SD, .SDcols = cat_var]
train_cont <- train[,.SD,.SDcols = numeric_var]

plotHist <- function(data_in, i) {
  data <- data.frame(x=data_in[[i]])
  p <- ggplot(data=data, aes(x=factor(x))) + stat_count() + xlab(colnames(data_in)[i]) + theme_light() + 
    theme(axis.text.x = element_text(angle = 90, hjust =1))
  return (p)
}

doPlots <- function(data_in, fun, ii, ncol=3) {
  pp <- list()
  for (i in ii) {
    p <- fun(data_in=data_in, i=i)
    pp <- c(pp, list(p))
  }
  do.call("grid.arrange", c(pp, ncol=ncol))
}

plotDen <- function(data_in, i){
  data <- data.frame(x=data_in[[i]], SalePrice = data_in$SalePrice)
  p <- ggplot(data= data) + geom_line(aes(x = x), stat = 'density', size = 1,alpha = 1.0) +
    xlab(paste0((colnames(data_in)[i]), '\n', 'Skewness: ',round(skewness(data_in[[i]], na.rm = TRUE), 2))) + theme_light() 
  return(p)
  
}

##-----Barplots for Categorical Variables---###
doPlots(train_cat, fun = plotHist, ii = 1:4, ncol = 2)

doPlots(train_cat, fun = plotHist, ii  = 4:8, ncol = 2)

doPlots(train_cat, fun = plotHist, ii = 8:12, ncol = 2)

doPlots(train_cat, fun = plotHist, ii = 13:18, ncol = 2)

doPlots(train_cat, fun = plotHist, ii = 18:22, ncol = 2)

train %>% select(LandSlope, Neighborhood, SalePrice) %>% filter(LandSlope == c('Sev', 'Mod')) %>% 
  arrange(Neighborhood) %>% group_by(Neighborhood, LandSlope) %>% summarize(Count = n()) %>% 
  ggplot(aes(Neighborhood, Count)) + geom_bar(aes(fill = LandSlope), position = 'dodge', stat = 'identity') + 
  theme_light() +theme(axis.text.x = element_text(angle = 90, hjust =1))

train %>% select(Neighborhood, SalePrice) %>% ggplot(aes(factor(Neighborhood), SalePrice)) + 
  geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust =1)) + xlab('Neighborhoods')

##-----Density plots for Numerical Variables---###
doPlots(train_cont, fun = plotDen, ii = 2:6, ncol = 2)

doPlots(train_cont, fun = plotDen, ii = 7:12, ncol = 2)

doPlots(train_cont, fun = plotDen, ii = 13:17, ncol = 2)

##-----Historgram plots for Numerical Variables---###
doPlots(train_cont, fun = plotHist, ii = 18:23, ncol = 2)

##---Explore the correlation---##
correlations <- cor(na.omit(train_cont[,-1, with = FALSE]))

# correlations
row_indic <- apply(correlations, 1, function(x) sum(x > 0.3 | x < -0.3) > 1)

correlations<- correlations[row_indic ,row_indic ]
corrplot(correlations, method="square")

#Plot scatter plot for variables that have high correlation.
train %>% select(OverallCond, YearBuilt) %>% ggplot(aes(factor(OverallCond),YearBuilt)) + 
  geom_boxplot() + xlab('Overall Condition')

plotCorr <- function(data_in, i){
  data <- data.frame(x = data_in[[i]], SalePrice = data_in$SalePrice)
  p <- ggplot(data, aes(x = x, y = SalePrice)) + geom_point(shape = 1, na.rm = TRUE) + geom_smooth(method = lm ) + xlab(paste0(colnames(data_in)[i], '\n', 'R-Squared: ', round(cor(data_in[[i]], data$SalePrice, use = 'complete.obs'), 2))) + theme_light()
  return(suppressWarnings(p))
}

highcorr <- c(names(correlations[,'SalePrice'])[which(correlations[,'SalePrice'] > 0.5)], names(correlations[,'SalePrice'])[which(correlations[,'SalePrice'] < -0.2)])

data_corr <- train[,highcorr, with = FALSE]


doPlots(data_corr, fun = plotCorr, ii = 1:6)

doPlots(data_corr, fun = plotCorr, ii = 6:11)

##Sales Price Variable distribution
library(scales)
ggplot(train, aes(x=SalePrice)) + geom_histogram(col = 'white') + theme_light() +scale_x_continuous(labels = comma)

summary(train[,.(SalePrice)])

#Normalize distribution
ggplot(train, aes(x=log(SalePrice+1))) + geom_histogram(col = 'white') + theme_light()



#######------------------ Plotting all data using tabplots------------#####

#Objective: find out some of the good features visually =)

#```{r, fig.width = 11, fig.height = 5.5, echo = FALSE, message = FALSE, warning = FALSE}
invisible(library(tabplot))
invisible(library(data.table))

columns <- c("numeric",
             rep("character", 2),
             rep("numeric", 2),
             rep("character", 12),
             rep("numeric", 4),
             rep("character", 5),
             "numeric",
             rep("character", 7),
             "numeric",
             "character",
             rep("numeric", 3),
             rep("character", 4),
             rep("numeric", 10),
             "character",
             "numeric",
             "character",
             "numeric",
             rep("character", 2),
             "numeric",
             "character",
             rep("numeric", 2),
             rep("character", 3),
             rep("numeric", 6),
             rep("character", 3),
             rep("numeric", 3),
             rep("character", 2),
             rep("numeric"))

data <- fread("train.csv", data.table = FALSE, header = TRUE, sep = ",", colClasses = columns)

data$SalePrice <- log(data$SalePrice) # To respect lrmse

data <- as.data.frame(data)

for (i in 1:80) {
  if (typeof(data[, i]) == "character") {
    data[is.na(data[, i]), i] <- ""
    data[, i] <- as.factor(data[, i])
  }
}

for (i in 1:16) {
  plot(tableplot(data, select = c(((i - 1) * 5 + 1):(i * 5), 81), sortCol = 6, nBins = 73, plot = FALSE), fontsize = 12, title = paste("log(SalePrice) vs ", paste(colnames(data)[((i - 1) * 5 + 1):(i * 5)], collapse = "+"), sep = ""), showTitle = TRUE, fontsize.title = 12)
}

