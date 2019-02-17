##--------------------------------------------------------------##
## Universal Options   -----------------------------------------##
##-------------------------------------------------------------##
options(java.parameters = "-Xmx8g")
options("stringsAsFactors" = FALSE)
options(pillar.subtle_num = FALSE)
options(scipen=999)

library(dplyr)
library(mlr)
library(readr)
library(tidyr)


##--------------------------------------------------------------##
## Read In Data                             --------------------##
##--------------------------------------------------------------##
data <- read.csv('~/Dropbox/MSAn Capstone - Fall 2018/Data/3. 100K Sampled Data/FINAL2_SAMPLED100.csv', 
                 na.strings=c("NA","NaN", " ", ""))

#---Remove variables with X% missing (constant)--------------------------------
data.5 <- removeConstantFeatures(data, perc=0.01) #99% the same value

#----Explore the 'Class' Types in your Data---------------------------------------

#Get Class by Variable DF
keep = c('V41', 'V42', 'V47', 'V48')
table(sapply(data.5[, keep], class))
shawn = c('V160', 'V159', 'V161')
table(sapply(data.5[, shawn], class))

keep_variables <- c('RESP', 'V57', 'V90', 'V48', 'V41', 'V42', 'V138', 'V85', 'V47', 'V86', 
                    'V160', 'V161', 'V159', 'V189', 'V260', 'V61', 'V122', 'V124', 'V126', 
                    'V127', 'V128', 'V129', 'V162', 'days_since_last_activity', 'State', 'Upmarket', "MATCH")

data_keep <- data.5[, keep_variables]

#----Impute Missings w/Mean & Mode-----------------------------------------------
imp <- mlr::impute(data_keep, classes = list(numeric=imputeMedian(),integer = imputeMedian()))
               # , dummy.classes = c("character", "numeric", "integer"), dummy.type = "numeric") #You can include this or not

data.noMiss <- imp$data

###------ Create Dummy Variables-----------###
# install.packages("fastDummies")
library(fastDummies)

to_dummy <- c('V57', 'V90', 'V48', 'V41', 'V42', 'V138', 'V85', 'V47', 'V86',
              'V160', 'V161', 'V159')

# convert to dummy variables
data_dummies <- fastDummies::dummy_cols(data.noMiss, select_columns = to_dummy)
head(data_dummies)
tail(data_dummies)

# get rid of the original columns that were made into dummy variables
list_to_keep <- setdiff(names(data_dummies), to_dummy) 
data_dummies <- data_dummies[ , list_to_keep]

tail(data_dummies)


library(knitr)

#Keep just numeric/integer
df<-data_dummies %>% 
  select_if(is.numeric) %>%
  filter(MATCH == 1)

#Create variable list (excluding dep var)
VAR_LIST <- names(df)[ - which(names(df) == "RESP")]

#Set-up df
col_names <- c("Variable", "Corr", "P.Value", "abs.Corr")
list <- data.frame(matrix(nrow = length(VAR_LIST), ncol = length(col_names)))
names(list) <- col_names
n <- 1

#Run corr for each var
for (i in 1:(length(VAR_LIST))) {
  p <- cor.test(df[[VAR_LIST[[i]]]], df$RESP)
  list[n,"Variable"] <- VAR_LIST[[i]]
  list[n,"Corr"] <- p$estimate
  list[n,"P.Value"] <- p$p.value
  list[n,"abs.Corr"] <- abs(p$estimate)
  n <- n + 1
}

#Summarise & Print
list <- list[order(-list$abs.Corr),]
kable(list, caption = "Pearson Correlation Coefficients w/RESP ")


# library(GGally)
# 
# top_10 <- c("RESP", "days_since_last_activity", "Upmarket", "V90_1", "V138_1", "V61", "V162", "V138_7", "V86_K", 
#             "V161_E1", "V138_2")
# 
# library(corrplot)
# ggpairs(df[, top_10])
# # corrplot(df[, top_10])
# test <- df[,top_10]
# ggpairs(test)
# corrplot(test, method = 'circle') 













