##--------------------------------------------------------------##
## Universal Options   -----------------------------------------##
##-------------------------------------------------------------##
options(java.parameters = "-Xmx8g")
options("stringsAsFactors" = FALSE)
options(pillar.subtle_num = FALSE)
options(scipen=999)

##--------------------------------------------------------------##
## Install Necessary Packages / Libraries   --------------------##
##--------------------------------------------------------------##
#install.packages("dplyr")
#install.packages("mlr")
#install.packages("tidyr")
#install.packages("fastDummies")

library(dplyr)
library(mlr)
library(tidyr)
library(knitr)
library(fastDummies)


##--------------------------------------------------------------##
## Read In Data                             --------------------##
##--------------------------------------------------------------##
data <- read.csv('C:/Users/shawn/Documents/KSB 620/FINAL2_SAMPLED100.TXT',
                 na.strings=c("NA","NaN", " ", ""))

#---Remove variables with 99% missing (constant)--------------------------------
#data.1 <- removeConstantFeatures(data, perc=0.01) #99% the same value

#---SELECT desired variables in a data frame  -------------------------------------------
data.2 <- data.1 %>%
  select(RESP, days_since_last_activity, State, Upmarket, V42, V48, V88, V90, V140,V272,V274,V276,V278, MATCH)
#data.2<-data.1

#----Impute Missings w/Mean & Mode-----------------------------------------------
imp <- mlr::impute(data.2, classes = list(numeric=imputeMedian(),integer = imputeMedian()),
                   dummy.classes = c("character", "numeric", "integer"), dummy.type = "numeric") #You can include this or not

data.noMiss <- imp$data

###------ Create Dummy Variables-----------###
#to_dummy <- c('V57', 'V90', 'V48', 'V41', 'V42', 'V138', 'V85', 'V47', 'V86', 'V160', 'V161', 'V159')
#to_dummy <- c('V3', 'V4', 'V7', 'V9', 'V10', 'V12', 'V13', 'V14', 'V15', 'V29', 'V30', 'V31', 'V32', 'V35', 'V36', 'V38', 'V40', 'V41', 'V42', 'V43')
to_dummy <- c('V140','V90','V88','V48','V42')

# convert to dummy variables
data_dummies <- fastDummies::dummy_cols(data.noMiss, select_columns = to_dummy)
#head(data_dummies)
#tail(data_dummies)

# get rid of the original columns that were made into dummy variables
list_to_keep <- setdiff(names(data_dummies), to_dummy)
data_dummies <- data_dummies[ , list_to_keep]

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


#-------Testing-----
#Possibility - fit <- lm(RESP ~ days_since_last_activity + Upmarket + MATCH + V140 + V272 + V274 +V276 + V278, data=data_dummies)
fit.1 <- lm(RESP ~ days_since_last_activity + Upmarket + MATCH, data=data_dummies)
fit.2 <- lm(RESP ~ days_since_last_activity + Upmarket + MATCH + V90_1 + V140_2 + V140_4 + V140_1 + V88_1 + V48.dummy + V42.dummy, data=data_dummies)
fit.3 <- lm(RESP ~ days_since_last_activity + Upmarket + MATCH + V272 + V274 +V276 + V278 + V90_1 + V140_2 + V140_4 + V140_1 + V88_1 + V48.dummy + V42.dummy, data=data_dummies)
fit.4 <- glm(RESP ~ days_since_last_activity + Upmarket + MATCH + V272 + V274 +V276 + V278 + V90_1 + V140_2 + V140_4 + V140_1 + V88_1 + V48.dummy + V42.dummy, data=data_dummies)
fit.scores<- lm(RESP ~ V272 + V274 +V276 + V278, data=data_dummies)
fit.scores.2<- lm(RESP ~ Upmarket + V272 + V274 +V276 + V278, data=data_dummies)
#summary(fit.4)
Anova_fit<-anova(fit.1,fit.2,fit.3,fit.4,fit.scores,fit.scores.2)
write(Anova_fit,)
