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
install.packages("dplyr")
install.packages("mlr")

library(dplyr)
library(mlr)
library(tidyr)


##--------------------------------------------------------------##
## Read In Data                             --------------------##
##--------------------------------------------------------------##
data <- read.csv('/Users/robertward/Dropbox/MSAn Capstone - Fall 2018/Data/3. 100K Sampled Data/FINAL2_SAMPLED100.csv',
                 na.strings=c("NA","NaN", " ", ""))


##--------------------------------------------------------------##
## Intro to DPLYR                           --------------------##
##--------------------------------------------------------------##

#---SELECT certain variables in a data frame  -------------------------------------------
data.1 <- data %>% 
  select(RESP, days_since_last_activity, V90, Upmarket )

#---DROP (-SELECT) certain variables in a data frame  -------------------------------------------
data.2 <- data.1 %>% 
  select(-V90, -Upmarket)

#---FILTER data frame based on conditions  -------------------------------------------
data.RESP <- data %>% 
  filter(RESP==1)

#---CREATE NEW VARIABLES  -------------------------------------------
data.3 <- data.1 %>% 
  mutate(recent_active = ifelse(days_since_last_activity<365, 1, 0))

#---Create SUMMARY METRICS by GROUP --------------------------------------------------
data.4 <- data.1 %>% 
  group_by(Upmarket) %>% 
  summarise(avg_RESP = mean(RESP, na.rm=T))

print(data.4)

##--------------------------------------------------------------##
## Missing Imputation                       --------------------##
##--------------------------------------------------------------##

#---Explore MISSING VARIABLES -------------------------------------------------------
missing_values <- data %>% 
  summarise_all(funs(sum(is.na(.))/n())) %>% 
  gather(key="feature", value="missing_pct")

missing_values %>% 
  ggplot(aes(x=reorder(feature,-missing_pct),y=missing_pct)) +
  geom_bar(stat="identity",fill="steelblue")+
  coord_flip()

#---Remove variables with X% missing (constant)--------------------------------
data.5 <- removeConstantFeatures(data, perc=0.01) #99% the same value


# names_path <- '/Users/robertward/Dropbox/American/KSB-620/Capstone_Project/rename_cols.csv'
library(data.table)

# setnames()


#----Explore the 'Class' Types in your Data---------------------------------------

#Get Class by Variable DF
table(sapply(data.5, class))


# could also impute by different groups...state/ 

#----Impute Missings w/Mean & Mode-----------------------------------------------
library(mlr)
imp <- mlr::impute(data.5, classes = list(character = imputeMode(), 
                                        numeric=imputeMedian(),
                                     integer = imputeMedian()), 
              dummy.classes = c("character", "numeric", "integer"), dummy.type = "numeric") #You can include this or not

data.noMiss <- imp$data

# #Re-Check Missings
# missing_values <- data.noMiss %>% 
#   summarise_all(funs(sum(is.na(.))/n())) %>% 
#   gather(key="feature", value="missing_pct")
# 
# missing_values %>% 
#   filter(missing_pct>0) %>% 
#   ggplot(aes(x=reorder(feature,-missing_pct),y=missing_pct)) +
#   geom_bar(stat="identity",fill="steelblue")+
#   coord_flip() 

data.orig <- data.5
names(data.orig)
variables.i.want <- names(data.orig)[c(1:50)]
variables.i.want
new.data <- data.orig[,variables.i.want]
data <- new.data

#give class of variable
data <- removeConstantFeatures(data, perc=0.01)
lapply(d, class)
lapply(data, class)

df<-data %>%
  mutate_if(is.integer, as.factor)
table(sapply(data, class))
table(sapply(df, class))

shinyApp(ui = ui, server = server)

