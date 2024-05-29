
Exploratory Data Analysis and Data Cleaning

## IMPORTING DATA
library(ggplot2)
library(plyr)
library(ROCR)
adult <- read.table('https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data', 
                    sep = ',', fill = F, strip.white = T)
colnames(adult) <- c('age', 'workclass', 'fnlwgt', 'educatoin', 
                     'educatoin_num', 'marital_status', 'occupation', 'relationship', 'race', 'sex', 
                     'capital_gain', 'capital_loss', 'hours_per_week', 'native_country', 'income')


## DISCARDING VARIABLES --
adult$educatoin <- NULL
adult$fnlwgt <- NULL
adult$relationship <- NULL


## HISTOGRAM GROUPED BY INCOME 
# histogram of age by income group
ggplot(adult) + aes(x=as.numeric(age), group=income, fill=income) + 
  geom_histogram(binwidth=1, color='black')
￼




## HISTOGRAM GROUPED BY GENDER
ggplot(adult) + aes(x=as.numeric(age), group=sex, fill=sex) + 
  geom_histogram(binwidth=1, color='black')

￼



## SUMMARY OF WORKCLASS COLUMN 

summary(adult$workclass)

##                ?      Federal-gov        Local-gov     Never-worked 
##             1836              960             2093                7 
##          Private     Self-emp-inc Self-emp-not-inc        State-gov 
##            22696             1116             2541             1298 
##      Without-pay 
##               14





## COMBINING FEATURES TOGETHER THAT FALL INTO THE SAME CLASS

levels(adult$workclass)[1] <- 'Unknown'
# combine into Government job
adult$workclass <- gsub('^Federal-gov', 'Government', adult$workclass)
adult$workclass <- gsub('^Local-gov', 'Government', adult$workclass)
adult$workclass <- gsub('^State-gov', 'Government', adult$workclass) 

# combine into Sele-Employed job
adult$workclass <- gsub('^Self-emp-inc', 'Self-Employed', adult$workclass)
adult$workclass <- gsub('^Self-emp-not-inc', 'Self-Employed', adult$workclass)

# combine into Other/Unknown
adult$workclass <- gsub('^Never-worked', 'Other', adult$workclass)
adult$workclass <- gsub('^Without-pay', 'Other', adult$workclass)
adult$workclass <- gsub('^Other', 'Other/Unknown', adult$workclass)
adult$workclass <- gsub('^Unknown', 'Other/Unknown', adult$workclass)

adult$workclass <- as.factor(adult$workclass)



## SUMMARY OF WORKCLASS COLUMN

summary(adult$workclass)
##    Government Other/Unknown       Private Self-Employed 
##          4351          1857         22696          3657


## INCOME FOR EACH WORKCLASS CATEGORY AS DATAFRAME

# barplot of job type by income group
# get the counts by industry and income group
count <- table(adult[adult$workclass == 'Government',]$income)["<=50K"]
count <- c(count, table(adult[adult$workclass == 'Government',]$income)[">50K"])
count <- c(count, table(adult[adult$workclass == 'Other/Unknown',]$income)["<=50K"])
count <- c(count, table(adult[adult$workclass == 'Other/Unknown',]$income)[">50K"])
count <- c(count, table(adult[adult$workclass == 'Private',]$income)["<=50K"])
count <- c(count, table(adult[adult$workclass == 'Private',]$income)[">50K"])
count <- c(count, table(adult[adult$workclass == 'Self-Employed',]$income)["<=50K"])
count <- c(count, table(adult[adult$workclass == 'Self-Employed',]$income)[">50K"])
count <- as.numeric(count)

# create a dataframe
industry <- rep(levels(adult$workclass), each = 2)
income <- rep(c('<=50K', '>50K'), 4)
df <- data.frame(industry, income, count)
df
##        industry income count
## 1    Government  <=50K  3010
## 2    Government   >50K  1341
## 3 Other/Unknown  <=50K  1666
## 4 Other/Unknown   >50K   191
## 5       Private  <=50K 17733
## 6       Private   >50K  4963
## 7 Self-Employed  <=50K  2311
## 8 Self-Employed   >50K  1346


## INCOME PERCENTAGE FOR EACH WORKCLASS AS BAR GRAPH

# calculate the percentages
df <- ddply(df, .(industry), transform, percent = count/sum(count) * 100)

# format the labels and calculate their positions
df <- ddply(df, .(industry), transform, pos = (cumsum(count) - 0.5 * count))
df$label <- paste0(sprintf("%.0f", df$percent), "%")

# bar plot of counts by industry with in group proportions 
ggplot(df, aes(x = industry, y = count, fill = income)) +
  geom_bar(stat = "identity") +
  geom_text(aes(y = pos, label = label), size = 2) + 
  ggtitle('Income by Industry')



## INCOME FOR EACH CLASS OF EDUCATION NUM AS DATAFRAME

# create a dataframe
df1 <- data.frame(table(adult$income, adult$educatoin_num))
names(df1) <- c('income', 'education_num', 'count')
df1

##    income education_num count
## 1   <=50K             1    51
## 2    >50K             1     0
## 3   <=50K             2   162
## 4    >50K             2     6
## 5   <=50K             3   317
## 6    >50K             3    16
## 7   <=50K             4   606
## 8    >50K             4    40
## 9   <=50K             5   487
## 10   >50K             5    27
## 11  <=50K             6   871
## 12   >50K             6    62
## 13  <=50K             7  1115
## 14   >50K             7    60
## 15  <=50K             8   400
## 16   >50K             8    33
## 17  <=50K             9  8826
## 18   >50K             9  1675
## 19  <=50K            10  5904
## 20   >50K            10  1387
## 21  <=50K            11  1021
## 22   >50K            11   361
## 23  <=50K            12   802
## 24   >50K            12   265
## 25  <=50K            13  3134
## 26   >50K            13  2221
## 27  <=50K            14   764
## 28   >50K            14   959
## 29  <=50K            15   153
## 30   >50K            15   423
## 31  <=50K            16   107
## 32   >50K            16   306



## INCOME FOR EACH CLASS OF EDUCATION NUM AS BAR GRAPH

# calculate the percentages
df1 <- ddply(df1, .(education_num), transform, percent = count/sum(count) * 100)

# format the labels and calculate their positions
df1 <- ddply(df1, .(education_num), transform, pos = (cumsum(count) - 0.5 * count))
df1$label <- paste0(sprintf("%.0f", df1$percent), "%")

# remove some in group percentage to avoid overlapped text
df1$label[which(df1$percent < 5)] <- NA

# bar plot of counts by years of education with in group proportions 
ggplot(df1, aes(x = education_num, y = count, fill = income)) +
  geom_bar(stat = "identity") +
  geom_text(aes(y = pos, label = label), size = 2) + 
  ggtitle('Income Level with Years of Education')

￼



## SUMMARY OF OCCUPATION COLUMN


summary(adult$occupation)
##                 ?      Adm-clerical      Armed-Forces      Craft-repair 
##              1843              3770                 9              4099 
##   Exec-managerial   Farming-fishing Handlers-cleaners Machine-op-inspct 
##              4066               994              1370              2002 
##     Other-service   Priv-house-serv    Prof-specialty   Protective-serv 
##              3295               149              4140               649 
##             Sales      Tech-support  Transport-moving 
##              3650               928              1597



## CREATING NEW STANDARD COLUMNS AND GROUP EXISTING INTO APPROPIATE ONES

levels(adult$occupation)[1] <- 'Unknown'
adult$occupation <- gsub('Adm-clerical', 'White-Collar', adult$occupation)
adult$occupation <- gsub('Craft-repair', 'Blue-Collar', adult$occupation)
adult$occupation <- gsub('Exec-managerial', 'White-Collar', adult$occupation)
adult$occupation <- gsub('Farming-fishing', 'Blue-Collar', adult$occupation)
adult$occupation <- gsub('Handlers-cleaners', 'Blue-Collar', adult$occupation)
adult$occupation <- gsub('Machine-op-inspct', 'Blue-Collar', adult$occupation)
adult$occupation <- gsub('Other-service', 'Service', adult$occupation)
adult$occupation <- gsub('Priv-house-serv', 'Service', adult$occupation)
adult$occupation <- gsub('Prof-specialty', 'Professional', adult$occupation)
adult$occupation <- gsub('Protective-serv', 'Service', adult$occupation)
adult$occupation <- gsub('Tech-support', 'Service', adult$occupation)
adult$occupation <- gsub('Transport-moving', 'Blue-Collar', adult$occupation)
adult$occupation <- gsub('Unknown', 'Other/Unknown', adult$occupation)
adult$occupation <- gsub('Armed-Forces', 'Other/Unknown', adult$occupation)
adult$occupation <- as.factor(adult$occupation)
summary(adult$occupation)

##   Blue-Collar Other/Unknown  Professional         Sales       Service 
##         10062          1852          4140          3650          5021 
##  White-Collar 
##          7836


## INCOME FOR EACH CLASS OF OCCUPATION AS DATAFRAME

# create a dataframe
df2 <- data.frame(table(adult$income, adult$occupation))
names(df2) <- c('income', 'occupation', 'count')
df2
##    income    occupation count
## 1   <=50K   Blue-Collar  8362
## 2    >50K   Blue-Collar  1700
## 3   <=50K Other/Unknown  1660
## 4    >50K Other/Unknown   192
## 5   <=50K  Professional  2281
## 6    >50K  Professional  1859
## 7   <=50K         Sales  2667
## 8    >50K         Sales   983
## 9   <=50K       Service  4389
## 10   >50K       Service   632
## 11  <=50K  White-Collar  5361
## 12   >50K  White-Collar  2475


## INCOME FOR EACH CLASS OF OCCUPATION AS BARPLOT

# calculate the percentages
df2 <- ddply(df2, .(occupation), transform, percent = count/sum(count) * 100)

# format the labels and calculate their positions
df2 <- ddply(df2, .(occupation), transform, pos = (cumsum(count) - 0.5 * count))
df2$label <- paste0(sprintf("%.0f", df2$percent), "%")

# bar plot of counts by occupation with in group proportions 
ggplot(df2, aes(x = occupation, y = count, fill = income)) +
  geom_bar(stat = "identity") +
  geom_text(aes(y = pos, label = label), size = 2) + 
  ggtitle('Income Level with Different Occupations')

￼




## SUMMARY OF MARITAL STATUS

summary(adult$marital_status)
##              Divorced     Married-AF-spouse    Married-civ-spouse 
##                  4443                    23                 14976 
## Married-spouse-absent         Never-married             Separated 
##                   418                 10683                  1025 
##               Widowed 
##                   993

## CREATING NEW STANDARD COLUMNS AND GROUP EXISTING INTO APPROPIATE ONES
adult$marital_status <- gsub('Married-AF-spouse', 'Married', adult$marital_status)
adult$marital_status <- gsub('Married-civ-spouse', 'Married', adult$marital_status)
adult$marital_status <- gsub('Married-spouse-absent', 'Married', adult$marital_status)
adult$marital_status <- gsub('Never-married', 'Single', adult$marital_status)
adult$marital_status <- as.factor(adult$marital_status)
summary(adult$marital_status)
##  Divorced   Married Separated    Single   Widowed 
##      4443     15417      1025     10683       993


## INCOME FOR EACH CLASS OF OCCUPATION AS DF


df3 <- data.frame(table(adult$income, adult$marital_status))
names(df3) <- c('income', 'marital_status', 'count')
df3
##    income marital_status count
## 1   <=50K       Divorced  3980
## 2    >50K       Divorced   463
## 3   <=50K        Married  8681
## 4    >50K        Married  6736
## 5   <=50K      Separated   959
## 6    >50K      Separated    66
## 7   <=50K         Single 10192
## 8    >50K         Single   491
## 9   <=50K        Widowed   908
## 10   >50K        Widowed    85

## INCOME FOR EACH CLASS OF OCCUPATION AS BARPLOT

# calculate the percentages
df3 <- ddply(df3, .(marital_status), transform, percent = count/sum(count) * 100)

# format the labels and calculate their positions
df3 <- ddply(df3, .(marital_status), transform, pos = (cumsum(count) - 0.5 * count))
df3$label <- paste0(sprintf("%.0f", df3$percent), "%")

# bar plot of counts by marital status with in group proportions 
ggplot(df3, aes(x = marital_status, y = count, fill = income)) +
  geom_bar(stat = "identity") +
  geom_text(aes(y = pos, label = label), size = 2) + 
  ggtitle('Income Level with Marital Status')

￼




## HISTOGRAM OF CAPITAL GAIN

# histogram of capital_gain
ggplot(adult) + aes(x=as.numeric(capital_gain), group=income, fill=income) + 
  geom_histogram(bins=10, color='black') + ggtitle('Histogram of Capital Gain')

￼




## HISTOGRAM OF CAPITAL LOSS


# histogram of capital_loss
ggplot(adult) + aes(x=as.numeric(capital_loss), group=income, fill=income) + 
  geom_histogram(bins=10, color='black') + ggtitle('Histogram of Capital Loss')
￼




## PERCENTAGE OF OBSERVATIONS WITH NO CAPITAL GAIN OR LOSS

# percentage of observatiosn with no capital gain or loss
sum(adult$capital_gain == 0)/length(adult$capital_gain)
## [1] 0.9167102
sum(adult$capital_loss == 0)/length(adult$capital_loss)
## [1] 0.9533491


## PERCENTAGE OF OBSERVATIONS WITH NO CAPITAL GAIN OR LOSS
sum(adult$native_country == ‘ United States’)/length(adult$native_country)



## REMOVING COLUMNS WITH HIGH SKEWNESS

adult$capital_gain <- NULL
adult$capital_loss <- NULL
adult$native_country <- NULL


## INCOME FOR EACH CLASS OF RACE AS DF

df4 <- data.frame(table(adult$income, adult$race))
names(df4) <- c('income', 'race', 'count')
df4
##    income               race count
## 1   <=50K Amer-Indian-Eskimo   275
## 2    >50K Amer-Indian-Eskimo    36
## 3   <=50K Asian-Pac-Islander   763
## 4    >50K Asian-Pac-Islander   276
## 5   <=50K              Black  2737
## 6    >50K              Black   387
## 7   <=50K              Other   246
## 8    >50K              Other    25
## 9   <=50K              White 20699
## 10   >50K              White  7117



## INCOME FOR EACH CLASS OF RACE AS BARPLOT

# calculate the percentages
df4 <- ddply(df4, .(race), transform, percent = count/sum(count) * 100)

# format the labels and calculate their positions
df4 <- ddply(df4, .(race), transform, pos = (cumsum(count) - 0.5 * count))
df4$label <- paste0(sprintf("%.0f", df4$percent), "%")


## REMOVE INSTANCES OF CLASSES WITH VERY LOW OCCURENCE

# do not display percentage for low counts categories
df4$label[df4$race == 'Other'] <- NA
df4$label[df4$race == 'Amer-Indian-Eskimo'] <- NA

# bar plot of counts by marital status with in group proportions 
ggplot(df4, aes(x = race, y = count, fill = income)) +
  geom_bar(stat = "identity") +
  geom_text(aes(y = pos, label = label), size = 2) + 
  ggtitle('Income Level by Race')

￼




## SUMMARY OF ADULT DATASET

##       age                workclass     educatoin_num     marital_status 
##  Min.   :17.00   Government   : 4351   Min.   : 1.00   Divorced : 4443  
##  1st Qu.:28.00   Other/Unknown: 1857   1st Qu.: 9.00   Married  :15417  
##  Median :37.00   Private      :22696   Median :10.00   Separated: 1025  
##  Mean   :38.58   Self-Employed: 3657   Mean   :10.08   Single   :10683  
##  3rd Qu.:48.00                         3rd Qu.:12.00   Widowed  :  993  
##  Max.   :90.00                         Max.   :16.00                    
##          occupation                    race           sex       
##  Blue-Collar  :10062   Amer-Indian-Eskimo:  311   Female:10771  
##  Other/Unknown: 1852   Asian-Pac-Islander: 1039   Male  :21790  
##  Professional : 4140   Black             : 3124                 
##  Sales        : 3650   Other             :  271                 
##  Service      : 5021   White             :27816                 
##  White-Collar : 7836                                            
##  hours_per_week    income     
##  Min.   : 1.00   <=50K:24720  
##  1st Qu.:40.00   >50K : 7841  
##  Median :40.00                
##  Mean   :40.44                
##  3rd Qu.:45.00                
##  Max.   :99.00



