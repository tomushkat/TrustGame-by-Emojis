

colnames(originalData)[6] <- 'Duration'



                              

#Remove index rows: 
data_clean <- originalData[3:nrow(originalData), ]
data_clean <- data_clean %>% 
  mutate(Duration = as.numeric(Duration) / 60) %>% 
  filter(anytime(StartDate) > '2021-07-28')
         
data_clean <- data_clean %>% 
  mutate(emCondition1 = paste0(FL_7_DO_Manyemojis, FL_7_DO_Fewemojis, FL_7_DO_Noemojis),
         emCondition = ifelse(emCondition1 == '1NANA', 'Many',
                              ifelse(emCondition1 == 'NA1NA', 'Single',
                                     ifelse(emCondition1 == 'NANA1', 'No', NA))),
         emCondition1 = NULL,
         Order1 = paste0(FL_8_DO_DVamountfirst, FL_8_DO_DVquestionfirst),
         Order = ifelse(Order1 == '1NA', 'amountFirst',
                        ifelse(Order1 == 'NA1','questionFirst',  NA)),
         Order1 = NULL,
         FL_7_DO_Manyemojis = NULL,
         FL_7_DO_Fewemojis = NULL,
         FL_7_DO_Noemojis = NULL,
         FL_8_DO_DVamountfirst = NULL,
         FL_8_DO_DVquestionfirst = NULL)

#Leave only actual data columns (removing the default Quadratics irrelevant information):
irrelevant_colnames <- colnames(data_clean)[c(1:5, 8:17, 19:28)]
data_clean <- select(data_clean, -irrelevant_colnames)
view(data_clean)


data_clean1 <- data_clean %>% 
  filter(Finished == TRUE)

#subset to exclude outliers: 
#1. Those that were not willing/ able to participate
data_clean2 <- data_clean1 %>% 
  filter(Alone == 'Alone')

#Those that did not complete the survey consequtively: 
data_clean3 <- data_clean2 %>% 
  filter(Consecutively == 'Yes')

#Examine the disturvances that the participants experienced:
data_clean3 %>% 
  filter(Disturbances != 'No')

view(data_clean3 %>% 
       filter(Disturbances != 'No'))
view(data_clean)


# Removing timing outliers
data_clean4 <- data_clean3 %>% 
  filter(Duration > 0)

data_clean4 %>% filter(Gender != mf)

data_clean4 %>% select(Gender, mf)

data_clean5 <- data_clean4 %>% 
  mutate(Amount = ifelse(is.na(amountAF_4) == FALSE, amountAF_4, amountQF_4),
         Trust = ifelse(is.na(trustAF_1) == FALSE, trustAF_1, trustQF_6),
         Amount = as.numeric(Amount),
         Trust = as.numeric(Trust),
         scaledAmount = scale(Amount),
         scaledTrust = scale(Trust),
         meandDV = colMeans(scaledAmount, scaledTrust))

cleanData <- data_clean5


originalData <- NULL
data_clean <- NULL
data_clean1 <- NULL
data_clean2 <- NULL
data_clean3 <- NULL
data_clean4 <- NULL

view(cleanData)

write.csv(cleanData, 'cleanData.csv')
