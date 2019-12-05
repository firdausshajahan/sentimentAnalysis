library(dplyr)

#this is where data is combine,set column, and remove null row
combineData2019 <- bind_rows(dataminer,dataminer2,dataminer3,dataminer4,dataminer5,dataminer6,dataminer7,dataminer8,dataminer9,dataminer10,dataminer11,dataminer12,dataminer13,dataminer14,dataminer15,dataminer16,dataminer17,dataminer18,dataminer19)
combineData2019 <- subset(combineData2019,select = c("name","comment","time"))
combineData2019 <- combineData2019 %>% na.omit()
write_csv(combineData2019, path = "C:/Users/seetrustudio-29/Documents/firdaus/studies/researchProject/SentimentAnalysis/data/cleanData/combineData2019.csv")

#this is where each year data is combine and exported all together
combineAllData <- bind_rows(combineData2017,combineData2018,combineData2019)
write_csv(combineAllData, path = "C:/Users/seetrustudio-29/Documents/firdaus/studies/researchProject/SentimentAnalysis/data/cleanData/combineAllData.csv")

#this is where, row id added, and unix timestamp change to ymd_hms format
dataWithIdDateTime <- combineAllData %>%
   mutate(id = row_number()) %>%
   select(id, everything()) %>%
   mutate(time = as_datetime(combineAllData$time, tz = "UTC"))

write_csv(dataWithIdDateTime, path = "C:/Users/seetrustudio-29/Documents/firdaus/studies/researchProject/SentimentAnalysis/data/cleanData/combineDataComplete.csv")
