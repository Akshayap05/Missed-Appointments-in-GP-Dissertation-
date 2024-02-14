library(dplyr)

#Reading my datasets
mydata <- read.csv2('GM2019-2021_official.csv', sep = ',', stringsAsFactors = T)

#Cleaning datasets

mydata$X <- NULL
mydata$stp_ons_code <- NULL

mydata <- mydata %>% distinct()
mydata$ccg_name[mydata$ccg_name == 'NHS Heywood Middleton and Rochdale CCG'] <- 'NHS Heywood Middleton & Rochdale CCG'

mydata$date <-NULL
mydata$countappt = as.numeric(as.character(mydata$countappt))

n<- mydata %>% group_by(ccg_code,ccg_name,year,month,apptstatus) %>% summarise_at(vars(countappt),list(countappt = sum))
n <- n %>% distinct()

app<- n %>% 
  pivot_wider(names_from = apptstatus, values_from = countappt)

app2 <- app[ -c(1:4) ]

app2<-cbind(app2, total = rowSums(app2))
totalF2F <- cbind(app, app2)
totalF2F <- totalF2F[ -c(8:10) ]
colnames(totalF2F) <- c("ccg_code", "ccg_name","Year","Month", "Attended","DNA", "Unknown", "Total") 
FINAL <- mutate(totalF2F, DNAPercentage = DNA /Total * 100)
colnames(FINAL)[8]  <- "Total"
colnames(FINAL)[9]  <- "DNA%"

FINAL <- FINAL[-c(37:42),]
FINAL <- FINAL[-c(73),]


FINAL1 <- FINAL[FINAL$ccg_name != "NHS Heywood Middleton and Rochdale CCG", ]

#Saving my database as a csv:

write.csv(FINAL1,"C:/Users/aksha/OneDrive/Desktop/data/COMPLETEFINAL.csv", row.names = FALSE)

FINAL <- FINAL[-c(31),]


#Analysis

require(dplyr)
df1 <- FINAL %>% group_by(ccg_code, ccg_name, Year, Month) %>% summarise(Attended = max(Attended), DNA = max(DNA), Unknown = max(Unknown))

df1<-FINAL %>% 
  group_by(ccg_code, Year, Month) %>% 
  arrange(Attended) %>%  
  slice(n())

write.csv(df1,"C:/Users/aksha/OneDrive/Desktop/greaterManchester/FINALGM19-21.csv", row.names = FALSE)

h<-F2F_GP_2[grepl('Boston', F2F_GP_2$ccg_name), ]

library(data.table)
h<-F2F_GP_2[F2F_GP_2$ccg_name %like% "Boston", ]

F2F_GP_2<-subset(F2F_GP_2, ccg_name == "Bury" & ccg_name == 'Bolton' & ccg_name == 'Rochdale' & ccg_name == 'Oldham' & ccg_name == 'Wigan' & ccg_name == 'Salford' & ccg_name == 'Trafford' & ccg_name == 'Manchester' & ccg_name == 'Stockport' & ccg_name == 'Tameside')

#All the greater manchester regions in this dataset:
#Bury|Bolton|Rochdale|Oldham|Wigan|Salford|Trafford|Manchester|Stockport|Tameside'
