#Filtering data and organising by appointment type
library(dplyr)
mydata <- read.csv2("mydata.csv", sep = ',', stringsAsFactors = T)
mydata$X <- NULL
mydata$stp_ons_code <- NULL

F2F_GP<-subset(mydata, apptmode == "Face-to-Face" & hcptp == 'GP')
F2F_GP<- F2F_GP %>% distinct()
F2F_GP$time2appt <-NULL

F2F_GP <- F2F_GP[ -c(2:6, 8:9) ]
F2F_GP$date <-NULL

F2F_GP <- F2F_GP[F2F_GP$year %in% c('2019','2020','2021'),]

F2F_GP$countappt = as.numeric(as.character(F2F_GP$countappt))

n<- F2F_GP %>% group_by(ccg_code,ccg_name,year,month,apptstatus) %>% summarise_at(vars(countappt),list(countappt = sum))
n <- n %>% distinct()
library(tidyr)
app<- n %>% 
  pivot_wider(names_from = apptstatus, values_from = countappt)

app2 <- app[ -c(1:3) ]

app2<-cbind(app2, total = rowSums(app2))
totalF2F <- cbind(app, app2)
totalF2F <- totalF2F[ -c(4:6) ]
totalF2F <- totalF2F[ -c(8:11) ]

#Statistics
colnames(totalF2F) <- c("ccg_code", "ccg_name","Year","Month", "Attended","DNA", "Unknown", "Total") 
FINAL <- mutate(totalF2F, DNAPercentage = DNA /Total * 100)
colnames(FINAL)[9]  <- "DNA%"
FINAL <- mutate(FINAL, AttendedPercentage = Attended /Total * 100)
colnames(FINAL)[10]  <- "Attended%"
FINAL <- mutate(FINAL, UnknownPercentage = Unknown /Total * 100)
colnames(FINAL)[10]  <- "Unknown%"


write.csv(FINAL,"C:/Users/aksha/OneDrive/Desktop/data/FINALDATASET.csv", row.names = FALSE)

