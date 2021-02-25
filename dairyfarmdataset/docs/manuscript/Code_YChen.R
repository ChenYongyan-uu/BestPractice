if (!require("data.table")) {
  install.packages("data.table", dependencies = TRUE)
  library(data.table)
}

if (!require("dplyr")) {
  install.packages("dplyr", dependencies = TRUE)
  library(dplyr)
}

Data<-fread('./data/dataset_Y Chen.csv')
glimpse(Data)

# All date column change to date format

if (!require("lubridate")) {
  install.packages("lubridate", dependencies = TRUE)
  library(lubridate)
}
Data$CalvingDate<-mdy(Data$CalvingDate)
Data$TestDate<-mdy(Data$TestDate)
Data$TestYear<-year(Data$TestDate)
glimpse(Data)

# Calculate DaysInMilk

Data<-Data %>%
  mutate(DaysInMilk=as.numeric(TestDate-CalvingDate))

# Descriptive analysis
summary(Data)
hist(Data$MilkKG)
hist(Data$FatPercentage)
hist(Data$ProteinPercentage)
hist(Data$DaysInMilk)
n_distinct(Data$HerdId)
n_distinct(Data$CowId)

# exclude extreme value
FilteredData<-Data %>%
  filter(ProteinPercentage>quantile(ProteinPercentage,0.01),
         ProteinPercentage<quantile(ProteinPercentage,0.99),
         FatPercentage>quantile(FatPercentage,0.01),
         FatPercentage<quantile(FatPercentage,0.99),
         DaysInMilk<quantile(DaysInMilk,0.99)
         )

# Descriptive analysis
summary(FilteredData)
hist(FilteredData$MilkKG)
hist(FilteredData$FatPercentage)
hist(FilteredData$ProteinPercentage)
hist(FilteredData$DaysInMilk)
n_distinct(FilteredData$HerdId)
n_distinct(FilteredData$CowId)

# herd summary

HerdSummary<-FilteredData%>%
  group_by(HerdId,TestYear)%>%
  summarise(HerdSize=n_distinct(CowId),
            AverageMilkProduction=mean(MilkKG),
            AverageProteinPercentage=mean(ProteinPercentage),
            AverageFatPercentage=mean(FatPercentage),
            AverageParity=mean(Parity))%>%
  ungroup()

# Plot
if (!require("ggplot2")) {
  install.packages("ggplot2", dependencies = TRUE)
  library(ggplot2)
}

ggplot(HerdSummary, aes(TestYear, AverageMilkProduction)) +
  geom_line(aes(col = HerdId, group = HerdId))+
  geom_point(aes(col = HerdId))

ggplot(HerdSummary, aes(TestYear, HerdSize)) +
  geom_line(aes(col = HerdId, group = HerdId))+
  geom_point(aes(col = HerdId))

if (!require("linter")) {
  install.packages("linter", dependencies = TRUE)
  library(linter)
}

install.packages("styler")
