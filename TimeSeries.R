library(scales)
library(xts)
library(dplyr)
library(tidyverse)
library(readxl)
library(lubridate)
library(writexl)
library(timeDate)
library(rlist)
library(ggforce)
library(ggrepel)
library(fable)
library(tsibble)
library(feasts)
library(urca)

#Script Parameters and Tools
RunNeuralNet <- FALSE
NeuralnetIterations <- 20
rollperiod <- 6
forecastperiod <- 36
numberyearsOPDdata <- 8
filteroutspecials <- c("Pallitive Care", "Diabetic Day Centre",
                       "Chiropody", "Neurophysiology", "General Medical",
                       "OMNITEST", "Oncology Radiation", "Detoxification",
                       "Microbiology", "Maxillo-Facial",
                       "Orthoptics")
'%!in%' <- function(x,y)!('%in%'(x,y))
ReferralFile <- "OPD Referrals.xlsx"
ReferralsSheet <- "Referrals"
Activityfile <- "OPDActivityfromDiver.xlsx"

#Create Directorates
Medical <- c("Cardiology", "Dermatology", "Endocrinology", "Gastroenterology", "Geriatrics", "Haematology",
             "Immunology", "Infectious Diseases", "Oncology Medical", "Respiratory Medicine", "Rheumatology")
TUN <- c("Urology", "Nephrology")
NeuroCent <- c("Neurosurgery", "Neurology", "Ophthalmology", "E.N.T.")
Surgical <- c("Breast Surgery", "General Surgical", "Gynaecology", "Orthopaedics", "Plastic Surgery", "Vascular Surgery")
CCandA <- c("Anaesthetics", "Pain Relief")


directorates <- list(Medical, TUN, NeuroCent, Surgical, CCandA)
names(directorates) <- c("Medical", "TUN", "NeuroCent", "Surgical", "CCandA")

#Pull in Referrals grouped by Specialty
Referrals <- read_excel(ReferralFile, 
                        sheet = ReferralsSheet)
Referrals <- Referrals[-1,]
Referrals$WeekEnding <- str_remove(Referrals$WeekEnding, "Week Ending ")
Referrals$WeekEnding <- ymd(Referrals$WeekEnding)
Referrals$Period <- as.Date(timeLastDayInMonth(ymd(paste(Referrals$Period,"/01",sep="/"))))

Referrals <- select(Referrals, MonthEnding = Period, WeekEnding, BookSpecialty, BookConsultant, Outcome, Referrals)

#set the end period by rolling back to previous month end from the max week
fullmonthmax <- rollback(max(Referrals$WeekEnding))

# Manual Set Full Month Max
#fullmonthmax <- date("2019-05-31")


#filter out unwanted
Referrals <- Referrals %>%
  filter(Outcome == "Seen",
         year(MonthEnding) != 2011,
         MonthEnding <= fullmonthmax) %>%
  filter(BookSpecialty %!in% filteroutspecials) %>%
  group_by( MonthEnding, BookSpecialty) %>%
  summarise(Referrals = sum(Referrals, na.rm = TRUE))

#OPD dataframe
OPD <- lapply(1:numberyearsOPDdata, function(i) read_excel(Activityfile, sheet = i))
OPD <- bind_rows(OPD)

#Tidy up OPD Dataframe
OPD$WeekEnding <- str_remove(OPD$WeekEnding, "Week Ending ")

OPD$NewReturn <- str_remove(OPD$NewReturn, " PATIENTS :")

OPD$WeekEnding <- ymd(OPD$WeekEnding)
OPD$Period <- as.Date(timeLastDayInMonth(ymd(paste(OPD$Period,"/01",sep="/"))))

OPD <- filter(OPD, WeekEnding <= fullmonthmax) %>%
  select(MonthEnding = Period, BookSpecialty, NewReturn, OPD = `O/P Count`, DNA = 'DNA Count') %>%
  filter(BookSpecialty %!in% filteroutspecials) %>%
  group_by(MonthEnding, BookSpecialty, NewReturn) %>%
  summarise(OPD = sum(OPD, na.rm = TRUE),
            DNA = sum(DNA, na.rm = TRUE))

OPD$NewReturn <- as.factor(OPD$NewReturn)


OPDWIP <- NULL




#Get the News
OPDWIP$News <- OPD %>%
  filter(NewReturn == "NEW") %>%
  select(MonthEnding, BookSpecialty, NewOPD = OPD, NewDNA = DNA)

#Get the Retunrs
OPDWIP$Returns <- OPD %>%
  filter(NewReturn == "RETURN") %>%
  select(MonthEnding, BookSpecialty, ReturnOPD = OPD, ReturnDNA = DNA)

#Join them Together
OPD <- full_join(OPDWIP$News, OPDWIP$Returns, by = c("MonthEnding", "BookSpecialty"))
remove(OPDWIP)

#Join them to the Referrals
OPD <- full_join(OPD, Referrals, by = c("MonthEnding", "BookSpecialty"))
remove(Referrals)
OPD[is.na(OPD)] <- 0
OPD$MonthEnding <- yearmonth(OPD$MonthEnding)



#Split the OPD data based on Specialty
OPD <- split.data.frame(OPD, OPD$BookSpecialty)

OPD <- lapply(OPD, as_tsibble)

CreateModels <- function(x) {
  x %>%
  model(
    arima = ARIMA(Referrals, stepwise = TRUE, approximation = FALSE)
  )
}

#Create ARIMA models ----
OPDModels <- lapply(OPD, CreateModels)

CreateForecasts <- function(x){
  x %>%
    forecast(h = forecastperiod) %>%
    hilo(level = 80) %>%
    unnest(`80%`)
}

#Create Forecasts
ForecastOutput <- lapply(OPDModels, CreateForecasts)


#Fix Column Names function
fixnames <- function(x) {
  x <- x[,2:5]
  x <- setNames(x, c("MonthEnding", "Forecast", "80% Lower", "80% Upper"))
}

ForecastOutput <- lapply(ForecastOutput, fixnames)

combinedforecasts <- bind_rows(ForecastOutput, .id = "BookSpecialty")

#Create function for rolling means
CreateRollingMeans <- function(x,y) {
  DNAAvg <- rollmean(x$NewDNA, y, fill = "extend", align = "right")
  ApptsAvg <- rollmean(x$NewOPD, y, fill = "extend", align = "right")
  OvCapAvg <- rollmean(x$NewDNA, y, fill = "extend", align = "right") + rollmean(x$NewOPD, y, fill = "extend", align = "right")
  cbind.data.frame(x,DNAAvg, ApptsAvg, OvCapAvg)
}
#Create Rolling Means for set period and make a data frame
OPD <- lapply(OPD, CreateRollingMeans, y = rollperiod)
combinedOPD <- bind_rows(OPD)


#combine the rolling means data frame and forecasts
combined <- bind_rows(combinedOPD, combinedforecasts)


#Arrange the combined data frame and rename the date to Month Ending
combined <- arrange(combined, BookSpecialty, MonthEnding)


#Roll last available rolling average forward with the predictions
OPDtest <- split.data.frame(combined, combined$BookSpecialty)

OPDtestfunc  <- function(x) {
  DNAAvg <- x$DNAAvg[(nrow(x)-(forecastperiod-1)):nrow(x)] <- x$DNAAvg[(nrow(x)-forecastperiod)]
  ApptsAvg <- x$ApptsAvg[(nrow(x)-(forecastperiod-1)):nrow(x)] <- x$ApptsAvg[(nrow(x)-forecastperiod)]
  OvCapAvg <- x$OvCapAvg[(nrow(x)-(forecastperiod-1)):nrow(x)] <- x$DNAAvg[(nrow(x)-forecastperiod)] + x$ApptsAvg[(nrow(x)-forecastperiod)]
  cbind.data.frame(x[(nrow(x)-(forecastperiod-1)):nrow(x),], DNAAvg, ApptsAvg, OvCapAvg)
}
OPDtest <- lapply(OPDtest, OPDtestfunc)
OPDtest <- bind_rows(OPDtest)

#Filter out the incomplete averages and replace with the last period rolled forward
combined <- combined %>%
  filter(is.na(DNAAvg) == FALSE) %>%
  bind_rows(OPDtest) %>%
  arrange(BookSpecialty, MonthEnding)


#Sum Directorates
medicaltally <- combined %>%
  ungroup() %>%
  filter(BookSpecialty %in% Medical) %>%
  mutate(BookSpecialty = "Medical") %>%
  group_by(MonthEnding, BookSpecialty) %>%
  summarise_all(sum)

 TUNtally <- combined %>%
  ungroup() %>%
  filter(BookSpecialty %in% TUN) %>%
  mutate(BookSpecialty = "TUN") %>%
  group_by(MonthEnding, BookSpecialty) %>%
  summarise_all(sum)
 
 NeuroCentTally <- combined %>%
   ungroup() %>%
   filter(BookSpecialty %in% NeuroCent) %>%
   mutate(BookSpecialty = "NeuroCent") %>%
   group_by(MonthEnding, BookSpecialty) %>%
   summarise_all(sum)
 
 Surgicaltally <- combined %>%
   ungroup() %>%
   filter(BookSpecialty %in% Surgical) %>%
   mutate(BookSpecialty = "Surgical") %>%
   group_by(MonthEnding, BookSpecialty) %>%
   summarise_all(sum)
 

 CCandAtally <- combined %>%
   ungroup() %>%
   filter(BookSpecialty %in% CCandA) %>%
   mutate(BookSpecialty = "CC&A") %>%
   group_by(MonthEnding, BookSpecialty) %>%
   summarise_all(sum)
 
 combined <- bind_rows(combined, medicaltally, NeuroCentTally, Surgicaltally, TUNtally, CCandAtally)

 
#Output it all to an Excel file
write_xlsx(combined, "DiverForecasts.xlsx", col_names = TRUE)
save(combined, file = "DiverForecasts.RData")
save(combined, file = "Shiny/DiverForecasts.RData")