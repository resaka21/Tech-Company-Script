#Tech Company Project  - R Script
#Dr. Diana Young - BAT 3303
#Taku Charles-Noel Endo, Rei Esaka, & Thomás Peña


#Loading common packages with functions we will use throughout this project
library(readxl)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)

#Import "Raw-Data (Class Use)" and "Cost Details" data tables into environment
Raw_Data <- read_xlsx("Student Data for Distribution-3.xlsx", sheet = "Raw-Data (Class Use)")
Cost_Details <- read_xlsx("Student Data for Distribution-3.xlsx", sheet = "Cost Details")

#Counting the rows in each data table
count(Raw_Data) #9151 rows within Raw-Data (Class Use) data table
count(Cost_Details) #9 rows within Cost Details data table

#Trying to see the structures of both data sets before we begin cleaning them
str(Raw_Data) #mixture of character and date structures
str(Cost_Details) #mixture of character and date structures
view(Raw_Data) #viewing raw data table within R
view(Cost_Details) #viewing cost details table within R


# Data Cleaning

#Renaming the columns to remove the spaces between all of the variables. 
Raw_Data <- rename(Raw_Data, c(Ship_Mode = `Ship Mode`))
Raw_Data <- rename(Raw_Data, c(PO_Download_Date = `PO Download Date`))
Raw_Data <- rename(Raw_Data, c(Ship_Date = `Ship Date`))
Raw_Data <- rename(Raw_Data, c(Receipt_Date = `Receipt Date`))
Raw_Data <- rename(Raw_Data, c(Ship_Qtr = `Ship Qtr`))
Cost_Details <- rename(Cost_Details, c(Ship_Mode = `Ship Mode`))

#Changing the categorical data from characters to factors. 
Raw_Data$LOB <- as.factor(Raw_Data$LOB)
Raw_Data$Origin <- as.factor(Raw_Data$Origin)
Raw_Data$Ship_Mode <- as.factor(Raw_Data$Ship_Mode)
Raw_Data$Origin <- as.factor(Raw_Data$Origin)
Raw_Data$Ship_Qtr <- as.factor(Raw_Data$Ship_Qtr)

#It is first useful to see how the raw state of data is in order to figure out our plan to clean and transform the data.
#We will dplyr to check which/how much levels of each categorical variable in the data exist.
Count_Ship_Mode <- Raw_Data %>% 
  count(Ship_Mode) ##Counting much how of each level of ship mode is within the data
Count_Ship_Mode # 4 levels exist: AIR, FASTBOAT, GROUND & OCEAN. 3829 records for AIR, 532 records for FASTBOAT, 2443 records for GROUND, and 2347 for OCEAN
Count_LOB <- Raw_Data %>%
  count(LOB) ##Counting how much of each level of LOB is within the data
Count_LOB #3 levels exist: Product A, Product B, and Product C. 2957 records for Product A, 5932 records for Product B, 262 records for Product C
Count_Origin <- Raw_Data %>%
  count(Origin) ##Counting how much of each level of LOB is within the data
Count_Origin #4 levels exists: Site A, Site B, Site C, and Site D. 3477 records for Site A, 1572 records for Site B, 1659 records for Site C, and 2443 records for Site D
Count_Qtr <- Raw_Data %>%
  count(Ship_Qtr) ##Counting how much of each level of Ship_Qtr is within the data
Count_Qtr #3 levels exist: Q1, Q2, & Q4. Q4 is from FY2019, and Q1 & Q2 are from FY2020

#Now that we know what our given data looks like, we will go into the 4 conditions we designated would invalidate data.

#'''INVALID DATA CONDITION #1'''
#We understand the duplicated data would affect the linear regression model that we will be creating further into the project.
#Thus, any data that is found to be duplicated need to be removed. Only to have unique data will be kept.
count(Raw_Data[duplicated(Raw_Data) == TRUE, ])   #Looks like we have 6849 duplicated rows in the Raw_Data data set. 
count(Raw_Data[duplicated(Raw_Data) == TRUE, ]) / count(Raw_Data) #74.84 % of our data will be erased since it is duplicated.
#We will erase the duplicated data and assigned it to the existing dataframe, Raw_Data. 
Raw_Data <- distinct(Raw_Data)
count(Raw_Data)   #There are now 2302 rows of distinct data within the raw data. We have lost majority of our initial records. 
#Now no more invalid duplicated data is found within the remainder of this dataset

#'''INVALID DATA CONDITION #2'''
#From our understanding of the provided Tech Company data, PO Download data must take place first, then the ship date, and lastly the receipt date.
#Thus, one of the invalid conditions are when PO_DownLoad_Date > Ship_Date > Receipt Date
#let's filter them out. 
Raw_Data %>%
  filter(PO_Download_Date >= Ship_Date | PO_Download_Date >= Receipt_Date | Ship_Date >= Receipt_Date) #We have 402 rows that are invalid.

#'''INVALID DATA CONDITION #3'''
#In talking with the Tech Company Team, we were able to understand that their FY calender schedule is shifted by one month,
#so Q1 would be between FEB-APR, Q2 would be between MAY-JUL, Q3 would be between AUG-OCT, and Q4 would be between NOV-JAN.
#Through exploring the raw data seeing that we were only provided data from Q4 of FY2019 and Q1 & Q2 of FY2020, any date that comes before 11/01/2019 is invalid.
#Furthermore, we are assuming that any data before the Tech Company Team presented this case study to the class would be invalid as well. For safety measures, we will cap valid dates to be 10 days before the Tech Company Team came to our class.
#Thus, any date before 2020/09/20 will be invalid. 

#Filtering out data according to our two new invalid conditions to PO_Download_Date, Ship_Date, and Receipt_Date
#Range Constraints for PO_Download_Date column
POD_Invalid <- Raw_Data %>%
  filter(PO_Download_Date < "2019-11-01" | PO_Download_Date > "2020-09-20") 
POD_Invalid #No record meets the condition for PO_Download_Date, all dates are valid

#Range Constraints for Ship Date column
ShipDate_Invalid <- Raw_Data %>%
  filter(Ship_Date < "2019-11-01" | Ship_Date > "2020-09-20") 
ShipDate_Invalid #70 Ship date records are invalid

#Range Constraints for Receipt Date column
ReceiptDate_Invalid <- Raw_Data %>%
  filter(Receipt_Date  < "2019-11-01" | Receipt_Date  > "2020-09-20") 
ReceiptDate_Invalid #No record meets the conditions for Receipt Date, all dates are valid

#'''INVALID CONDITION #4'''
#Rows that include NA's are considered invalid since they prove to be problematic, do not provide any significant insights, and need to be transformed to useful information.
summary(Raw_Data) #Using summary() we see that there are 150 rows across both Ship_Date and Receipt_Date that include NA's.

#'''CLEANING INVALID DATA'''
#In approaching data cleaning, we are using PO_Download_Date as the base of valid information
#To address the invalid dates that break the precedent order of PO Download Date, Ship Date, then Receipt Date, we want to find the median Manufacturing Lead Time (MLT) and Transit Lead Time (TLT) using valid dates for the rows that are invalid, then replace them with valid dates. 
#To do this, we need to understand the relations MLT and TLT has with the data set variables.
#Since TLT is dependent upon ship modes and origins, the median TLT has to be calculated separately for each combination of Ship_Mode and Origin.
#Since MTL is dependent upon LOBs and origins, the median MLT has to be calculated separately for each combination of LOB and Origin.

#We can see which combination of LOB and Origin exists in our data set. 
ggplot(Raw_Data, aes(x = Origin, fill = LOB)) + geom_bar() 
##According to the graph, we have six combinations: Site A:Product A, Site A:Product B, Site A:Product C, Site B:Product B, Site C:Product B, and Site D:Product B

#We will now see how many invalid Ship_Dates we have on our Raw_Data by checking every combination of Origin and LOB under our stated invalid data conditions.
#SITE A: Product A
count(Raw_Data[Raw_Data$Origin == "Site A" & Raw_Data$LOB == "Product A" & 
                 (is.na(Raw_Data$Ship_Date) | Raw_Data$Ship_Date <= Raw_Data$PO_Download_Date 
                  | Raw_Data$Ship_Date >= Raw_Data$Receipt_Date | Raw_Data$Ship_Date < "2019-11-01" | Raw_Data$Ship_Date > "2020-09-20"), ]) #164 invalid dates. 

#SITE A: Product B
count(Raw_Data[Raw_Data$Origin == "Site A" & Raw_Data$LOB == "Product B" & 
                 (is.na(Raw_Data$Ship_Date) | Raw_Data$Ship_Date <= Raw_Data$PO_Download_Date 
                  | Raw_Data$Ship_Date >= Raw_Data$Receipt_Date | Raw_Data$Ship_Date < "2019-11-01" | Raw_Data$Ship_Date > "2020-09-20"), ]) #23 invalid dates. 

#SITE A: Product C
count(Raw_Data[Raw_Data$Origin == "Site A" & Raw_Data$LOB == "Product C" & 
                 (is.na(Raw_Data$Ship_Date) | Raw_Data$Ship_Date <= Raw_Data$PO_Download_Date 
                  | Raw_Data$Ship_Date >= Raw_Data$Receipt_Date | Raw_Data$Ship_Date < "2019-11-01" | Raw_Data$Ship_Date > "2020-09-20"), ]) #11 invalid dates. 

#SITE B: Product B
count(Raw_Data[Raw_Data$Origin == "Site B" & Raw_Data$LOB == "Product B" & 
                 (is.na(Raw_Data$Ship_Date) | Raw_Data$Ship_Date <= Raw_Data$PO_Download_Date 
                  | Raw_Data$Ship_Date >= Raw_Data$Receipt_Date | Raw_Data$Ship_Date < "2019-11-01" | Raw_Data$Ship_Date > "2020-09-20"), ]) #83 invalid dates. 

#SITE C: Product B
count(Raw_Data[Raw_Data$Origin == "Site C" & Raw_Data$LOB == "Product B" & 
                 (is.na(Raw_Data$Ship_Date) | Raw_Data$Ship_Date <= Raw_Data$PO_Download_Date 
                  | Raw_Data$Ship_Date >= Raw_Data$Receipt_Date | Raw_Data$Ship_Date < "2019-11-01" | Raw_Data$Ship_Date > "2020-09-20"), ]) #132 invalid dates. 

#SITE D: Product B
count(Raw_Data[Raw_Data$Origin == "Site D" & Raw_Data$LOB == "Product B" & 
                 (is.na(Raw_Data$Ship_Date) | Raw_Data$Ship_Date <= Raw_Data$PO_Download_Date 
                  | Raw_Data$Ship_Date >= Raw_Data$Receipt_Date | Raw_Data$Ship_Date < "2019-11-01" | Raw_Data$Ship_Date > "2020-09-20"), ]) #139 invalid dates. 

##Now knowing how many invalid dated exist in each combination, we need to find the median MLT using the valid days in each combination.

#Site A / Product A MLT = 6 days
SA_PA_MLTMedian <- Raw_Data %>%
  filter(Origin ==  "Site A" & LOB == "Product A" & as.numeric(Ship_Date - PO_Download_Date) > 0 & Ship_Date > "2019-11-01" & Ship_Date < "2020-09-20") %>%
  summarise(median = median(as.numeric(Ship_Date - PO_Download_Date))) 

#Site A / Product B MLT = 7 days
SA_PB_MLTMedian <- Raw_Data %>%
  filter(Origin ==  "Site A" & LOB == "Product B" & as.numeric(Ship_Date - PO_Download_Date) > 0 & Ship_Date > "2019-11-01" & Ship_Date < "2020-09-20") %>%
  summarise(median = median(as.numeric(Ship_Date - PO_Download_Date))) 


#Site A / Product C MLT = 10 days
SA_PC_MLTMedian <- Raw_Data %>%
  filter(Origin ==  "Site A" & LOB == "Product C" & as.numeric(Ship_Date - PO_Download_Date) > 0 & Ship_Date > "2019-11-01" & Ship_Date < "2020-09-20") %>%
  summarise(median = median(as.numeric(Ship_Date - PO_Download_Date))) 

#Site B / Product B MLT = 12 days
SB_PB_MLTMedian <- Raw_Data %>%
  filter(Origin ==  "Site B" & LOB == "Product B" & as.numeric(Ship_Date - PO_Download_Date) > 0 & Ship_Date > "2019-11-01" & Ship_Date < "2020-09-20") %>%
  summarise(median = median(as.numeric(Ship_Date - PO_Download_Date))) 

#Site C / Product B MLT = 5 days 
SC_PB_MLTMedian <- Raw_Data %>%
  filter(Origin ==  "Site C" & LOB == "Product B" & as.numeric(Ship_Date - PO_Download_Date) > 0 & Ship_Date > "2019-11-01" & Ship_Date < "2020-09-20") %>%
  summarise(median = median(as.numeric(Ship_Date - PO_Download_Date))) 

#Site D / Product B MLT = 8 days
SD_PB_MLTMedian <- Raw_Data %>%
  filter(Origin ==  "Site D" & LOB == "Product B" & as.numeric(Ship_Date - PO_Download_Date) > 0 & Ship_Date > "2019-11-01" & Ship_Date < "2020-09-20") %>%
  summarise(median = median(as.numeric(Ship_Date - PO_Download_Date))) 

#We will use the calculated median MLT later in the code to replace the invalid ship dates into valid dates using their given combination's median MLT.

#As mentioned before, because Transit Lead Time (TLT) is dependent upon LOBs and origins, the median TLT has to be calculated separately for each combination of LOB and Origin.
#It is helpful to know which combination of Origin and Ship_Mode exists.
ggplot(Raw_Data, aes(x = Ship_Mode, fill = Origin)) + geom_bar()  
#According the the graph we have six combinations: Site A:Air, Site A:Ocean, Site A:Fastboat, Site B:Ocean, Site C:Air, and Site D:Ground
Raw_Data[(Raw_Data$Ship_Mode == "AIR" & Raw_Data$Origin == "Site D") | 
           (Raw_Data$Ship_Mode == "FASTBOAT" & Raw_Data$Origin != "Site A") | 
           (Raw_Data$Ship_Mode == "GROUND" & Raw_Data$Origin != "Site D") |
           (Raw_Data$Ship_Mode == "OCEAN" & (Raw_Data$Origin == "Site C" | Raw_Data$Origin == "Site D")), ]
#We have confirmed the output from the ggplot.


#We will now see how much invalid Receipt_Dates we have on our Raw_Data
#SITE A: Air
count(Raw_Data[Raw_Data$Origin == "Site A" & Raw_Data$Ship_Mode == "AIR" & 
                 (is.na(Raw_Data$Receipt_Date) | Raw_Data$Receipt_Date <= Raw_Data$PO_Download_Date 
                  | Raw_Data$Receipt_Date <= Raw_Data$Ship_Date | Raw_Data$Receipt_Date < "2019-11-01" | Raw_Data$Receipt_Date > "2020-09-20"), ]) #84 invalid dates. 

#SITE A: FastBoat
count(Raw_Data[Raw_Data$Origin == "Site A" & Raw_Data$Ship_Mode == "FASTBOAT" & 
                 (is.na(Raw_Data$Receipt_Date) | Raw_Data$Receipt_Date <= Raw_Data$PO_Download_Date 
                  | Raw_Data$Receipt_Date <= Raw_Data$Ship_Date | Raw_Data$Receipt_Date < "2019-11-01" | Raw_Data$Receipt_Date > "2020-09-20"), ]) #13 invalid dates. 

#SITE A: Ocean
count(Raw_Data[Raw_Data$Origin == "Site A" & Raw_Data$Ship_Mode == "OCEAN" & 
                 (is.na(Raw_Data$Receipt_Date) | Raw_Data$Receipt_Date <= Raw_Data$PO_Download_Date 
                  | Raw_Data$Receipt_Date <= Raw_Data$Ship_Date | Raw_Data$Receipt_Date < "2019-11-01" | Raw_Data$Receipt_Date > "2020-09-20"), ]) #22 invalid dates. 

#SITE B: Air
count(Raw_Data[Raw_Data$Origin == "Site B" & Raw_Data$Ship_Mode == "AIR" & 
                 (is.na(Raw_Data$Receipt_Date) | Raw_Data$Receipt_Date <= Raw_Data$PO_Download_Date 
                  | Raw_Data$Receipt_Date <= Raw_Data$Ship_Date | Raw_Data$Receipt_Date < "2019-11-01" | Raw_Data$Receipt_Date > "2020-09-20"), ]) #1 invalid dates. 

#SITE B: Ocean
count(Raw_Data[Raw_Data$Origin == "Site B" & Raw_Data$Ship_Mode == "OCEAN" & 
                 (is.na(Raw_Data$Receipt_Date) | Raw_Data$Receipt_Date <= Raw_Data$PO_Download_Date 
                  | Raw_Data$Receipt_Date <= Raw_Data$Ship_Date | Raw_Data$Receipt_Date < "2019-11-01" | Raw_Data$Receipt_Date > "2020-09-20"), ]) #35 invalid dates. 

#SITE C: Air
count(Raw_Data[Raw_Data$Origin == "Site C" & Raw_Data$Ship_Mode == "AIR" & 
                 (is.na(Raw_Data$Receipt_Date) | Raw_Data$Receipt_Date <= Raw_Data$PO_Download_Date 
                  | Raw_Data$Receipt_Date <= Raw_Data$Ship_Date | Raw_Data$Receipt_Date < "2019-11-01" | Raw_Data$Receipt_Date > "2020-09-20"), ]) #60 invalid dates. 

#SITE D: Ground
count(Raw_Data[Raw_Data$Origin == "Site D" & Raw_Data$Ship_Mode == "GROUND" & 
                 (is.na(Raw_Data$Receipt_Date) | Raw_Data$Receipt_Date <= Raw_Data$PO_Download_Date 
                  | Raw_Data$Receipt_Date <= Raw_Data$Ship_Date | Raw_Data$Receipt_Date < "2019-11-01" | Raw_Data$Receipt_Date > "2020-09-20"), ]) #111 invalid dates. 

#Now, we will find the median TLT based in valid rows for each combination of Ship mode and Site Location.
#To filter out the valid dates, we will once again use our stated invalid data conditions to ONLY include valid data in the median TLT calculation.
#SITE A / Air TLT = 8 days
SiteA_Air_TLTMedian  <- Raw_Data %>% 
  filter(Origin == "Site A" & Ship_Mode == "AIR" & as.numeric(Receipt_Date - Ship_Date) > 0 & Receipt_Date > "2019-11-01" & Receipt_Date < "2020-09-20" & Ship_Date > "2019-11-01" & Ship_Date < "2020-09-20") %>%
  summarise(median = median(as.numeric(Receipt_Date - Ship_Date)))

#Site A / FastBoat TLT = 25 days
SiteA_Fastboat_TLTMedian  <- Raw_Data %>% 
  filter(Origin == "Site A" & Ship_Mode == "FASTBOAT" & as.numeric(Receipt_Date - Ship_Date) > 0 & Receipt_Date > "2019-11-01" & Receipt_Date < "2020-09-20" & Ship_Date > "2019-11-01" & Ship_Date < "2020-09-20")  %>%
  summarise(median = median(as.numeric(Receipt_Date - Ship_Date)))

#Site A / Ocean TLT = 38 days
SiteA_Ocean_TLTMedian  <- Raw_Data %>% 
  filter(Origin == "Site A" & Ship_Mode == "OCEAN" & as.numeric(Receipt_Date - Ship_Date) > 0 & Receipt_Date > "2019-11-01" & Receipt_Date < "2020-09-20" & Ship_Date > "2019-11-01" & Ship_Date < "2020-09-20") %>%
  summarise(median = median(as.numeric(Receipt_Date - Ship_Date)))

#SITE B / Air TLT = 33 days
SiteB_Air_TLTMedian  <- Raw_Data %>% 
  filter(Origin == "Site B" & Ship_Mode == "AIR" & as.numeric(Receipt_Date - Ship_Date) > 0 & Receipt_Date > "2019-11-01" & Receipt_Date < "2020-09-20" & Ship_Date > "2019-11-01" & Ship_Date < "2020-09-20") %>%
  summarise(median = median(as.numeric(Receipt_Date - Ship_Date)))

#Site B / Ocean TLT = 26 days
SiteB_Ocean_TLTMedian  <- Raw_Data %>% 
  filter(Origin == "Site B" & Ship_Mode == "OCEAN" & as.numeric(Receipt_Date - Ship_Date) > 0 & Receipt_Date > "2019-11-01" & Receipt_Date < "2020-09-20" & Ship_Date > "2019-11-01" & Ship_Date < "2020-09-20") %>%
  summarise(median = median(as.numeric(Receipt_Date - Ship_Date)))

#SITE C / Air TLT = 8 days
SiteC_Air_TLTMedian  <- Raw_Data %>% 
  filter(Origin == "Site C" & Ship_Mode == "AIR" & as.numeric(Receipt_Date - Ship_Date) > 0 & Receipt_Date > "2019-11-01" & Receipt_Date < "2020-09-20" & Ship_Date > "2019-11-01" & Ship_Date < "2020-09-20") %>%
  summarise(median = median(as.numeric(Receipt_Date - Ship_Date)))

#SITE D / Ground TLT = 3 days
SiteD_Ground_TLTMedian  <- Raw_Data %>% 
  filter(Origin == "Site D" & Ship_Mode == "GROUND" & as.numeric(Receipt_Date - Ship_Date) > 0 & Ship_Date > "2019-11-01" & Ship_Date < "2020-09-20") %>%
  summarise(median = median(as.numeric(Receipt_Date - Ship_Date)))

#Now that we have calculated the median MLT and TLT days, we will begin replacing the invalid rows of records for both Ship and Receipt Dates.
#Before we start cleaning, we will input the Raw_Data dataframe into Clean_Data for when all columns are replaced with valid information, the entire dataframe is clean. 
Clean_Data <- Raw_Data
Clean_Data <- as.data.frame(Clean_Data) #Making the Clean_Data dataset from a tibble to a dataframe

##Lets replace the Ship_dates using the median manufacturing lead time found previously.

#SITE A / Product A 
#In order to replace the NA values with each column, the structure has to be a a date. Therefor, we are inputting a random date to enable us to add days to the PO_Download_Date and inputting the result into Ship_Date
Clean_Data[Clean_Data$Origin == "Site A" & Clean_Data$LOB == "Product A" & is.na(Clean_Data$Ship_Date), "Ship_Date"] <- ymd("2018-01-01")
#Replacing the invalid Dates using median Manufacturing Lead Time
Clean_Data[Clean_Data$Origin == "Site A" & Clean_Data$LOB == "Product A" & (Clean_Data$Ship_Date <= Clean_Data$PO_Download_Date | 
                                                                              Clean_Data$Ship_Date >= Clean_Data$Receipt_Date | Clean_Data$Ship_Date < "2019-11-01" | Clean_Data$Ship_Date > "2020-09-20"), "Ship_Date"] <- 
  Clean_Data[Clean_Data$Origin == "Site A" & Clean_Data$LOB == "Product A" & (Clean_Data$Ship_Date <= Clean_Data$PO_Download_Date | 
                                                                                Clean_Data$Ship_Date >= Clean_Data$Receipt_Date | Clean_Data$Ship_Date < "2019-11-01" | Clean_Data$Ship_Date > "2020-09-20"), "PO_Download_Date"] + days(SA_PA_MLTMedian)
#Check if values have been successfully replaced. 
Clean_Data[Clean_Data$Origin == "Site A" & Clean_Data$LOB == "Product A" & is.na(Clean_Data$Ship_Date), ]  
Clean_Data[Clean_Data$Origin == "Site A" & Clean_Data$LOB == "Product A" &  (Clean_Data$Ship_Date <= Clean_Data$PO_Download_Date | 
                                                                               Clean_Data$Ship_Date < "2019-11-01" | Clean_Data$Ship_Date > "2020-09-20"), ] #No more invalid Ship_Date for this combination


#SITE A / Product B 
#Replace the NA values with random invalid date to enable addition of day range.
Clean_Data[Clean_Data$Origin == "Site A" & Clean_Data$LOB == "Product B" & is.na(Clean_Data$Ship_Date), "Ship_Date"] <- ymd("2018-01-01")
#Replace the Invalid Dates using median Manufacturing Lead Time
Clean_Data[Clean_Data$Origin == "Site A" & Clean_Data$LOB == "Product B" & (Clean_Data$Ship_Date <= Clean_Data$PO_Download_Date | 
                                                                              Clean_Data$Ship_Date >= Clean_Data$Receipt_Date | Clean_Data$Ship_Date < "2019-11-01" | Clean_Data$Ship_Date > "2020-09-20"), "Ship_Date"] <- 
  Clean_Data[Clean_Data$Origin == "Site A" & Clean_Data$LOB == "Product B" & (Clean_Data$Ship_Date <= Clean_Data$PO_Download_Date | 
                                                                                Clean_Data$Ship_Date >= Clean_Data$Receipt_Date | Clean_Data$Ship_Date < "2019-11-01" | Clean_Data$Ship_Date > "2020-09-20"), "PO_Download_Date"] + days(SA_PB_MLTMedian)
#Check if values have been successfully replaced. 
Clean_Data[Clean_Data$Origin == "Site A" & Clean_Data$LOB == "Product B" & is.na(Clean_Data$Ship_Date), ]  
Clean_Data[Clean_Data$Origin == "Site A" & Clean_Data$LOB == "Product B" &  (Clean_Data$Ship_Date < Clean_Data$PO_Download_Date | 
                                                                               Clean_Data$Ship_Date < "2019-11-01" | Clean_Data$Ship_Date > "2020-09-20"), ] #No more invalid Ship_Date for this combination


#SITE A / Product C
#Replace the NA values with random date to enable addition of day range.
Clean_Data[Clean_Data$Origin == "Site A" & Clean_Data$LOB == "Product C" & is.na(Clean_Data$Ship_Date), "Ship_Date"] <- ymd("2018-01-01")
#Replace the Invalid Dates using median Manufacturing Lead Time
Clean_Data[Clean_Data$Origin == "Site A" & Clean_Data$LOB == "Product C" & (Clean_Data$Ship_Date <= Clean_Data$PO_Download_Date | 
                                                                              Clean_Data$Ship_Date >= Clean_Data$Receipt_Date | Clean_Data$Ship_Date < "2019-11-01" | Clean_Data$Ship_Date > "2020-09-20"), "Ship_Date"] <- 
  Clean_Data[Clean_Data$Origin == "Site A" & Clean_Data$LOB == "Product C" & (Clean_Data$Ship_Date <= Clean_Data$PO_Download_Date | 
                                                                                Clean_Data$Ship_Date >= Clean_Data$Receipt_Date | Clean_Data$Ship_Date < "2019-11-01" | Clean_Data$Ship_Date > "2020-09-20"), "PO_Download_Date"] + days(SA_PC_MLTMedian)
#Check if values have been successfully replaced. 
Clean_Data[Clean_Data$Origin == "Site A" & Clean_Data$LOB == "Product C" & is.na(Clean_Data$Ship_Date), ]  
Clean_Data[Clean_Data$Origin == "Site A" & Clean_Data$LOB == "Product C" &  (Clean_Data$Ship_Date < Clean_Data$PO_Download_Date | 
                                                                               Clean_Data$Ship_Date < "2019-11-01" | Clean_Data$Ship_Date > "2020-09-20"), ] #No more invalid Ship_Date for this combination


#SITE B / Product B
#Replace the NA values with random date to enable addition of day range.
Clean_Data[Clean_Data$Origin == "Site B" & Clean_Data$LOB == "Product B" & is.na(Clean_Data$Ship_Date), "Ship_Date"] <- ymd("2018-01-01")
#Replace the Invalid Dates using median Manufacturing Lead Time
Clean_Data[Clean_Data$Origin == "Site B" & Clean_Data$LOB == "Product B" & (Clean_Data$Ship_Date <= Clean_Data$PO_Download_Date | 
                                                                              Clean_Data$Ship_Date >= Clean_Data$Receipt_Date | Clean_Data$Ship_Date < "2019-11-01" | Clean_Data$Ship_Date > "2020-09-20"), "Ship_Date"] <- 
  Clean_Data[Clean_Data$Origin == "Site B" & Clean_Data$LOB == "Product B" & (Clean_Data$Ship_Date <= Clean_Data$PO_Download_Date | 
                                                                                Clean_Data$Ship_Date >= Clean_Data$Receipt_Date | Clean_Data$Ship_Date < "2019-11-01" | Clean_Data$Ship_Date > "2020-09-20"), "PO_Download_Date"] + days(SB_PB_MLTMedian)
#Check if values have been successfully replaced. 
Clean_Data[Clean_Data$Origin == "Site B" & Clean_Data$LOB == "Product B" & is.na(Clean_Data$Ship_Date), ]  
Clean_Data[Clean_Data$Origin == "Site B" & Clean_Data$LOB == "Product B" &  (Clean_Data$Ship_Date < Clean_Data$PO_Download_Date | 
                                                                               Clean_Data$Ship_Date < "2019-11-01" | Clean_Data$Ship_Date > "2020-09-20"), ] #No more invalid Ship_Date for this combination


#SITE C / Product B
#Replace the NA values with random date to enable addition of day range.
Clean_Data[Clean_Data$Origin == "Site C" & Clean_Data$LOB == "Product B" & is.na(Clean_Data$Ship_Date), "Ship_Date"] <- ymd("2018-01-01")
#Replace the Invalid Dates using median Manufacturing Leadtime
Clean_Data[Clean_Data$Origin == "Site C" & Clean_Data$LOB == "Product B" & (Clean_Data$Ship_Date <= Clean_Data$PO_Download_Date | 
                                                                              Clean_Data$Ship_Date >= Clean_Data$Receipt_Date | Clean_Data$Ship_Date < "2019-11-01" | Clean_Data$Ship_Date > "2020-09-20"), "Ship_Date"] <- 
  Clean_Data[Clean_Data$Origin == "Site C" & Clean_Data$LOB == "Product B" & (Clean_Data$Ship_Date <= Clean_Data$PO_Download_Date | 
                                                                                Clean_Data$Ship_Date >= Clean_Data$Receipt_Date | Clean_Data$Ship_Date < "2019-11-01" | Clean_Data$Ship_Date > "2020-09-20"), "PO_Download_Date"] + days(SC_PB_MLTMedian)
#Check if values have been successfully replaced.
Clean_Data[Clean_Data$Origin == "Site C" & Clean_Data$LOB == "Product B" & is.na(Clean_Data$Ship_Date), ]  
Clean_Data[Clean_Data$Origin == "Site C" & Clean_Data$LOB == "Product B" &  (Clean_Data$Ship_Date < Clean_Data$PO_Download_Date | 
                                                                               Clean_Data$Ship_Date < "2019-11-01" | Clean_Data$Ship_Date > "2020-09-20"), ] #No more invalid Ship_Date for this combination




#SITE D / Product B
#Replace the NA values with random date to enable addition of day range.
Clean_Data[Clean_Data$Origin == "Site D" & Clean_Data$LOB == "Product B" & is.na(Clean_Data$Ship_Date), "Ship_Date"] <- ymd("2018-01-01")
#Replace the Invalid Dates using median Manufacturing Leadtime
Clean_Data[Clean_Data$Origin == "Site D" & Clean_Data$LOB == "Product B" & (Clean_Data$Ship_Date <= Clean_Data$PO_Download_Date | 
                                                                              Clean_Data$Ship_Date >= Clean_Data$Receipt_Date | Clean_Data$Ship_Date < "2019-11-01" | Clean_Data$Ship_Date > "2020-09-20"), "Ship_Date"] <- 
  Clean_Data[Clean_Data$Origin == "Site D" & Clean_Data$LOB == "Product B" & (Clean_Data$Ship_Date <= Clean_Data$PO_Download_Date | 
                                                                                Clean_Data$Ship_Date >= Clean_Data$Receipt_Date | Clean_Data$Ship_Date < "2019-11-01" | Clean_Data$Ship_Date > "2020-09-20"), "PO_Download_Date"] + days(SD_PB_MLTMedian)
#Check if values have been successfully replaced.
Clean_Data[Clean_Data$Origin == "Site D" & Clean_Data$LOB == "Product B" & is.na(Clean_Data$Ship_Date), ]  
Clean_Data[Clean_Data$Origin == "Site D" & Clean_Data$LOB == "Product B" &  (Clean_Data$Ship_Date < Clean_Data$PO_Download_Date | 
                                                                               Clean_Data$Ship_Date < "2019-11-01" | Clean_Data$Ship_Date > "2020-09-20"), ] #No more invalid Ship_Date for this combination

Clean_Data[Clean_Data$PO_Download_Date >= Clean_Data$Ship_Date, ] #Double check to see if any invalid dates existed. No invalid dates exists

#Now that the Ship_Date column variable is cleaned, we can move on to replacing the Receipt_Date column variable with the median TLT we previously calculated.

#SITE A: Air
#Through the following code, we will replace the Receipt_Date with a valid new date (Ship_Date + Median TLT of 8)
#First, we need to replace the NA variables with any dates so that following replacement is executed smoothly without error. 
Clean_Data[Clean_Data$Origin == "Site A" & Clean_Data$Ship_Mode == "AIR" & is.na(Clean_Data$Receipt_Date), "Receipt_Date"] <- ymd("2018-01-01")
#Now to replace the Receipt_Date with a new date by adding the median TLT  to the Ship_Date.  
Clean_Data[Clean_Data$Origin == "Site A" & Clean_Data$Ship_Mode == "AIR" &  Clean_Data$Ship_Date >= Clean_Data$Receipt_Date, "Receipt_Date"] <- 
  Clean_Data[Clean_Data$Origin == "Site A" & Clean_Data$Ship_Mode == "AIR" &  Clean_Data$Ship_Date >= Clean_Data$Receipt_Date, "Ship_Date"] + days(SiteA_Air_TLTMedian)
#Validate if the code above actually replaced the invalid dates.
Clean_Data[Clean_Data$Origin == "Site A" & Clean_Data$Ship_Mode == "AIR" & is.na(Clean_Data$Receipt_Date), ]
Clean_Data[Clean_Data$Origin == "Site A" & Clean_Data$Ship_Mode == "AIR" & Clean_Data$Ship_Date >= Clean_Data$Receipt_Date, "Receipt_Date"]  #Looks like all invalid values have been replaced. 


#SITE A: Fast Boat
#Through the following code, we will replace the Receipt_Date with a valid new date (Ship_Date + Median TLT of 25)
#First, we need to replace the NA variables with any dates so that following replacement is executed smoothly without error.  
Clean_Data[Clean_Data$Origin == "Site A" & Clean_Data$Ship_Mode == "FASTBOAT" & is.na(Clean_Data$Receipt_Date), "Receipt_Date"] <- ymd("2018-01-01")
#Now to replace the Receipt_Date with a new date by adding the median TLT  to the Ship_Date.  
Clean_Data[Clean_Data$Origin == "Site A" & Clean_Data$Ship_Mode == "FASTBOAT" &  Clean_Data$Ship_Date >= Clean_Data$Receipt_Date, "Receipt_Date"] <- 
  Clean_Data[Clean_Data$Origin == "Site A" & Clean_Data$Ship_Mode == "FASTBOAT" &  Clean_Data$Ship_Date >= Clean_Data$Receipt_Date, "Ship_Date"] + days(SiteA_Fastboat_TLTMedian)
#Validate if they actually replaced the invalid dates.
Clean_Data[Clean_Data$Origin == "Site A" & Clean_Data$Ship_Mode == "FASTBOAT" & is.na(Clean_Data$Receipt_Date), ]
Clean_Data[Clean_Data$Origin == "Site A" & Clean_Data$Ship_Mode == "FASTBOAT" &  Clean_Data$Ship_Date >= Clean_Data$Receipt_Date, "Receipt_Date"]  #Looks like all invalid values have been replaced. 


#SITE A: Ocean
#Through the following code, we will replace the Receipt_Date with a valid new date (Ship_Date + Median TLT of 38)
#First, we need to replace the NA variables with any dates so that following replacement is executed smoothly without error.
Clean_Data[Clean_Data$Origin == "Site A" & Clean_Data$Ship_Mode == "OCEAN" & is.na(Clean_Data$Receipt_Date), "Receipt_Date"] <- ymd("2018-01-01")
#Now to replace the Receipt_Date with a new date by adding the median TLT  to the Ship_Date. 
Clean_Data[Clean_Data$Origin == "Site A" & Clean_Data$Ship_Mode == "OCEAN" &  Clean_Data$Ship_Date >= Clean_Data$Receipt_Date, "Receipt_Date"] <- 
  Clean_Data[Clean_Data$Origin == "Site A" & Clean_Data$Ship_Mode == "OCEAN" &  Clean_Data$Ship_Date >= Clean_Data$Receipt_Date, "Ship_Date"] + days(SiteA_Ocean_TLTMedian)
#Validate if they actually replaced the invalid dates.
Clean_Data[Clean_Data$Origin == "Site A" & Clean_Data$Ship_Mode == "OCEAN" & is.na(Clean_Data$Receipt_Date), ]
Clean_Data[Clean_Data$Origin == "Site A" & Clean_Data$Ship_Mode == "OCEAN" &  Clean_Data$Ship_Date >= Clean_Data$Receipt_Date, "Receipt_Date"]  #Looks like all invalid values have been replaced. 


#SITE B: Air
#Through the following code, we will replace the Receipt_Date with a valid new date (Ship_Date + Median TLT of 33)
#First, we need to replace the NA variables with any dates so that following replacement is executed smoothly without error. 
Clean_Data[Clean_Data$Origin == "Site B" & Clean_Data$Ship_Mode == "AIR" & is.na(Clean_Data$Receipt_Date), "Receipt_Date"] <- ymd("2018-01-01")
#Now to replace the Receipt_Date with a new date by adding the median TLT  to the Ship_Date.  
Clean_Data[Clean_Data$Origin == "Site B" & Clean_Data$Ship_Mode == "AIR" &  Clean_Data$Ship_Date >= Clean_Data$Receipt_Date, "Receipt_Date"] <- 
  Clean_Data[Clean_Data$Origin == "Site B" & Clean_Data$Ship_Mode == "AIR" &  Clean_Data$Ship_Date >= Clean_Data$Receipt_Date, "Ship_Date"] + days(SiteB_Air_TLTMedian)
#Validate if they actually replaced the invalid dates.
Clean_Data[Clean_Data$Origin == "Site B" & Clean_Data$Ship_Mode == "AIR" & is.na(Clean_Data$Receipt_Date), ]
Clean_Data[Clean_Data$Origin == "Site B" & Clean_Data$Ship_Mode == "AIR" &  Clean_Data$Ship_Date >= Clean_Data$Receipt_Date, "Receipt_Date"]  #Looks like all invalid values have been replaced. 


#SITE B: Ocean
#Through the following code, we will replace the Receipt_Date with a valid new date (Ship_Date + Median TLT of 26)
#First, we need to replace the NA variables with any dates so that following replacement is executed smoothly without error. 
Clean_Data[Clean_Data$Origin == "Site B" & Clean_Data$Ship_Mode == "OCEAN" & is.na(Clean_Data$Receipt_Date), "Receipt_Date"] <- ymd("2018-01-01")
#Now to replace the Receipt_Date with a new date by adding the median TLT  to the Ship_Date.  
Clean_Data[Clean_Data$Origin == "Site B" & Clean_Data$Ship_Mode == "OCEAN" &  Clean_Data$Ship_Date >= Clean_Data$Receipt_Date, "Receipt_Date"] <- 
  Clean_Data[Clean_Data$Origin == "Site B" & Clean_Data$Ship_Mode == "OCEAN" &  Clean_Data$Ship_Date >= Clean_Data$Receipt_Date, "Ship_Date"] + days(SiteB_Ocean_TLTMedian)
#Validate if they actually replaced the invalid dates.
Clean_Data[Clean_Data$Origin == "Site B" & Clean_Data$Ship_Mode == "OCEAN" & is.na(Clean_Data$Receipt_Date), ]
Clean_Data[Clean_Data$Origin == "Site B" & Clean_Data$Ship_Mode == "OCEAN" &  Clean_Data$Ship_Date >= Clean_Data$Receipt_Date, "Receipt_Date"]  #Looks like all invalid values have been replaced. 


#SITE C: Air
#Through the following code, we will replace the Receipt_Date with a valid new date (Ship_Date + Median TLT of 8)
#First, we need to replace the NA variables with any dates so that following replacement is executed smoothly without error. 
Clean_Data[Clean_Data$Origin == "Site C" & Clean_Data$Ship_Mode == "AIR" & is.na(Clean_Data$Receipt_Date), "Receipt_Date"] <- ymd("2018-01-01")
#Now to replace the Receipt_Date with a new date by adding the median TLT  to the Ship_Date. 
Clean_Data[Clean_Data$Origin == "Site C" & Clean_Data$Ship_Mode == "AIR" &  Clean_Data$Ship_Date >= Clean_Data$Receipt_Date, "Receipt_Date"] <- 
  Clean_Data[Clean_Data$Origin == "Site C" & Clean_Data$Ship_Mode == "AIR" &  Clean_Data$Ship_Date >= Clean_Data$Receipt_Date, "Ship_Date"] + days(SiteC_Air_TLTMedian)
#Validate if they actually replaced the invalid dates.
Clean_Data[Clean_Data$Origin == "Site C" & Clean_Data$Ship_Mode == "AIR" & is.na(Clean_Data$Receipt_Date), ]
Clean_Data[Clean_Data$Origin == "Site C" & Clean_Data$Ship_Mode == "AIR" &  Clean_Data$Ship_Date >= Clean_Data$Receipt_Date, "Receipt_Date"]  #Looks like all invalid values have been replaced. 


#SITE D: Ground
#Through the following code, we will replace the Receipt_Date with a valid new date (Ship_Date + Median TLT of 3)
#First, we need to replace the NA variables with any dates so that following replacement is executed smoothly without error.
Clean_Data[Clean_Data$Origin == "Site D" & Clean_Data$Ship_Mode == "GROUND" & is.na(Clean_Data$Receipt_Date), "Receipt_Date"] <- ymd("2018-01-01")
#Now to replace the Receipt_Date with a new date by adding the median TLT  to the Ship_Date.  
Clean_Data[Clean_Data$Origin == "Site D" & Clean_Data$Ship_Mode == "GROUND" &  Clean_Data$Ship_Date >= Clean_Data$Receipt_Date, "Receipt_Date"] <- 
  Clean_Data[Clean_Data$Origin == "Site D" & Clean_Data$Ship_Mode == "GROUND" &  Clean_Data$Ship_Date >= Clean_Data$Receipt_Date, "Ship_Date"] + days(SiteD_Ground_TLTMedian)
#Validate if they actually replaced the invalid dates.
Clean_Data[Clean_Data$Origin == "Site D" & Clean_Data$Ship_Mode == "GROUND" & is.na(Clean_Data$Receipt_Date), ]
Clean_Data[Clean_Data$Origin == "Site D" & Clean_Data$Ship_Mode == "GROUND" &  Clean_Data$Ship_Date >= Clean_Data$Receipt_Date, "Receipt_Date"]  #Looks like all invalid values have been replaced. 

##Checking all stated invalid data conditions to see if any invalid data still exists within Clean_Data
Clean_Data[Clean_Data$Ship_Date >= Clean_Data$Receipt_Date,]
Clean_Data[Clean_Data$PO_Download_Date >= Clean_Data$Ship_Date,]
Clean_Data[is.na(Clean_Data$Ship_Date) | is.na(Clean_Data$Receipt_Date)]
view(Clean_Data)
#No more invalid data is filtered out, therefore, we have successfully replaced all invalid data
#We can now move all the clean data within the Clean_Data dataframe into the final itteration of data for further analysis
Final_Data <- Clean_Data #Putting all clean data into Final_Data

#Now to help with data analysis, we are adding the manufacturing Lead Time (MTL), Transit Lead Time (TLT), & Total Lead time (Total_LT) fas new columns for all rows. 
Final_Data <- Final_Data %>%
  mutate(MLT = as.numeric((Ship_Date - PO_Download_Date))) %>% #MLT is calculated through the difference of Ship_Date and PO_Download_Date
  mutate(TLT = as.numeric((Receipt_Date - Ship_Date))) %>% #TLT is calculated through the difference of Receipt_Date and Ship_Date
  mutate(Total_LT = MLT + TLT) #Total_LT is calculated from addinf both MLT and TLT together
str(Final_Data) #Looking at the structure of the dataframe to ensure the datatypes are listed as we want for future analysis. All datatypes look good for further exploration
count(Final_Data) 


# Exploratory Analysis

#'''Univariate Exploration'''#
#Visualize the amount of transactions for each Products
ggplot(Final_Data, aes(x = LOB)) + geom_bar() #Product A has ranking the top, followed by B and C
table(Final_Data$LOB)  #Product A = 1102, Product B = 997, Product C = 203

#Visualize the amount of transactions for each sites
ggplot(Final_Data, aes(x = Origin)) + geom_bar()  #Site A has the most transaction, followed by D, C, and B
table(Final_Data$Origin) #Site A = 1354, Site B = 177, Site C = 277, Site D = 494

#Visualize the amount of transactions for each Ship_Mode
ggplot(Final_Data, aes(x = Ship_Mode)) + geom_bar() # Air has the most transaction, followed by Ground, Ocean, and fastboat. 
table(Final_Data$Ship_Mode) #Air = 1075, FastBoat = 333, Ground = 494, Ocean = 400

#Visualize the amount of transactions for each Ship Quarter
ggplot(Final_Data, aes(x = Ship_Qtr)) + geom_bar() #Q2 has the most transactions, followed by Q1, and Q4
table(Final_Data$Ship_Qtr) #Q2 = 805, Q1 = 1182, Q3 = 315


#'''Bivariate Exploration'''#
#Visualize Manufacturing Lead Time with density plot, color by Products, facet by Origin. 
ggplot(Final_Data) + geom_density(aes(x = MLT, color = LOB)) + scale_x_continuous() +
  xlab("Manufacturing Lead Time (Days)") + facet_wrap(vars(Origin))
Final_Data[Final_Data$MLT > 100, ] #Filtering to find records of MLT over 100 days, one record found
Final_Data[Final_Data$MLT > 31, ] #There are 21 rows that have more than one month of manufacturing lead time. 
#That is reasonable if the timeline was after COVID-19 impact, but there is one row that has 146 days of 
#manufacturing lead time. It is just one row, so lets delete it. 
Final_Data <- Final_Data[Final_Data$MLT != 143, ]
# Run the ggplot again
ggplot(Final_Data) + geom_density(aes(x = MLT, color = LOB)) + scale_x_continuous() +
  xlab("Manufacturing Lead Time (Days)") + facet_wrap(vars(Origin))
#We don't have anymore extreme data in MLT


#Visualize Transit Lead Time with density plot, color by Ship_Mode, facet by Origin. 
ggplot(Final_Data) + geom_density(aes(x = TLT, color = Ship_Mode)) + scale_x_continuous() +
  xlab("Transit Lead Time (Days)") + facet_wrap(vars(Origin))
#Nothing major issues that stands out. 
#Although we have some high transit lead time, 
#it can be because it arrived at a certain destination, but was stored there for a while before being shipped to a final destination.


#Looking at the proportion of transactions for Origin and Product
data.table <- table(Final_Data$Origin, Final_Data$LOB)
prop.table(data.table, 2) #Product A and Product C is only manufactured in Site A and C.                                           
#4.91 % of Product B is manufactured in Site A                                       
#17.6 % in Site B                                             
#27.8 % in Site C                                            
#49.5 % in Site D
ggplot(Final_Data, aes(x = Origin, fill = LOB)) + geom_bar() #Visualization of proportion of products within each Origin 


#Look at the proportion of transactions for Origin and Ship_Mode
data.table <- table(Final_Data$Origin, Final_Data$Ship_Mode) 
prop.table(data.table, 2) #73.30% of products transported by Air are manufactured in Site A
#0.93% of products transported by Air are manufactured in Site B                    
#25.76% of products transported by Air are manufactured in Site C                      
#100 % of products transported by Fastboat are manufactured in Site                   
#100 % of products transported by Ground are manufactured in Site D                   
#58.25 % of products transported by Ocean are manufactured in Site A                     
#41.75 % of products transported by Ocean are manufactured in Site B
ggplot(Final_Data, aes(x = Ship_Mode, fill = Origin)) + geom_bar() #Visualization of proportion of origin within each ship mode


#Look at the proportion of transactions for Product and Ship_Mode
data.table <- table(Final_Data$LOB, Final_Data$Ship_Mode)
prop.table(data.table, 2)  #71.90 % of products transported by Air are Product A
#26.69 % of products transported by Air are Product B                         
#1.39 % of products transported by Air are Product C                        
#45.94 % of products transported by Fastboat are Product A                        
#54.05 % of products transported by Fastboat are Product C                          
#100 % of the products transported by ground are Product B                          
#44.0 % of products transported by Ocean are Product A                          
#54.0 % of products transported by Ocean are Product B                          
#2.0 % of the products transported by Ocean are Product C
ggplot(Final_Data, aes(x = Ship_Mode, fill = LOB)) + geom_bar() #Visualization of proportion of LOBs within each ship mode

#Look at the proportion of transactions for Ship Quarter and Product
data.table <- table(Final_Data$Ship_Qtr, Final_Data$LOB)
prop.table(data.table, 2) #38.29 % of Quarter 1 consists of Product A          
#33.63 % of Quarter 1 consists of Product B
#23.15 % of Quarter 1 consists of Product C                      
#45.37 % of Quarter 2 consists of Product A                       
#54.11 % of Quarter 2 consists of Product B                   
#70.44 % of Quarter 2 consists of Product C                      
#16.33 % of Quarter 4 consists of Product A                         
#12.24 % of Quarter 4 consists of Product B                      
#6.4 % of Quarter 4 consists of Product C
ggplot(Final_Data, aes(x = Ship_Qtr, fill = LOB)) + geom_bar() #Visualization of proportion of LOBs within each quarter

#We will now explore the distribution of the Manufacturing Lead Time and find various descriptive statistics, such as:
#minimum, max, mean, median, and standard deviation (sd)
ggplot(Final_Data, aes(x = MLT)) + geom_density() #It is right skewed
min(Final_Data$MLT) #min = 1 day
max(Final_Data$MLT) #max = 51 days
mean(Final_Data$MLT) #mean = 8.59 days
median(Final_Data$MLT) #median = 7 days
sd(Final_Data$MLT) #sd = 6.31 days


#We will now explore the distribution of the Transit Lead Time and find various descriptive statistics, such as:
#minimum, max, mean, median, and standard deviation (sd)
ggplot(Final_Data, aes(x = TLT)) + geom_density() #It is right skewed
min(Final_Data$TLT) #min = 1 day
max(Final_Data$TLT) #max = 86 days
mean(Final_Data$TLT) #mean = 13.83 days
median(Final_Data$TLT) #median = 9 days
sd(Final_Data$TLT) #sd = 11.99 days

#We will now explore the distribution of the total lead time and find various descriptive statistics, such as:
#minimum, max, mean, median, and standard deviation (sd) 
ggplot(Final_Data, aes(x = Total_LT)) + geom_density() #It is right skewed
min(Final_Data$Total_LT) #min = 3 days
max(Final_Data$Total_LT) #max = 89 days
mean(Final_Data$Total_LT) #mean = 22.43 days
median(Final_Data$Total_LT) #median = 16 days
sd(Final_Data$Total_LT) #sd = 14.67 days


#Looking at the correlation of numeric variables (Lead Times), which lead time (Manufacturing or Transit) has more affect in overall lead time?
cor.test(Final_Data$MLT, Final_Data$TLT) #20.97 % correlation
cor.test(Final_Data$MLT, Final_Data$Total_LT) # 60.14% correlation
cor.test(Final_Data$TLT, Final_Data$Total_LT) # 90.72 % correlation, TLT has the great affect in the overall  lead time

# DATA TRANSFORMING / SPLITTING / TRAINING / TESTING

##Now that we reached the data splitting stage, we will detail the steps in our method to split and validate our data. Since there are only 2301 individual records to work with, we want to give 75% of the record to training, and 25% into testing. 
##This is because we want to build a model with as much data as possible for better model. 

##Splitting Data Part 1##
set.seed(100) 

size = floor(0.75 * nrow(Final_Data)) 
rownums = seq(1:nrow(Final_Data))  
trainIndex = sample(rownums, size) 
Train_Data = Final_Data[trainIndex,]
Test_Data = Final_Data[-trainIndex,]
count(Train_Data) #Train has 75% split of data
count(Test_Data) #Test has 25% split of data

##Regression Model Trial #1 (fail)##
#For our first trial model, we will combine the main effect of Origin, Ship_mode, and Products 
#Without interactions to see how each explanatory variable independently affects the total lead time. 
Model <- lm(Total_LT ~ Origin + Ship_Mode + LOB, data = Train_Data)
summary(Model) # All variables are significant except for Ship_ModeGround where there is perfect collinearity  
# Rsquared: 67.72 %, Adj Rsquared: 67.07 %
# Overall p-value 2.2e-16. 
#Does each explanatory variables have linear relationship with total lead time?
ggplot(Train_Data, aes(x = Origin, y = Total_LT)) + geom_point() + geom_smooth(method = "lm")
#In above model, the residuals are not normally distributed around the mean of 0, there is perfect collinearity, and no linear relationship
#The first trial model fails.
#We are going to look at the overall distribution of total lead time. 
ggplot(Train_Data, aes(Total_LT)) + geom_density() #We can see that the total lead time is highly skewed to the right. 
plot(Model) # In fitted vs. residual plot, some residuals are not distributed equally around 0. 
# In Q-Q plot, we can see that residuals deviates from the normal distribution of residuals from 1 to 3.  
# In Scale Location plot, the red line is not approximately horizontal (have a good amount of slope), this means that average magnitude of standardized residuals are cahnging as a function of fitted values. 
# In the Leverage plot, there are no sign of extremely influential values. This is good. No need to pull out data from our current dataset. 
# As a conclusion, this plotting showed that the data has heteroscedasticity. Residuals are not normally distributed.  


#To fix the heteroscedasticity of the data and make residual more normally distributed, We are going to apply the log10 transformation. 
#First, apply the log10 and graph the total lead time to see if it will be normally distributed. 
ggplot(Train_Data, aes(Total_LT)) + geom_density() +
  scale_x_log10(breaks = c(0, 25, 50, 75)) + labs(title = "Total Lead Time Distribution") #Log10 works. This made the total lead time more normally distributed

##Regression Model Trial #2  (fail)##
#Now we are going to rebuild the model with main effect of all categorical variables, but with log 10 transformation. 
Model <- lm(log10(Total_LT) ~ Origin + Ship_Mode + LOB, data = Train_Data)
summary(Model)  #The residuals are normally distributed around the median of -0.00653. 
#Rsquared: 67.11%, Adj Rquared: 66.96%. Looks pretty good. 
#Once again, we see perfect collinearity for Ship_ModeGround
#Now plot the model again to see if the transformation affected the residual statistics. 
plot(Model) # Residual vs. Fitted plot shows that residuals are very normally distributed around the mean of 0, all throughout fitted values. 
# Q-Q plot looks better. The deviation from the normal distribution of residual is much less magnitude than before I applid Log10 transformation. 
# Scale Location plot shows that although we have slope for fitted values between 1.1 to 1.2, mostly the red line is horizontal. 
# Again in the leverage plot, there are no sign of extremely influential values. This is good. No need to pull out data from our current dataset. 
# As conclusion, the data has been transformed successfully to fit our model. 

##Regression Model Trial #3 (fail) ##
#Let's add interactions to this model. 
#In reality, Ship_Mode can differ as they come from different Origins. 
#LOB can also differ from different Origin. 
#We will add an interaction effect for both Ship_Mode*Origin and LOB*Origin. 
Model <- lm(log10(Total_LT) ~ Ship_Mode*Origin + LOB*Origin, data = Train_Data)
summary(Model) #Rsquared: 68.15%, Adj Rsquared: 68%
#All individual variables are statistically significant, however many variables and interactions have perfect collinearity. 
#The model is statistically significant with p-value: 2.2e-16
#Residual standard error 0.1423 with 1716 degree of freedom
plot(Model) 
#Model still looks good, however,in the summary output of the model it says "Coefficients: (15 not defined because of singularities)" 
#This indicates that it is most likely due to the high correlation between different independent variables. In other words, multicollinearity. 
#This is a case of perfect collinearity, which is caused by model generating interaction terms that does not exist in the data set. 

#To solve for this, we are going to concatenate the predictor variables and make additional variable that has the each unique combination of the categorical values. 
#This will eliminate the perfect collinearity because the combinations of categories are only what exist in the dataset. 

#Now, concatenating the variables Ship_Mode / Origin and LOB / Origin. 
Train_Data$Ship_ModeOrigin <- paste(Train_Data$Ship_Mode, Train_Data$Origin)
Train_Data$LOBOrigin <- paste(Train_Data$LOB, Train_Data$Origin)
#Make the new variables factors
Train_Data$Ship_ModeOrigin <- as.factor(Train_Data$Ship_ModeOrigin)
Train_Data$LOBOrigin <- as.factor(Train_Data$LOBOrigin)
#Now building a new model with the new explanatory variables that were created. 
Model <- lm(log10(Total_LT) ~ LOBOrigin + Ship_ModeOrigin, data = Train_Data)
summary(Model)

levels(Train_Data$Ship_ModeOrigin)
levels(Train_Data$LOBOrigin)
#The above model still created perfect collinearity, since origin is represented twice in the model, which is an issue in the model. 
#The possible solution is to build a model with concatenated variable of all Ship_Mode, Origin, and LOB. 

##Final Regression Model (correct)##
#Now we are concatenating the variables Ship_Mode, Origin, and LOB together.
Train_Data$ThreeComb <- paste(Train_Data$Ship_Mode, Train_Data$Origin, Train_Data$LOB)
Train_Data$ThreeComb <- as.factor(Train_Data$ThreeComb) #converting to factor
levels(Train_Data$ThreeComb) #all unique combinations/levels of new variable

#Now, build a model only using ThreeComb (shortened for Three Combination) as an explanatory variable. 
Model <- lm(log10(Total_LT) ~ ThreeComb + Ship_Qtr, data = Train_Data)
summary(Model)
#First note, the perfect collinearity is gone.
#In the model, we cannot include the main effects of original categorical variables since that will result in perfect collinearity again. 
#All coefficients are significant
#Rsquared: 68.78 %, Adj Rsquared: 68.57 %
#p-value: <2.2e-16 
#Residual are normally distributed around the mean of 0 
#Residuals:
#Min      1Q      Median      3Q       Max 
#-0.47318 -0.08345 -0.00897  0.07997  0.63482 
#We will plot the residual for a final check. 
plot(Model)   #The residual plot looks good too. 
# Residual vs. Fitted plot shows that residuals are very normally distributed around the mean of 0, all throughout fitted values. 
# Q-Q plot looks better. The deviation from the normal distribution of residual is much less magnitude than before  applying the Log10 transformation. 
# Scale Location plot shows that although we have slope for fitted values between 1.1 to 1.2, mostly the red line is horizontal. 
# Again in the leverage plot, there are no sign of extremely influential values. This is good. No need to pull out data from our current dataset. 

#We will see now if the model generated will violate the assumptions of linear regression by these four criteria. 
#Assumption 1: Linear relationship between dependent variable and independent variables
#Since all independent variables are categorical, we cannot plot a linear graph. Therefore this assumption does not apply. 

#Assumption 2: Residuals are normally distributed around zero. 
#From our output of the residual above, it is clear that residuals are roughly normally distributed around zero.
#Q-Q plot also shows that there are no significant deviation of residuals from the center line. 
#Scale-Location plot shows that red line is very horizontal throughout the graph. 
#All of the above is a good indication that the log 10 transformation has successfully made the residual normally distributed around the mean of 0.

#Assumption 3: No relationship between dependent variables and residuals (homoscedasticity.)
#Residual vs. fitted plot shows that residuals are distributed equally around the red line throughout the graph.
#Also in residual vs. fitted plot, the red line is almost perfectly on the 0.0 line throughout the graph. 
#This is a good indication of homoscedasticity. 


#Assumption 4: No or little multicollinearity
library(car)
vif(Model)  #Looks like we do not have any multicollinearity since all GVIF values are under 10
#Ship_Qtr and ThreeComb is not correlated with each other. 


##Splitting Data Part 2##

#Transforming the prediction to the original outcome space. By applying 10^predicted(Model)
#Without doing so, predicted value will still be in its log10 transformed outcome space. 
#This step is critical in order to get the correct residual. 
#Otherwise, R-squared for training dataset will be 1) extremely small value = extremely high percentage.
Train_Data$Predicted <- 10^(predict(Model))  

#Using the Predicted value that is already in the original outcome space, calculate the residuals. 
Train_Data$Residual <- Train_Data$Predicted - Train_Data$Total_LT 
View(Train_Data)

#Calculate the Rsquared and RMSE using the residual calculated above. 
Train_R2 <- 1 - (sum(((Train_Data$Residual))^2) / sum((Train_Data$Total_LT - mean(Train_Data$Total_LT))^2))
Train_RMSE <- sqrt(mean((Train_Data$Residual)^2))
Train_R2   
Train_RMSE      

#Now, Test the model. 
#Before we generate the R2 and RMSE for the test dataset, we will create a concatenated column in the same way that was done for Train_Data dataset. 
Test_Data$ThreeComb <- paste(Test_Data$Ship_Mode, Test_Data$Origin, Test_Data$LOB)
Test_Data$ThreeComb <- as.factor(Test_Data$ThreeComb)

#To calculate the predicted value with the test dataset using our model. 
#Again, predicted value has to be transformed back into its original outcome space. 
#Add Predicted data and Residual data in the Test_Data dataset
Test_Data$Predicted <- 10^(predict(Model, newdata = Test_Data))
Test_Data$Residual <- Test_Data$Total_LT - Test_Data$Predicted

#Calculate R2 and RMSE
Test_R2 <- 1 - (sum((Test_Data$Residual)^2)/ sum((Test_Data$Total_LT - mean(Test_Data$Total_LT))^2))
Test_RMSE<- sqrt(mean((Test_Data$Residual)^2))
Test_R2
Test_RMSE

#Create a dataframe that have both R2 and RMSE for Training and R2 and RMSE for Test
Comparison <- data.frame(Train_R2, Train_RMSE, Test_R2, Test_RMSE)
Comparison

#   Train_R2   Train_RMSE   Test_R2     Test_RMSE
#  0.6787921    8.031126   0.6752069    9.133107

#This looks very good.



# Analytical Questions

#In the analytical questions to answer, logistics lead time is commonly used, in our code we distinguished it as transit lead time (TLT). Both names are interchangable.

#'''Question 1'''#
#Use the dataset to come up with a multi-variable linear regression equation for lead
#time. What is the adjusted R-square for the model? Are all variables equally important?
#Using the final model created in the previous section to answer this question, we were able to create a multi-variable linear regression.
Model <- lm(log10(Total_LT) ~ ThreeComb + Ship_Qtr, data = Train_Data)
summary(Model)


#'''Question 2'''#
#Check for multicollinearity in the independent variables of the model and list down the
#degree of correlation between these predictor variables. Create regression models with
#Multicollinearity and by eliminating the multicollinearity to determine and present the
#level of impact it has on the coefficients, p-values, predictions and accuracy of
#predictions?


#'''Question 3'''#
#Has the logistics lead time reduced since Q2, when compared with Q4 and Q1 for AIR and GROUND ship mode?

#Select only the variables that we are interested in. In this case, Ship_Qtr, Ship_Mode, and TLT. 
Data_Logistics <- Final_Data %>%
  select(Ship_Qtr, Ship_Mode, TLT)
#Now filter the rows that only have the AIR and GROUND Ship_Mode.
Data_Logistics <- Data_Logistics[Data_Logistics$Ship_Mode == "AIR" | Data_Logistics$Ship_Mode == "GROUND", ]
Data_Logistics
Data_Logistics[Data_Logistics$Ship_Mode != "AIR" & Data_Logistics$Ship_Mode != "GROUND", ] #Successfully filtered values to be Air and Ground only. 

#To find the insights for the question, we will use dplyr package to manipulate the data
library(dplyr)
#We will group the dataset by Ship_Mode and Ship_Qtr to find mean, standard deviation, and max of the transit lead time.  
Logistics_Change <- Data_Logistics %>%
  group_by(Ship_Qtr, Ship_Mode) %>%
  summarise(Mean = mean(TLT), Median = median(TLT), SD = sd(TLT), Max = max(TLT))
Logistics_Change #    Ship_Qrt   Ship_Mode    Mean    Median    SD    Max
#        Q1        Air       8.53      8      3.33   28
#        Q1     Ground       3.79      4      1.97   11     
#        Q2        Air       8.62      8      4.39   39
#        Q2     Ground       3.75      4      2.04   15
#        Q4        Air       8.23      8      3.25   21
#        Q4     Ground       5.01      4      2.07   11
ggplot(Data_Logistics, aes(x = Ship_Qtr, y = TLT)) + geom_boxplot() + facet_wrap(vars(Ship_Mode)) #visualizing the distribution of Air and Ground over the quarter to view and changes
#Looking at the descriptive statistics calculated and the scatter plot of distribution, there are some interesting finds. In Ground Ship_Mode, there does seems to be a reduced transit (logistics) lead time in Q2 in comparing it to Q1 and Q4. 
#In Ground Ship_Mode, we see that Q4 had a average TLT of 5.01 days, Q1 has a average TLT of 3.79 ~ 4 days, and Q2 had a average TLT of 3.75 ~ 4 days. Overall, the TLT has reduced since Q$ of FY2019, and has stayed constant through Q1 and Q2 of FY2020.
#Looking at TLT for Air, there are large visual and numeric differences. Looking at the scatter plot, we can interpret that all three quarters has practically the same TLT average, and this is true as the descriptive statistics show they all means are between 8-9 days. 
#However given COVID-19, the large amount of data outliers in Q1 and Q2 for Air Ship_Mode is a clear indication of supply chain disruption since there has been a more than abnormal about of outliers, the highest for Q! being 28 days, and Q2 being 38 days.
#The difference betwwen outliers between Ground and Air mean that there is a greater dependency of resources being provided by manufactuers in overseas locations.


#'''Question #4'''#
#'How many in-transit days can be saved if we bring inventory by AIR from Site A compared to Site C?

#Select only the variables that we are interested in. In this case Ship_Mode, Origin, and TLT. 
Data_SA_SC <- Final_Data %>%
  select(Ship_Mode, Origin, TLT)
Data_SA_SC
Data_SA_SC <- Data_SA_SC[Data_SA_SC$Origin == "Site A" | Data_SA_SC$Origin == "Site C", ]#Successfully filtered values to be Site A and Site C only. 

#Visualized which Ship_Mode Site A and Site C uses with ggplot. 
ggplot(Data_SA_SC, aes(x = Origin, fill = Ship_Mode)) + geom_bar() #We see that a majority of Site A is transported by Air while all of Site C is transported by Air

#We need to find the mean and median of Transit Lead Time by Ship_Mode from their origins to see if there is a difference in TLT:
Median_TLT_Air <- Data_SA_SC %>%
  filter(Ship_Mode == "AIR") %>%
  group_by(Origin) %>%
  summarise(Median = median(TLT), Mean = round(mean(TLT)))
Median_TLT_Air
##Looking at the descriptive statistics, there is a difference in TLT is inventory was being brough by Air from Site A compared to Site C.
##Since the mean of TLT of Site A is 8 days, and the mean of TLT for Site C is 9 days, one day could be saves if inventory was brough by Air from Site A. 


# Optimization

#'''Question #1'''#
#Assuming we have a customer deal 100K units of product A, for which Purchase
#Orders(PO) are set to be provided to site A between 21st Sept and 25th Sept at a daily
#rate of 20K units (20K*5days = 100K) and we must the meet deadline of 27th Oct to
#receive all units at the destination facility/warehouse in US in order to meet customer
#due date. We have only $1.5M allocated on freight budget to realize positive margin
#on customer sales (The higher the logistics cost, it lowers the profit margin). The
#product is sold at $333 to the customer with 7% margin. Considering all the above
#factors, the supply chain analytics team needs to optimize and evaluate the following:

#Join Cost_Details table and Final_Data. Clean up the columns. 
Final_Data$Lookup <- paste(Final_Data$LOB, Final_Data$Ship_Mode)
Cost_Details <- Cost_Details %>%
  select(-c(Lookup))
Cost_Details <- rename(Cost_Details, c(Ship_Mode = `Ship Mode`))
Cost_Details$Lookup <- paste(Cost_Details$LOB, Cost_Details$Ship_Mode)
Combined <- Final_Data %>%
  inner_join(Cost_Details, by = "Lookup")
Combined <- Combined %>% 
  select(-c(LOB.y, Ship_Mode.y))
Combined <- rename(Combined, c(LOB = LOB.x, Ship_Mode = Ship_Mode.x))

#Can we fulfill all 100K units before the deadline with the available budget based
#on the current manufacturing and logistics lead times?

#Filter the data to only include record from Site A and Product A.
Combined <- Combined %>%
  filter(Origin == "Site A", LOB == "Product A")
#Find find the mean of Total_LT for each shipmode
Data_Total_LT <- Combined %>%
  group_by(Ship_Mode) %>%
  summarise(Mean_LT = ceiling(mean(Total_LT)))
Data_Total_LT #Mean Total_LT for AIR is 16 days, for FASTBOAT is 35 days, and for OCEAN is 44 days

#Calculate the range of days between the first purchase order date and the deadline date. 
date("2020-10-27") - date("2020-09-21") #There are 36 days difference
#From the output of Data_Total_LT, the mean total lead time for Air is 16 days, mean total lead time for Fastboat is 35 days, mean total lead time for Ocean is 44 days. 
#From this insight, we can say that the ocean will not be a optimal ship mode to choose since it will not arrive within 36 days.
#Air has a mean total lead time of 16 days, and fastboat has mean total lead tome of 35 days. 
#It is possible to use Fastboat for the first 2 days (since fastboat is cheaper) but because 35days is only a mean, is is super close to the 36 day range
#one approach to determine if we should use Fastboat is to assess the percentage of records that have total lead time of under 35days for FastBoat from the whole dataset.
#If 70% had total lead tiome of smaller than 35days, then we will include in first 2 days. 

Data_Fastboat <- Combined[Combined$Ship_Mode == "FASTBOAT", ]

Under <- count(Data_Fastboat[Data_Fastboat$Total_LT <= 35, ])
Prob_Under <- Under/nrow(Data_Fastboat) #72.52% of the Fastboat records have total lead time of under 35 days. 
Prob_Under                              #We will use Fastboat for the first 2 days of shipment. 
#Next 3 days of shipment will be done by Air. 

Budget <- 1500000 #Budget of $1,500,00
Cost_Details #Going back to the Cost_detail table, we can see that for Product A, Air cost $22 per box, and Fastboat cost $13 per box. 
#We can calculate how much logistic cost is needed to fulfill the order from this information. 
#The first 2days by fastboat. Fastboat logistic cost = $13*20000units*2days
Fastboat_LC <- 13*20000*2 #$520,000
Fastboat_LC
#Next 3days by Air. Air logistic cost = $22*20000units*3days
Air_LC <- 22*20000*3 #$1,320,000
Air_LC
Total_LC <- Fastboat_LC + Air_LC
Total_LC #Total Logistic Cost = $1.84M 
Dif_Budget <- Budget - Total_LC
Dif_Budget
#Answer: We are able to fulfill the order prior to the deadline, however we will go over budget by $340000. 

#'''Question #2'''
#If not, please come up with an optimized volume of product A needs to be lifted
#by air/Ocean/Fast Boat to meet maximum customer demand depending on the
#ship mode lead times and budget available?
#Our goal here is identify volume allocation for minimizing the logistic cost and without exceeding budget of $1.5M and still meet the deadline. 
#From the mean of total lead time for each ship modes, we know that ocean is not an available transportation. 
#Also, it is favorable to ship as much products by fastboat as long as it meets the deadline date. 
#In our last calculation, we found that allocation of fastboat shipment of 2days and air shipment of 3days will meet the deadline, but 
#exceeds the allocated budget for $340000. 

#Adjusted volume: 50Kunits/day, in a span of 2 days with Fastboat, totaling 100K units. 
Fastboat_LC <- 13*50000*2
Total_LC <- Fastboat_LC 
Dif_Budget <- Budget - Total_LC  #With this scenario, we have about $200000 under budget expense. 


#3 Create a proposal to request for additional budget to fullfil all 100K units by the
#deadline?
#Based on previous analysis, the minimum cost to fulfill the order by the deadline of oct 27th is $1.84M (initial proposed budget) which is over $340000 the budget. 
#Since this customer can create large amount of revenue, being able to meet the deadline for this customer is important for our profit margins.
#We would like to request additional $340000 to our budget to fulfill the order by Oct 27th. Being able to invest $340,000 in the short run will result in $33,300,000 in long term revenue,
#and by meeting the customer's designated deadline results in increased customer loyalty.


#4 Provide an analysis to leadership team on total logistics cost vs total revenue
#vs profit margin with the initial budget provided and proposed budget for them
#to decide what trade-offs to make?

#Total logistic costs
#For the initial budget, total logistics costs total $1,840,000 to transport the material over five days with a volume of 20K units per day using FASTBOAT for 2/5 days and AIR for 3/5 days.
#For the newly proposed budget with optimized volume amounts, the total logistics costs total $1,300,000 to transport the materials over two days with a volume of 50K units per day using FASTBOAT for each day.
#Comparing the two, in the initial budget, it goes over by $340,000, and in the new budget, it would be under budget by $200,000 ***$200,000 SAVED***

#Total Revenue
#Considering that Tech Company is selling each unit for $333, and there are 100K units being sold, the revenue acquired would be $33,300,000 
#Under both budget, since revenue only takes into account the total money made off of selling goods before subtracting associated costs, both budgets would have the same revenue ($33,300,000)

#Profit Margin
#For the first budget, with being $340,000 over the designated budget ($1.5 MIL), the profit margin is 7%.
#Knowing that higher logistics costs result in lower profit margins, this would mean that with using the new budget of $1,300,000
#and sending the 100K units over two days by FASTBOAT while being under budget by $200,000, the profit margin from the proposed budget is expected to be bigger than the initial budget.

#Trade offs
#Understanding the implications of both budget, there is no clear trade off for choosing the initial budget. With the initial budget, all units will be received within five days, however over the budget by $340,000.
#In the newly proposed budget, in choosing this, all units will be received in 2 days times (less time) with $200,000 saved from the initial budget.














