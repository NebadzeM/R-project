### Programming Project for IPD, Mate Nebadze

### Topic - Financial Sector Strength and Stability and its Influence on Social Well-Being

##Primariliy, Run All the Necessary Libraries
library (tidyverse)
library (mosaic)
library (readr)
library (dplyr)
library(ggplot2)
library(httr2)
library(plotly)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)

##Importing the Data - All the Data is imported From World Bank, for the last decade (2010-2019) and for the countries of Emerging Markets: Asia(Japan, Turkiye, Uzkebistan, Vietnam, South Korea), Europe(Poland, Hungary, Czech Republic, Croatia, Slovakia), Meadle East & Africa (Egypt, Saudi Arabia, United Arab Emirates, South Africa, Nigeria), Latin America(Brazil, Argentina, Mexico, Colombia, Chile) and Caucasus (Georgia, Armenia, Azerbaijan)
##Datatypes of Variables - Character(Country, ISO Code of Country), Numeric(Year, Life Expectancy, Capital to Asset Ratio, Loans Lent to Businesses, Deposit Interest Rate, Depositors Per 1000 Citizen, Foreign Direct Investment, GDP (USD), Gross National Income (GNI USD), Interest Rate Spread, Non-Performing Loans (NPL) to Total Loans Ratio, Unemployment Rate) 

#Importing Life expectancy at birth - Measurement for Social Well-being 
req <- request("https://api.worldbank.org/v2/country/GEO;JPN;TUR;UZB;VNM;KOR;POL;HUN;CZE;HRV;SVK;EGY;SAU;ARE;ZAF;NGA;BRA;ARG;MEX;COL;CHL;ARM;AZE/indicator/SP.DYN.LE00.IN?per_page=230&format=json&date=2010:2019")
req_dry_run(req)
response <-  req_perform(req)
resp_status(response)
resp_status_desc(response)
result <- resp_body_json(response)
result <- result [[2]]
Country <- lapply(seq(1,length(result)), FUN = function(i)
{  extractedData <- result[[i]][[2]][[2]]
return(extractedData)
})
ISO_Code <- lapply(seq(1,length(result)), FUN = function(i)
{  extractedData <- result[[i]][3]
return(extractedData)
})
Year <- lapply(seq(1,length(result)), FUN = function(i)
{  extractedData <- result[[i]][4]
return(extractedData)
})
LIfe_Expectancy <- lapply(seq(1,length(result)), FUN = function(i)
{  extractedData <- result[[i]][5]
return(extractedData)
})
Country <- unlist(Country)
ISO_Code <- unlist(ISO_Code)
Year <- unlist(Year)
LIfe_Expectancy <- unlist(LIfe_Expectancy)
LIfe_Expectancy <- tibble(Country, ISO_Code, Year, LIfe_Expectancy)
#check the structure and types of vectors 
str(LIfe_Expectancy)
#Years and Life expectancy are not numeric, so I turn them into numeric
LIfe_Expectancy$LIfe_Expectancy <- as.numeric(LIfe_Expectancy$LIfe_Expectancy)
LIfe_Expectancy$Year<- as.numeric(LIfe_Expectancy$Year)


#Importing Unemployment rate - % of total labor force - Measurement of well-being (economical view) 
req <- request("https://api.worldbank.org/v2/country/GEO;JPN;TUR;UZB;VNM;KOR;POL;HUN;CZE;HRV;SVK;EGY;SAU;ARE;ZAF;NGA;BRA;ARG;MEX;COL;CHL;ARM;AZE/indicator/SL.UEM.TOTL.ZS?per_page=230&format=json&date=2010:2019")
req_dry_run(req)
response <-  req_perform(req)
resp_status(response)
resp_status_desc(response)
result <- resp_body_json(response)
result <- result [[2]]
Country <- lapply(seq(1,length(result)), FUN = function(i)
{  extractedData <- result[[i]][[2]][[2]]
return(extractedData)
})
ISO_Code <- lapply(seq(1,length(result)), FUN = function(i)
{  extractedData <- result[[i]][3]
return(extractedData)
})
Year <- lapply(seq(1,length(result)), FUN = function(i)
{  extractedData <- result[[i]][4]
return(extractedData)
})
Unemployment_Rate <- lapply(seq_along(result), function(i) {
  Value <- result[[i]][[5]]
  if (is.null(Value)) Value <- NA
  return(Value)
})
Country <- unlist(Country)
ISO_Code <- unlist(ISO_Code)
Year <- unlist(Year)
Unemployment_Rate <- unlist(Unemployment_Rate)
Unemployment_Rate <- tibble(Country, ISO_Code, Year, Unemployment_Rate)
str(Unemployment_Rate)
#Year is saved as charachter so I will make it numeric
Unemployment_Rate$Year <-  as.numeric(Unemployment_Rate$Year)

#Importing Domestic Credit Provided By Financial Sector to Business Sector(% of GDP)
req <- request("https://api.worldbank.org/v2/country/GEO;JPN;TUR;UZB;VNM;KOR;POL;HUN;CZE;HRV;SVK;EGY;SAU;ARE;ZAF;NGA;BRA;ARG;MEX;COL;CHL;ARM;AZE/indicator/FS.AST.PRVT.GD.ZS?per_page=230&format=json&date=2010:2019")
req_dry_run(req)
response <-  req_perform(req)
resp_status(response)
resp_status_desc(response)
result <- resp_body_json(response)
result <- result [[2]]
Country <- lapply(seq(1,length(result)), FUN = function(i)
{  extractedData <- result[[i]][[2]][[2]]
return(extractedData)
})
ISO_Code <- lapply(seq(1,length(result)), FUN = function(i)
{  extractedData <- result[[i]][3]
return(extractedData)
})
Year <- lapply(seq(1,length(result)), FUN = function(i)
{  extractedData <- result[[i]][4]
return(extractedData)
})
Business_Loans_prcntOfGDP <- lapply(seq_along(result), function(i) {
  Value <- result[[i]][[5]]
  if (is.null(Value)) Value <- NA
  return(Value)
})

Country <- unlist(Country)
ISO_Code <- unlist(ISO_Code)
Year <- unlist(Year)
Business_Loans_prcntOfGDP <- unlist(Business_Loans_prcntOfGDP)
#express as percentage for further actions
as.numeric(Business_Loans_prcntOfGDP)
Business_Loans_prcntOfGDP <- Business_Loans_prcntOfGDP/100
Business_Loans_prcntOfGDP <- tibble(Country, ISO_Code, Year, Business_Loans_prcntOfGDP)
anyNA(Business_Loans_prcntOfGDP)
NAs <- Business_Loans_prcntOfGDP %>% 
  filter(is.na(Business_Loans_prcntOfGDP))
NA_countries <- distinct(NAs, Country)
NA_countries <- NA_countries[[1]]
Business_Loans_prcntOfGDP <- Business_Loans_prcntOfGDP%>% 
  filter(!Country %in% NA_countries)


#Importing GDP ( measured in current US$) to multiply by the previous chart, to turn into the percentage of GDP into the absolute frequencies.
req <- request("https://api.worldbank.org/v2/country/GEO;JPN;TUR;UZB;VNM;KOR;POL;HUN;CZE;HRV;SVK;EGY;SAU;ARE;ZAF;NGA;BRA;ARG;MEX;COL;CHL;ARM;AZE/indicator/NY.GDP.MKTP.CD?per_page=230&format=json&date=2010:2019")
req_dry_run(req)
response <-  req_perform(req)
resp_status(response)
resp_status_desc(response)
result <- resp_body_json(response)
#I need only the second component of the List, which includes Country name, ISO code, Year and value of GDP
result <- result [[2]]
#Using Loops for extracting each variable from the list
Country <- lapply(seq(1,length(result)), FUN = function(i)
{  extractedData <- result[[i]][[2]][[2]]
return(extractedData)
})

ISO_Code <- lapply(seq(1,length(result)), FUN = function(i)
{  extractedData <- result[[i]][3]
return(extractedData)
})

Year <- lapply(seq(1,length(result)), FUN = function(i)
{  extractedData <- result[[i]][4]
return(extractedData)
})

GDP_USD <- lapply(seq(1,length(result)), FUN = function(i)
{  extractedData <- result[[i]][5]
return(extractedData)
})
#unlisting extracted lists
Country <- unlist(Country)
ISO_Code <- unlist(ISO_Code)
Year <- unlist(Year)
GDP_MLN_USD <- unlist(GDP_USD)
as.numeric(GDP_MLN_USD)
GDP_MLN_USD <- GDP_MLN_USD/1000000
#creating a tibble out of variables
GDP_USD <- tibble(Country, ISO_Code, Year, GDP_MLN_USD)


#Importing Gross National Income (GNI) per capita - Income earned by resident inside (DGP) and Outside of the countri
#GNI is better measurement in todays world, as globalization gets more common, and we should take into account income earned outside of the borders by the citizens of the country
#per capita measurement reflects better how an individual lives in different area of the world, instead of comparing gross measurements. 
req <- request("https://api.worldbank.org/v2/country/GEO;JPN;TUR;UZB;VNM;KOR;POL;HUN;CZE;HRV;SVK;EGY;SAU;ARE;ZAF;NGA;BRA;ARG;MEX;COL;CHL;ARM;AZE/indicator/NY.GNP.PCAP.PP.CD?per_page=230&format=json&date=2010:2019")
req_dry_run(req)
response <-  req_perform(req)
resp_status(response)
resp_status_desc(response)
result <- resp_body_json(response)
result <- result [[2]]
Country <- lapply(seq(1,length(result)), FUN = function(i)
{  extractedData <- result[[i]][[2]][[2]]
return(extractedData)
})
ISO_Code <- lapply(seq(1,length(result)), FUN = function(i)
{  extractedData <- result[[i]][3]
return(extractedData)
})
Year <- lapply(seq(1,length(result)), FUN = function(i)
{  extractedData <- result[[i]][4]
return(extractedData)
})
GNI_Per_Capita <- lapply(seq(1,length(result)), FUN = function(i)
{  extractedData <- result[[i]][5]
return(extractedData)
})
#unlisting extracted lists
Country <- unlist(Country)
ISO_Code <- unlist(ISO_Code)
Year <- unlist(Year)
GNI_Per_Capita_USD <- unlist(GNI_Per_Capita)
#creating a tibble out of variables
GNI_Per_Capita_USD <- tibble(Country, ISO_Code, Year, GNI_Per_Capita_USD)


#Importing Bank capital to assets ratio (in percentages) - this ratio is for banks stability measurement
#Bank's Capital is Equity funds, retained earning - Tier 1 and Subordinated loans and Bonds - Tier 2
#Bank's assets are everything that the bank owns (physical assets, loans, investments, etc.)
#This ratio Shows if a bank can deal with potencial losses.

req <- request("https://api.worldbank.org/v2/country/GEO;JPN;TUR;UZB;VNM;KOR;POL;HUN;CZE;HRV;SVK;EGY;SAU;ARE;ZAF;NGA;BRA;ARG;MEX;COL;CHL;ARM;AZE/indicator/FB.BNK.CAPA.ZS?per_page=230&format=json&date=2010:2019")
req_dry_run(req)
response <-  req_perform(req)
resp_status(response)
resp_status_desc(response)
result <- resp_body_json(response)
result <- result [[2]]
Country <- lapply(seq(1,length(result)), FUN = function(i)
{  extractedData <- result[[i]][[2]][[2]]
return(extractedData)
})
ISO_Code <- lapply(seq(1,length(result)), FUN = function(i)
{  extractedData <- result[[i]][3]
return(extractedData)
})
Year <- lapply(seq(1,length(result)), FUN = function(i)
{  extractedData <- result[[i]][4]
return(extractedData)
})
#as there are NAs in the imported data, I need different approach
Bank_CtoA_Ratio <- lapply(seq_along(result), function(i) {
  Value <- result[[i]][[5]]
  if (is.null(Value)) Value <- NA
  return(Value)
})
Country <- unlist(Country)
ISO_Code <- unlist(ISO_Code)
Year <- unlist(Year)
Bank_CtoA_Ratio <- unlist(Bank_CtoA_Ratio)
Bank_CtoA_Ratio <- tibble(Country, ISO_Code, Year, Bank_CtoA_Ratio)
#check if there are NAs
anyNA(Bank_CtoA_Ratio)
#as there were NAs, I filtered the tibble to understand which country lacks the data, and then use only countries that has data
NAs <- Bank_CtoA_Ratio %>% 
  filter(is.na(Bank_CtoA_Ratio))
#understand which countries do not have data
NA_countries <- distinct(NAs, Country)
#it saves as a tibble, so I need to turn this tibble into a vector
NA_countries <- NA_countries[[1]]
Bank_CtoA_Ratio <- Bank_CtoA_Ratio%>% 
  filter(!Country %in% NA_countries)

#Importing Bank nonperforming loans (NPL) to total gross loans (in percentage) 
#NPL are the loans that is concidered problematic and potentially not being paid back
#this ratio shows how safe and good is a porfel of the bank, which is directly connected to the Stability and strenfth of the Financial Institurtion
req <- request("https://api.worldbank.org/v2/country/GEO;JPN;TUR;UZB;VNM;KOR;POL;HUN;CZE;HRV;SVK;EGY;SAU;ARE;ZAF;NGA;BRA;ARG;MEX;COL;CHL;ARM;AZE/indicator/FB.AST.NPER.ZS?per_page=230&format=json&date=2010:2019")
req_dry_run(req)
response <-  req_perform(req)
resp_status(response)
resp_status_desc(response)
result <- resp_body_json(response)
result <- result [[2]]
Country <- lapply(seq(1,length(result)), FUN = function(i)
{  extractedData <- result[[i]][[2]][[2]]
return(extractedData)
})
ISO_Code <- lapply(seq(1,length(result)), FUN = function(i)
{  extractedData <- result[[i]][3]
return(extractedData)
})
Year <- lapply(seq(1,length(result)), FUN = function(i)
{  extractedData <- result[[i]][4]
return(extractedData)
})
#as there are NAs in the imported data, I need different approach
NPL <- lapply(seq_along(result), function(i) {
  Value <- result[[i]][[5]]
  if (is.null(Value)) Value <- NA
  return(Value)
})
Country <- unlist(Country)
ISO_Code <- unlist(ISO_Code)
Year <- unlist(Year)
NPL <- unlist(NPL)
NPL_to_Total <- tibble(Country, ISO_Code, Year, NPL)
NAs <- NPL_to_Total %>% 
  filter(is.na(NPL))
NA_countries <- distinct(NAs, Country)
NA_countries <- NA_countries[[1]]
NPL_to_Total <- NPL_to_Total%>% 
  filter(!Country %in% NA_countries)



#importing Deposit Interest Rates (in Percentage)
req <- request("https://api.worldbank.org/v2/country/GEO;JPN;TUR;UZB;VNM;KOR;POL;HUN;CZE;HRV;SVK;EGY;SAU;ARE;ZAF;NGA;BRA;ARG;MEX;COL;CHL;ARM;AZE/indicator/FR.INR.DPST?per_page=230&format=json&date=2010:2019")
req_dry_run(req)
response <-  req_perform(req)
resp_status(response)
resp_status_desc(response)
result <- resp_body_json(response)
result <- result [[2]]
Country <- lapply(seq(1,length(result)), FUN = function(i)
{  extractedData <- result[[i]][[2]][[2]]
return(extractedData)
})
ISO_Code <- lapply(seq(1,length(result)), FUN = function(i)
{  extractedData <- result[[i]][3]
return(extractedData)
})
Year <- lapply(seq(1,length(result)), FUN = function(i)
{  extractedData <- result[[i]][4]
return(extractedData)
})
Deposit_Int_Rate <- lapply(seq_along(result), function(i) {
  Value <- result[[i]][[5]]
  if (is.null(Value)) Value <- NA
  return(Value)
})
Country <- unlist(Country)
ISO_Code <- unlist(ISO_Code)
Year <- unlist(Year)
Deposit_Int_Rate <- unlist(Deposit_Int_Rate)
Deposit_Int_Rate <- tibble(Country, ISO_Code, Year, Deposit_Int_Rate)
anyNA(Deposit_Int_Rate)
NAs <- Deposit_Int_Rate %>% 
  filter(is.na(Deposit_Int_Rate))
NA_countries <- distinct(NAs, Country)
NA_countries <- NA_countries[[1]]
Deposit_Int_Rate <- Deposit_Int_Rate%>% 
  filter(!Country %in% NA_countries)
str(Deposit_Int_Rate)
Deposit_Int_Rate$Deposit_Int_Rate <- as.numeric(Deposit_Int_Rate$Deposit_Int_Rate)


#importing Interest rate spread (lending rate minus deposit rate, %)
req <- request("https://api.worldbank.org/v2/country/GEO;JPN;TUR;UZB;VNM;KOR;POL;HUN;CZE;HRV;SVK;EGY;SAU;ARE;ZAF;NGA;BRA;ARG;MEX;COL;CHL;ARM;AZE/indicator/FR.INR.LNDP?per_page=230&format=json&date=2010:2019")
req_dry_run(req)
response <-  req_perform(req)
resp_status(response)
resp_status_desc(response)
result <- resp_body_json(response)
result <- result [[2]]
Country <- lapply(seq(1,length(result)), FUN = function(i)
{  extractedData <- result[[i]][[2]][[2]]
return(extractedData)
})
ISO_Code <- lapply(seq(1,length(result)), FUN = function(i)
{  extractedData <- result[[i]][3]
return(extractedData)
})
Year <- lapply(seq(1,length(result)), FUN = function(i)
{  extractedData <- result[[i]][4]
return(extractedData)
})
Int_Rate_Spread <- lapply(seq_along(result), function(i) {
  Value <- result[[i]][[5]]
  if (is.null(Value)) Value <- NA
  return(Value)
})

Country <- unlist(Country)
ISO_Code <- unlist(ISO_Code)
Year <- unlist(Year)
Int_Rate_Spread <- unlist(Int_Rate_Spread)
Int_Rate_Spread <- tibble(Country, ISO_Code, Year, Int_Rate_Spread)
anyNA(Int_Rate_Spread)
NAs <- Int_Rate_Spread %>% 
  filter(is.na(Int_Rate_Spread))
NA_countries <- distinct(NAs, Country)
NA_countries <- NA_countries[[1]]
Int_Rate_Spread <- Int_Rate_Spread%>% 
  filter(!Country %in% NA_countries)

#importing Number of Depositors with commercial banks (per 1,000 adults)
req <- request("https://api.worldbank.org/v2/country/GEO;JPN;TUR;UZB;VNM;KOR;POL;HUN;CZE;HRV;SVK;EGY;SAU;ARE;ZAF;NGA;BRA;ARG;MEX;COL;CHL;ARM;AZE/indicator/FB.CBK.DPTR.P3?per_page=230&format=json&date=2010:2019")
req_dry_run(req)
response <-  req_perform(req)
resp_status(response)
resp_status_desc(response)
result <- resp_body_json(response)
result <- result [[2]]
Country <- lapply(seq(1,length(result)), FUN = function(i)
{  extractedData <- result[[i]][[2]][[2]]
return(extractedData)
})
ISO_Code <- lapply(seq(1,length(result)), FUN = function(i)
{  extractedData <- result[[i]][3]
return(extractedData)
})
Year <- lapply(seq(1,length(result)), FUN = function(i)
{  extractedData <- result[[i]][4]
return(extractedData)
})
Depositor_Per_1000 <- lapply(seq_along(result), function(i) {
  Value <- result[[i]][[5]]
  if (is.null(Value)) Value <- NA
  return(Value)
})
Country <- unlist(Country)
ISO_Code <- unlist(ISO_Code)
Year <- unlist(Year)
Depositor_Per_1000 <- unlist(Depositor_Per_1000)
Depositor_Per_1000 <- tibble(Country, ISO_Code, Year, Depositor_Per_1000)
anyNA(Depositor_Per_1000)
NAs <- Depositor_Per_1000 %>% 
  filter(is.na(Depositor_Per_1000))
NA_countries <- distinct(NAs, Country)
NA_countries <- NA_countries[[1]]
Depositor_Per_1000 <- Depositor_Per_1000%>% 
  filter(!Country %in% NA_countries)


###Research Questions 

##Research Questions N1 - What is the relationship between Financial Institutions Stability and Social Well-being Of Society in Emerging Market Countries over the past decade? 
#I am merging both ratios for Financial Institution stability(Capital to Asset Ratio and Non-Performing Loans NPL to Total Loans) and Social Well-Being variables (Unemployment Rate and Life Expectancy) for further analysis
#For correlation analysis I use scatterplot 
RQ1 <- merge(Bank_CtoA_Ratio, NPL_to_Total, by = c("Country", "Year", "ISO_Code"))
RQ1 <- merge(LIfe_Expectancy, RQ1, by = c("Country", "Year", "ISO_Code"))
RQ1 <- merge(Unemployment_Rate, RQ1, by = c("Country", "Year", "ISO_Code"))
#as some variables consist of fewer observations, the dataset does not cover all 23 countries after merging the variables

#Correlation between CtoA ratio and Life Expectancy 
Chart_N1 <- ggplot(data=RQ1) + aes(x= Bank_CtoA_Ratio, y = LIfe_Expectancy, color = Country) + geom_jitter() + 
  theme_minimal() + labs (title = "Financial Stability and Social Well-Being")
ggplotly(Chart_N1)
#The chart shows weak positive correlation between the variables, which means more stable banks (able to deal with problems with capital) provide better life circumstances for the society at some extent.
#however, we have two most explicit outlier in the chart, countries from Africa (Nigeria and South Africa) -which probably is caused by low life expectancy in Africa
#Croatia Seems to be outlier, but actually it follows the path of positive correlation strongly, as it has both life expectancy and the ratio high. 

#Correlation Between CtoA ratio and Unemployment
Chart_N2 <- ggplot(data=RQ1) + aes(x= Bank_CtoA_Ratio, y = Unemployment_Rate, color = Country) + geom_jitter() +
  theme_minimal() + labs (title = "Financial Stability and Social Well-Being")
ggplotly(Chart_N2)
#this chart actually does not show any specific correlation, so unemployment is not directly connected to the CtoA ratio

#Correlation Between NPL to total loans and Life expectancy 
Chart_N3 <- ggplot(data=RQ1) + aes(x= NPL, y = LIfe_Expectancy, color = Country) + geom_jitter() +
  theme_minimal()
ggplotly(Chart_N3)
#chart shows weak negative correlation, as the trend is somehow downward sloping trend.
#the outliers are Hungary and Croatia, as it seems (based on the Chart N1 too), these countries have no stable banking system but being a part of EU helps their Well-Being

#Correlation Between NPL to total loans and Unemployment
Chart_N4 <- ggplot(data=RQ1) + aes(x= NPL, y = Unemployment_Rate, color = Country) + geom_jitter(alpha = 0.7) +
  theme_minimal()
ggplotly(Chart_N4)
#there is not such a correlation to make conclusions, but it may be concidered as a extremally weak correlation, so unstable economic conditions and Non-performing loans are connected.

#In conclusion, There is weak but still a relationship between Life expectancy and stability of Financial institutions, but they do not tend to be related to unemployment. 


## RQ N2 - Does the credit availability for Business Sector increase economic development of humans? 
#merging the datasets of loans given to Business sector(% of GDP) and GDP
#for dependence analysis I use Regression Analysis and scatterplot
RQ2 <- merge(Business_Loans_prcntOfGDP, GDP_USD, by = c("Country", "Year", "ISO_Code"))
#multiply Loan percentage on GDP to get absolute amount of loans
RQ2 <- RQ2 %>% 
  mutate(Business_Loans_USD_1000 = Business_Loans_prcntOfGDP * GDP_MLN_USD/1000)
RQ2 <- RQ2 %>% 
  select("Country", "Year", "ISO_Code", "Business_Loans_USD_1000")
#adding Unemployment rate as measurement of well-being
RQ2 <- merge(RQ2, Unemployment_Rate, by= c("Country", "Year", "ISO_Code"))

str(RQ2)
#using scatterplot for correlation between Loans given to Businesses and Unemployment rate, then adding regression line to see the dependance level 
Chart_N5 <- ggplot(data=RQ2) + aes(x = Business_Loans_USD_1000, y = Unemployment_Rate) + 
  geom_jitter(aes(color = Country)) +geom_smooth(method=lm, formula = y~x, se= FALSE)
ggplotly(Chart_N5)
#the result shows that there is a negative correlation between the variables, so as the Business Loans increase, Unemployment level decreases.
Regression_N1 <- lm(formula=RQ2$Unemployment_Rate ~ RQ2$Business_Loans_USD_1000)
summary(Regression_N1)
#result shows that each increasement in Loans given to the Businesses, Unemployment rate decrease by -0.0007180 (slope) 

##RQ3 - Does interest rates of loans and deposits influence Economic Prosperity of humans?
#I am merging  Interest rate Spread (Loan Interest rate minus Deposit Interest Rate) and GNI per capita - economic prosperty measurement
#I am using Regression analysis for quantifing the correlation 
RQ3 <- merge( GNI_Per_Capita_USD, Int_Rate_Spread,by = c("Country", "Year", "ISO_Code"))

#correlation between Deposit Interest Rate and GNI per Capita with Regression
#using Scatterplot and Regresson line
Chart_N6 <- ggplot(data = RQ3) + aes(x= Int_Rate_Spread, y= GNI_Per_Capita_USD) + geom_jitter(aes(color = Country)) + 
  geom_smooth(method=lm, formula = y~x, se= FALSE)
ggplotly(Chart_N6)
#there is a negative correlation that means, the higher the difference between loan and deposit interest rates, people are less likely to take loans and invest in economic growth

#regression analysis 
Regression_N2 <- lm(formula=RQ3$GNI_Per_Capita_USD ~ RQ3$Int_Rate_Spread)
summary(Regression_N2)
#as the slope (-247.8) shows, each percent growth in the Interest rate spread decreases GNI per capita by -247.8

##RQ4 - what was the trends in emerging markets of deposit interest rates and number of depositors in the previous decade and is there any connection between? 
#I use Deposit interest rate and depositors per 1000 person as variables 
#I use line charts for the time series/trend analysis 
RQ4 <- merge(Deposit_Int_Rate, Depositor_Per_1000,by = c("Country", "Year", "ISO_Code"))
str(RQ4)
RQ4$Year <- as.numeric(RQ4$Year) 
#trends in Deposit interest rate
Chart_N7 <- ggplot(RQ4) + aes(x = Year, y = Deposit_Int_Rate, group = Country, color = Country) + geom_line()
ggplotly(Chart_N7)
#tren ind GDP per capita 
Chart_N8 <- ggplot(RQ4) + aes(x= Year, y= Depositor_Per_1000, group = Country, color = Country) + geom_line()
ggplotly(Chart_N8)
#when comparing those two charts, one can say that generally speaking, number of depositors tend to be increasing despite of the changes in the interest rate, does not matter if interest rate increase, decrease or stay the same, so in general, people are saving money more and more. 


###Dashboard
#creating User Interface 
#I want to have four tabs, as each answers for one Research Question
ui <- dashboardPage(
  dashboardHeader(title = "Financial Sector Strength and Stability and its Influence on Social Well-Being", titleWidth = 800),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Banks' Stability", tabName = "tab1"),
      menuItem("Credit Availability", tabName = "tab2"),
      menuItem("Interest Rate Spread", tabName = "tab3"), 
      menuItem("Deposit Interest Rate", tabName = "tab4"),
      menuItem("Conclusion", tabName = "tab5")
    ),
    uiOutput("sidebarInput")
  ), 
  dashboardBody(
    tabItems(
      tabItem(tabName = "tab1",
              h2("Banks' Stability"),
              fluidRow(
                column(
                  width = 6,
                  box(plotlyOutput("Chart_N1"),
                      title="Capital to Asset Ratio* and Life Expectancy",
                      width=NULL),
                  p("Two most explicit outlier in the chart, countries from Africa (Nigeria and South Africa) -which probably is caused by low life expectancy in Africa")
                ), 
                column(
                  width = 6,
                  box(plotlyOutput("Chart_N2"),
                      title="Capital to Asset Ratio and Unemployment Rate",
                      width=NULL)
                )
              ),
              fluidRow(
                column(
                  width = 6,
                  box(plotlyOutput("Chart_N3"),
                      title="NPL to Total Loans** and Life Expectancy",
                      width=NULL),
                  p("The outliers are Hungary and Croatia, as it seems (based on the Chart N1 too), these countries have no stable banking system but being a part of EU helps their Well-Being")
                ),
                column(
                  width = 6,
                  box(plotlyOutput("Chart_N4"),
                      title="NPL to Total Loans and Unemployment",
                      width=NULL)
                )
              ),
              p("*Capital to Asset Ratio consists of Bank's Capital (Equity funds, retained earning - Tier 1 and Subordinated loans and Bonds - Tier 2 and Bank's Assets (everything that the bank owns - physical assets, loans, investments, etc.).
                This ratio Shows if a bank can deal with potencial losses."),
              p("**Non-Performing loans (NPL) are the loans that are concidered problematic and potentially will not be paid back.
                NPL to Total Loans ratio shows how safe and qualitative is a portfel of the bank, which is directly connected to the Stability and strength of the Financial Institurtion")
      ),
      
      tabItem(tabName = "tab2",
              h2("Credit Availability for Businesses"),
              fluidRow(
                column(
                  width = 12,
                  box(plotlyOutput("Chart_N5"),
                      title="Domestic Business debts and Unemployment Rate",
                      width=NULL)
                )
              ), 
              p("Weak Negative Correlation Shows that when loans given to businesses increase, unemployment rate decrease, and regression analysis result shows that each point increasement in Loans given to the Businesses, Unemployment rate decrease by -0.0007180 (slope)")
      ), 
      tabItem(tabName = "tab3",
              h2("Interest Rate Spread*"),
              fluidRow(
                column(
                  width = 12,
                  box(plotlyOutput("Chart_N6"),
                      title="Gross National Income (GNI) and Interest rate Spread",
                      width=NULL)
                )
              ),
              p("*Interest Rate Spread is Interest Rates for Loans - Interest Rates for Deposits in commercial banks"),
              p("There is a negative correlation that means, the higher the difference between loan and deposit interest rates, people are less likely to take loans and invest in economic growth"),
              p("From regression analysisas, the slope (-247.8) shows, each percent growth in the Interest rate spread decreases GNI per capita by -247.8")
      ),
      tabItem(tabName = "tab4",
              h2("Trends of Deposit Interest Rates and Number of Depositors in Emerging Markets In the Previous Decade"),
              fluidRow(
                column(
                  width = 12,
                  box(plotlyOutput("Chart_N7"),
                      title="Deposit Interest Rates Trends",
                      width=NULL)
                ), 
                column(
                  width = 12,
                  box(plotlyOutput("Chart_N8"),
                      title="Number of Depositors per 1000 Citizens",
                      width=NULL) 
                )
              ),
              p("When comparing those two charts, we can say that, number of depositors tend to be increasing despite of the changes in the interest rate, (it does not matter if interest rate increase, decrease or stay the same), so in general, people are saving money more and more.")
      ),
      tabItem(tabName = "tab5",
              h2("Conclusion"),
              p("Based on the Banks stability analysis, Financial sector indicators - interest rates, and their correlation with social well-being indicators
                we saw that there is a weak, but still correlation among those variables, that gives us reason to say that other than
                financial institutions, there are other influencing factors affecting prosperity of society, as the economy is not a closed ecosystem."),
              p("However, Banks and other financial institutions has big influence on the success and well-being of people, especialy in emerging markets, 
                where many opportunities for investment exist.")
      
    )
  )
)
)

#creating a Server
server <- function(input,output){
  
  output$sidebarInput <- renderUI({
    if (input$tabs == "tab1" ) { 
      #I need a checkbox to check  the countries that I want to see together
      checkboxGroupInput(
        inputId = "selectedCountries",
        label = "Select the Countries that you want to see",
        #this function shows distinct countries from my first research question dataframe to display them on the checkbox
        choices = unique(RQ1$Country),
        selected = unique(RQ1$Country), 
        width = NULL
      )
    }else if (input$tabs ==  "tab2" ) {  
      checkboxGroupInput(
        inputId = "selectedCountries",
        label = "Select the Countries that you want to see",
        choices = unique(RQ2$Country),
        selected = unique(RQ2$Country), 
        width = NULL
      )
    } else if (input$tabs ==  "tab3" ) {  
      checkboxGroupInput(
        inputId = "selectedCountries",
        label = "Select the Countries that you want to see",
        choices = unique(RQ3$Country),
        selected = unique(RQ3$Country), 
        width = NULL
      )
    } else if (input$tabs ==   "tab4" ) {  
      tagList( 
        checkboxGroupInput(
        inputId = "selectedCountries",
        label = "Select the Countries that you want to see",
        choices = unique(RQ4$Country),
        selected = unique(RQ4$Country), 
        width = NULL
      ), 
      sliderInput(
        inputId ="From",
        #here I need slider for help user filter only the years that he wants
        label="Select Which Year Do You want To Start From",
        value = 2010, min=2010, max=2019, step = 1
      ),
      sliderInput(
        inputId ="To",
        label="Select Which Year Do You want To End",
        value = 2019, min=2010, max=2019, step = 1
        )
      )
    } else if (input$tabs ==   "tab5" ) {
      NULL
    }
})
  
  #using reactive to create a dataframe the way that it makes dashboard output do whatever the user tells
  selectedValues_1 <- reactive({
    filter(RQ1, Country %in% input$selectedCountries)
  })
  selectedValues_2 <- reactive({
    filter(RQ2, Country %in% input$selectedCountries)
  })
  
  selectedValues_3 <- reactive({
    filter(RQ3, Country %in% input$selectedCountries)
  })
  
  selectedValues_4 <- reactive({
    filter(RQ4, Country %in% input$selectedCountries, 
           Year %in% c(input$From : input$To))
  })
  
  #inserting the charts for displaying on the dashboard
  #using plotly to activate mouseover
  output$Chart_N1 <- renderPlotly({
    plot_N1 <- ggplot(data=  selectedValues_1()) + aes(x= Bank_CtoA_Ratio, y = LIfe_Expectancy, color = Country) + geom_jitter() + 
      theme_minimal() + labs (title = "Financial Stability and Social Well-Being")
    ggplotly(plot_N1)
      })
  output$Chart_N2 <- renderPlotly({
    plot_N2 <- ggplot(data=selectedValues_1 ()) + aes(x= Bank_CtoA_Ratio, y = Unemployment_Rate, color = Country) + geom_jitter() +
      theme_minimal() + labs (title = "Financial Stability and Social Well-Being")
    ggplotly(plot_N2)
  })  
  output$Chart_N3 <- renderPlotly({
    plot_N3 <- ggplot(data=selectedValues_1 ()) + aes(x= NPL, y = LIfe_Expectancy, color = Country) + geom_jitter() +
      theme_minimal()
    ggplotly(plot_N3)
  })
  output$Chart_N4 <- renderPlotly({
    plot_N4 <- ggplot(data=selectedValues_1()) + aes(x= NPL, y = Unemployment_Rate, color = Country) + geom_jitter(alpha = 0.7) +
      theme_minimal()
    ggplotly(plot_N4)
  })
  
  output$Chart_N5 <- renderPlotly({
    plot_N5 <- ggplot(data=selectedValues_2 () ) + aes(x = Business_Loans_USD_1000, y = Unemployment_Rate) + 
      geom_jitter(aes(color = Country)) +geom_smooth(method=lm, formula = y~x, se= FALSE)
    ggplotly(plot_N5)
  })
  output$Chart_N6 <- renderPlotly({
    plot_N6 <- ggplot(data = selectedValues_3 ()) + aes(x= Int_Rate_Spread, y= GNI_Per_Capita_USD) + geom_jitter(aes(color = Country)) + 
      geom_smooth(method=lm, formula = y~x, se= FALSE)
    ggplotly(plot_N6)
  })
  output$Chart_N7 <- renderPlotly({
    plot_N7 <- ggplot(data = selectedValues_4 ()) + aes(x = Year, y = Deposit_Int_Rate, group = Country, color = Country) + geom_line()+scale_x_continuous(breaks=c(2010:2019))
    ggplotly(plot_N7)
  })
  output$Chart_N8 <- renderPlotly({
    plot_N8 <- ggplot(data = selectedValues_4 ()) + aes(x= Year, y= Depositor_Per_1000, group = Country, color = Country) + geom_line()+scale_x_continuous(breaks=c(2010:2019))
    ggplotly(plot_N8)
  })
}
#distplay the dashboard
shinyApp(ui,server)
