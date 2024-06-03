
# **********************************************************************************
# 1.                 INITIAL DB LOAD AND PACKAGES  ----
# **********************************************************************************


# 1.1 Packages ----

# install.packages("ResourceSelection") 
# install.packages("pROC")
# install.packages("ggpubr")
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("tidyverse")
# install.packages("stargazer")
# install.packages("mlogit")
# install.packages("ROCR")
# install.packages("lintr")
# install.packages("class")
# install.packages("mice") 
# install.packages("missForest")
# install.packages("caret")

library(readxl)
library(tidyverse)
library(dplyr)
library(stargazer)
library(mlogit)
library(ROCR)
library(ggplot2)
library(rmarkdown)
library(ggpubr)
library(pROC)
library(DescTools)
library(ResourceSelection)
library(stargazer)
library(lintr)
library(class)
library(mice)
library(missForest)
library(caret)



# 1.2 Load DB  ----

TOTALDATA <- read_excel("C:/Users/david/OneDrive/Documents/DAVID/FORMATOS/DATOS.xlsx" , sheet = "DATOS")

# To avoid scientific notation in command results:
options(scipen = 999) 

# check table dimensions and description of variables
dim(TOTALDATA)	  # (36410, 55)
str(TOTALDATA)

# see what the data looks like
head(TOTALDATA)

# let's take a quick summary of the data
summary(TOTALDATA)




# *******************************************************************************
# 2.                      INITIAL DATA TRANSFORMATION  ----
# *******************************************************************************

# Since the original data base, we need a initial transformation that it will have the following objectives:

  # New names variables with respect original names (from spanish to english)
  # Only filter people's data
  # Monetary values in terms of millions
  # Numbers by text in categorical variables


DATA <- TOTALDATA %>%
  
  mutate(
    # Convert date format:
    Date = as.Date(PERIODO),
    
    # Filter by removing client type 9 legal entity (we want people data):
    CodTypeClient = `Codigo Tipo de Asociado`,) %>% 
    filter(`Codigo Tipo de Asociado` != 9) %>% 
  
  mutate(
    
    # DEPEND VARIABLE:
    # Good status if the rating is A:
    Status = ifelse(`Calificacion antes de Arrastre (Cierre)`=="A","good","bad"),
    Status = as.factor(Status),
    
  
  # INDEPENDENT VARIABLES: Categorical
    
    # If the obligation has a asset as real warranty:
    Warranty = ifelse(`% Cobertura de la Garantia (Cierre)` == 70, "yes", "not"),
    Warranty = as.factor(Warranty),
    
    # If the obligation has a co-debtor:
    CoDebtor = ifelse(Codeudor == "SI", "yes", "not"),
    CoDebtor = as.factor(CoDebtor),
    
    # Sex (male or female):
    Sex = as.factor(Sexo),
    
    # Job occupation:
    Job = as.factor(Ocupacion),
    
    # Type of work contract:
    TypeContract = as.factor(`Tipo de contrato`),
    
    # Level education:
    Education = as.factor(`Nivel Estudios`),
    
    # Civil status:
    CivilStatus = as.factor(`Estado civil`),
    
    # Pay form (payroll or counter):
    PayForm = as.factor(`Forma de Pago`),  
    
    
  # INDEPENDENT VARIABLES: Continuous
    
    # Initial number / term of fees credit (in months):
    NumFees = Num.Cuotas,
    
    # Income in millions (the currency is COP):
    Income = Sueldo / 1000000,
    
    # Contributions as client in millions (the currrency is COP):
    Contributions = `Total Aportes` / 1000000,
    
    # Credit amount in millions (the currency is COP):
    Amount = Monto / 1000000,
    
    # Capital balance / debt balance of credit in millions (the currency is COP):
    MonthDebt = `Endeudamiento mensual` / 1000000,
    
    # The credit fee in millions (the currency is COP):
    Fee = `Valor Cuota` / 1000000,
    
    # Labor old of client (in days):
    LaborOld = `Antiguedad laboral dias`,
    
    # Length of time as a client (in days):
    ClientOld = `Antiguedad asociado dias`,
    
    # Years old of borrower:
    Years = Edad
  ) %>% 
  
  # Select only the new variables created:
  select(Date, CodTypeClient, 
         # Categorical variables:
         Status, Warranty, CoDebtor, Sex, Job, TypeContract, Education, CivilStatus, PayForm, 
         
         #Continuous variables:
         NumFees, Income, Contributions, Amount, MonthDebt, Fee, LaborOld, ClientOld, Years)

dim(DATA)	  # (36192, 20)
str(DATA)



# *******************************************************************************
# 3.                    EXPLORATORY DATA ANALYSIS  ----
# *******************************************************************************

# Objective of exploratory data analysis

# * check maximum and minimum values
# * check the scale of the variables (categorical, continuous, etc)
# * look for outliers, weird values, missing values


# 3.1. Check categorical variables  ----
# *******************************************

# Identify NA values

sum(is.na(DATA$CodTypeClient))
sum(is.na(DATA$Warranty))
sum(is.na(DATA$CoDebtor))
sum(is.na(DATA$PayForm))
sum(is.na(DATA$Sex))
sum(is.na(DATA$Status))
sum(is.na(DATA$Job))
sum(is.na(DATA$Education))
sum(is.na(DATA$CivilStatus))
sum(is.na(DATA$TypeContract))
sum(is.na(DATA$Job))
sum(is.na(DATA$PayForm))
sum(is.na(DATA$Date))
# According to the last, there are some NA values


# Create DF with relevant categorical variables:
Categorical <- DATA %>% 
  select(Warranty, CoDebtor, PayForm, Sex, Status,
         Job, Education, CivilStatus, TypeContract, Job)


# Initialize vector and iteration over each column and count the NA values. At the end it prints results:
NA_categorical <- c()
for (col in colnames(Categorical)) {
  NA_categorical[col] <- sum(is.na(Categorical[[col]]))}
print(NA_categorical)
# Total missing values: Civil/Marital Status 8, and Contract Type 305.


# Elimination of missing values:
DATA <- DATA %>% 
  drop_na(CodTypeClient, Date, Warranty,
          CoDebtor, PayForm, Sex, Status,
          Job, Education, CivilStatus, TypeContract, Job)


# check new table dimensions:
dim(DATA)	  # (35879, 20)
# The total quantity of NA values is 313. Because the sum of NA values do not exceed 5% of original DB, it is statistically valid to remove them.



# 3.2. Check continuous variables ----
# *******************************************

# Identify NA values:

sum(is.na(DATA$Amount))
sum(is.na(DATA$Years))
sum(is.na(DATA$Income))
sum(is.na(DATA$LaborOld))
sum(is.na(DATA$ClientOld))
sum(is.na(DATA$NumFees))
sum(is.na(DATA$MonthDebt))
sum(is.na(DATA$Contributions))
# According to the last, there are some NA values


# How many zeros (this is just for curiosity):
sum(DATA$Income == 0, na.rm = TRUE)
sum(DATA$MonthDebt == 0, na.rm = TRUE)
sum(DATA$LaborOld == 0, na.rm = TRUE)
sum(DATA$ClientOld == 0, na.rm = TRUE)
sum(DATA$Contributions == 0, na.rm = TRUE) 


# Create DF with relevant continuous variables:
Continuous <- DATA %>% 
  select(Amount , Years , Income , LaborOld , ClientOld ,
         NumFees ,MonthDebt , Contributions)


# Initialize vector and iteration over each column and count the NA values. At the end it prints results:
NA_continuous <- c()
for (col in colnames(Continuous)) {
  NA_continuous[col] <- sum(is.na(Continuous[[col]]))}
print(NA_continuous)
# Total missing values: Income 10 and Labor Old 4157.
# The total quantity of NA values is 4167. In this case, the quantify exceeds the 5% of original DB,
# so, we must not eliminated the NA values.




# 3.2.1. Machine Learning: Imputation by regression for NA values ----
# **********************************************************************

# Since there are a lot of missing values in the continuous variables, specially the Income ans Labor Old variables,
# we need to do some imputation. In this case, let's apply Imputation by regression.

# Imputation for the Income variable using the median:
DATA$Income[is.na(DATA$Income)] <- median(DATA$Income, na.rm = TRUE)
sum(is.na(DATA$Income))



# For the LaborOld imputation, we install the packages:
# install.packages("caret")
library(caret)

# Split the data set into two parts: one with known values and one with missing values in "LaborOld":
LaborOld_known <- DATA[!is.na(DATA$LaborOld), ]
LaborOld_unknown <- DATA[is.na(DATA$LaborOld), ]

# Train a regression model to predict "LaborOld" values based on other variables:
model <- train(LaborOld ~ ., data = LaborOld_known, method = "lm")

# Predict NA values of "LaborOld" using the trained model:
predicted_LaborOld <- predict(model, newdata = LaborOld_unknown)

# Replace missing values in "LaborOld" with model predictions:
DATA$LaborOld[is.na(DATA$LaborOld)] <- predicted_LaborOld

# Verify that there are no longer missing values in "LaborOld"
sum(is.na(DATA$LaborOld))



# 3.3. Define new variables  ----
# **********************************************************************

# Respect to the variables in DATA, we can make some interest financial variables:

DATA <- DATA %>% 
  mutate(
    SavingCapacity = ((Income - Fee) / Income),
    ContribCapacity = (Contributions / Income),
    DebtRatio = (MonthDebt / Income)
  )


# Crear un histograma usando ggplot2
ggplot(DATA, aes(x = SavingCapacity)) + 
  geom_histogram(binwidth = 0.02, fill = "gray85", color = "black") + 
  labs(title = "Saving Capacity", x = "Rate", y = "Frequency") +
  xlim(0, 1)

# Crear un histograma usando ggplot2
ggplot(DATA, aes(x = ContribCapacity)) + 
  geom_histogram(binwidth = 0.02, fill = "gray85", color = "black") + 
  labs(title = "Contribution Capacity", x = " Rate", y = "Frequency") +
  xlim(0, 1)

# Crear un histograma usando ggplot2
ggplot(DATA, aes(x = DebtRatio)) + 
  geom_histogram(binwidth = 0.02, fill = "gray85", color = "black") + 
  labs(title = "Debt Ratio", x = "Rate", y = "Frequency") +
  xlim(0, 1)


# **********************************************************************
# 4.         RECODING (CATEGORIZING) CONTINUOUS VARIABLES  ----
# **********************************************************************

# Purpose of recoding:

  # 1. Facilitate analysis: Categorical variables can simplify the analysis and 
  # interpretation of data. It is easier to analyze categories that analyze a 
  # wide range of continuous values.

  # 2. Improve modeling: Some machine learning and statistical models work better 
  # with categorical variables or may require them. For example, decision trees 
  # and rule-based models can benefit from categorical variables.

  # 3. Detect patterns: Grouping continuous values into categories can help 
  # identify patterns and relationships that are not evident in continuous data.

names(Continuous)

# Categorizing:
AmountC = cut(DATA$Amount, breaks=c(0, 5, 10, 15, 20, 30, 60, 100, 200, 400, max(DATA$Amount)), include.lowest=TRUE)
YearsC= cut(DATA$Years, breaks=c(0, 18, 25, 30, 40, 50, 60, max(DATA$Years)), include.lowest=TRUE)
IncomeC = cut(DATA$Income, breaks=c(0, 1, 1.5, 2, 2.5, 3, 4, 6, 8, max(DATA$Income)), include.lowest=TRUE)
LaborOldC = cut(DATA$LaborOld, breaks=c(0, 1, 3, 5, 10, 20, max(DATA$LaborOld)), include.lowest=TRUE)
ClientOldC = cut(DATA$ClientOld, breaks=c(0, 1, 3, 5, 8, 10, max(DATA$ClientOld)), include.lowest=TRUE)
NumFeesC = cut(DATA$NumFees, breaks=c(0, 6, 12, 24, 36, 48, 60, max(DATA$NumFees)), include.lowest=TRUE)
MonthDebtC = cut(DATA$MonthDebt, breaks=c(0, 0.5, 1, 1.5, 2, max(DATA$MonthDebt)), include.lowest=TRUE)
ContributionsC = cut(DATA$Contributions, breaks=c(0, 0.5, 1, 2, 4, 6, 8, 10, max(DATA$Contributions)), include.lowest=TRUE)


# Category labeling for recoded continuous variables:
levels(AmountC) = paste("amount", levels(AmountC))
levels(YearsC) = paste("year", levels(YearsC))
levels(IncomeC) = paste("income", levels(IncomeC))
levels(LaborOldC) = paste("labOld", levels(LaborOldC))
levels(ClientOldC) = paste("clientOld", levels(ClientOldC))
levels(NumFeesC) = paste("numFees", levels(NumFeesC))
levels(MonthDebtC) = paste("monthDebt", levels(MonthDebtC))
levels(ContributionsC) = paste("contrib", levels(ContributionsC))


# Add categorized variables to the dataframe:
DATA$AmountC = AmountC
DATA$YearsC = YearsC
DATA$IncomeC = IncomeC
DATA$LaborOldC = LaborOldC
DATA$ClientOldC = ClientOldC
DATA$NumFeesC = NumFeesC
DATA$MonthDebtC = MonthDebtC
DATA$ContributionsC = ContributionsC

# Saving:
write.csv(DATA, "InternalCreditScoring.csv", row.names=FALSE)















