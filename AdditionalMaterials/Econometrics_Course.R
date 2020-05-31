#install.packages("corrplot")
library(corrplot)
library(data.table)
install.packages("aods3")
library("aods3")

bondora <- read.csv("C:/Users/Stanislaw/Desktop/WebScrapping/Bondora/LoanData.csv")
# change data format
factorToChar <- function(df){
  for(name in names(df)){
    if(is.factor(df[, name])){
      if(length(levels(df[, name])) > 20){
        df[, name] <- as.character(df[, name])
      }
    }
  }
  return(df)
}
bondora <- factorToChar(bondora)
bondora$ReportAsOfEOD <- as.Date(bondora$ReportAsOfEOD, '%Y-%m-%d')

# some int variables are factors with levels coded by ints.
# Remember to make them binary later!


charToDate <- function(df){
  checkChar <- function(char, vec, sign){
    if(!is.character(char)){
      return(F)
    }
    if(nchar(char) < vec[1]){
      return(F)
    }
    i = 0
    for(int in vec){
      if(substr(char, int, int) == sign){
        i <- i + 1
      }
    }
    return(ifelse(i == length(vec), T, F))
  }
  
  positionOfDash <- c(5, 8)
  positionOfColon <- c(14, 17)
  for(name in names(df)){
    noNAs <- na.omit(df[, name])
    if(class(noNAs) == 'Date'){
      next
    }
    obs <- noNAs[noNAs != ''][1]
    if(checkChar(obs, positionOfDash, '-') & checkChar(obs, positionOfColon, ':')){
      df[, name] <- sapply(df[, name], function(x) strsplit(x, ' ')[[1]][1])
      df[, name] <- as.Date(df[, name], '%Y-%m-%d')
    }
    else if(checkChar(obs, positionOfDash, '-')){
      df[, name] <- as.Date(df[, name], '%Y-%m-%d')
    }
  }
  return(df)
}
bondora <- charToDate(bondora)


sum(bondora$ReportAsOfEOD > bondora$MaturityDate_Last)
# only 11279 ended loans


# check only those variables before paying the devt
variables <- c('NewCreditCustomer', 'VerificationType', 'LanguageCode', 'Age', 'Gender', 'Country', 
               'AppliedAmount', 'Amount', 'LoanDuration', 'County', 'City', 'UseOfLoan', 'Education',
               'MaritalStatus', 'NrOfDependants', 'EmploymentStatus', 'EmploymentDurationCurrentEmployer',
               'EmploymentPosition', 'WorkExperience', 'OccupationArea', 'HomeOwnershipType', 'IncomeFromPrincipalEmployer',
               'IncomeFromPension', 'IncomeFromFamilyAllowance', 'IncomeFromSocialWelfare', 'IncomeFromLeavePay',
               'IncomeFromChildSupport', 'IncomeOther', 'IncomeTotal', 'ExistingLiabilities', 'LiabilitiesTotal',
               'RefinanceLiabilities', 'DebtToIncome', 'FreeCash', 'NoOfPreviousLoansBeforeLoan',
               'AmountOfPreviousLoansBeforeLoan', 'PreviousRepaymentsBeforeLoan', 'PreviousEarlyRepaymentsBefoleLoan',
               'PreviousEarlyRepaymentsCountBeforeLoan')


bondoraModel <- bondora[variables]


# preparing variables
bondoraModel$NewCreditCustomer <- ifelse(bondoraModel$NewCreditCustomer == 'True', 1, 0)
summary(as.factor(bondoraModel$LanguageCode))

table(bondoraModel$LanguageCode)
# 3 main languages
lev <- c('Estonians', 'Finnish', 'Spanish', 'Other')

mapLanguage <- function(col){
  if(!col %in% c('1', '4', '6')){
    return('8')
  }
  return(col)
}
bondoraModel$LanguageCode <- sapply(bondoraModel$LanguageCode, mapLanguage)
bondoraModel$LanguageCode <- as.factor(bondoraModel$LanguageCode)
levels(bondoraModel$LanguageCode) <- lev
summary(bondoraModel$LanguageCode)

bondoraModel$Gender <- ifelse(is.na(bondoraModel$Gender), 2, bondoraModel$Gender)
bondoraModel$Gender <- as.factor(bondoraModel$Gender)
levels(bondoraModel$Gender) <- c('Man', 'Woman', 'Undefined')
summary(bondoraModel$Gender)

bondoraModel$AmountReceivedToApplied <- bondoraModel$Amount / bondoraModel$AppliedAmount
length(unique(bondoraModel$County))
length(unique(bondoraModel$City))
# too many cities
bondoraModel$City <- NULL

bondoraModel.dt <- data.table(bondoraModel)
bondoraModel.dt[, .N, by = County][order(-N)][1:15]
# most from Harju -> lets make binary variable
bondoraModel$IsHarju <- ifelse(bondoraModel$County %in% c('HARJU', 'Harju maakond'), 1, 0)
sum(bondoraModel$IsHarju)
bondoraModel$County <- NULL


summary(as.factor(bondoraModel$UseOfLoan))
mapUseOfLoan <- function(obs){
  # all are part of businnes, but not supported now
  if(obs %in% c(101, 102, 104, 106, 107, 108, 110)){
    return(3)
  }else if(obs == 2){
    return(1)
  }
  return(obs)
}
bondoraModel$UseOfLoan <- sapply(bondoraModel$UseOfLoan, mapUseOfLoan)
bondoraModel$UseOfLoan <- as.factor(bondoraModel$UseOfLoan)
levels(bondoraModel$UseOfLoan) <- c('Unknown', 'Loan consolidation', 'Home', 'Business',
                                    'Educations', 'Travel', 'Vehicle', 'Other', 'Health')
summary(bondoraModel$UseOfLoan)


summary(as.factor(bondoraModel$Education))
bondoraModel$Education <- ifelse(bondoraModel$Education %in% c(-1, 0), NA, bondoraModel$Education)
bondoraModel$Education <- ifelse(is.na(bondoraModel$Education), 'Undefined', bondoraModel$Education)
bondoraModel$Education <- as.factor(bondoraModel$Education)
levels(bondoraModel$Education) <- c('Primary education', 'Basic education', 
                                    'Vocational education', 'Secondary education',
                                    'Higher education', 'Undefined')

summary(as.factor(bondoraModel$MaritalStatus))
bondoraModel$MaritalStatus <- ifelse(bondoraModel$MaritalStatus == 0 | is.na(bondoraModel$MaritalStatus), -1, bondoraModel$MaritalStatus)
bondoraModel$MaritalStatus <- as.factor(bondoraModel$MaritalStatus)
levels(bondoraModel$MaritalStatus) <- c('Undefined', 'Married', 'Cohabitant', 'Single', 'Divorced',
                                        'Widow')

summary(as.factor(bondoraModel$EmploymentStatus))
bondoraModel$EmploymentStatus <- ifelse(bondoraModel$EmploymentStatus == 0 | is.na(bondoraModel$EmploymentStatus), -1, bondoraModel$EmploymentStatus)
bondoraModel$EmploymentStatus <- as.factor(bondoraModel$EmploymentStatus)
levels(bondoraModel$EmploymentStatus) <- c('Undefined', 'Partially employed',
                                           'Fully employed', 'Self-employed',
                                           'Entrepreneur', 'Retiree')

summary(bondoraModel$EmploymentDurationCurrentEmployer)
levels(bondoraModel$EmploymentDurationCurrentEmployer)[1] <- 'Undefined'

bondoraModel.dt[, .N, by = EmploymentPosition][order(-N)][1:15]
# too noise
bondoraModel$EmploymentPosition <- NULL


summary(bondoraModel$WorkExperience)
levels(bondoraModel$WorkExperience)[1] <- 'Undefined'

summary(as.factor(bondoraModel$OccupationArea))
bondoraModel$OccupationArea <- as.factor(bondoraModel$OccupationArea)
bondoraModel.dt[, .(mean(IncomeTotal, na.rm = T)), by = OccupationArea]
# seem to be redundant with Income
bondoraModel$OccupationArea <- NULL

summary(bondora$MaritalStatus)
levels(bondora$MaritalStatus) <- ifelse(levels(bondora$MaritalStatus) %in% c('Unknown', 'Widow', 'Divorced'), 'Other', levels(bondora$MaritalStatus))

summary(as.factor(bondoraModel$HomeOwnershipType))
bondoraModel$HomeOwnershipType[bondoraModel$HomeOwnershipType == '-1'] <- NA
bondoraModel$HomeOwnershipType <- ifelse(is.na(bondoraModel$HomeOwnershipType), 'Unknown', bondoraModel$HomeOwnershipType)
bondoraModel$HomeOwnershipType <- as.factor(bondoraModel$HomeOwnershipType)
summary(bondoraModel$HomeOwnershipType)

levelsHOT <- c('Homeless', 'Owner', 'Other', 'Living with parents', 'Tenant, pre-furnished property',
               'Tenant, unfurnished property', 'Council house', 'Joint tenant',
               'Joint ownership', 'Mortgage', 'Owner with encumbrance', 'Undefined')
levels(bondoraModel$HomeOwnershipType) <- levelsHOT

bondoraModel$StableIncome <- bondoraModel$IncomeFromPrincipalEmployer + 
  bondoraModel$IncomeFromPension
bondoraModel$UnstableIncome <- bondoraModel$IncomeTotal - bondoraModel$StableIncome

bondoraModel$IncomeFromChildSupport <- NULL
bondoraModel$IncomeFromFamilyAllowance <- NULL
bondoraModel$IncomeFromLeavePay <- NULL
bondoraModel$IncomeFromPension <- NULL
bondoraModel$IncomeFromPrincipalEmployer <- NULL
bondoraModel$IncomeFromSocialWelfare <- NULL
bondoraModel$IncomeOther <- NULL

summary(bondoraModel$NrOfDependants)
levels(bondoraModel$NrOfDependants) <- c('Undefined', '0', '1', '3 and more', '3 and more',
                                         '2', '3 and more', '3 and more', 
                                         '3 and more', '3 and more', '3 and more', '3 and more')
summary(bondoraModel$NrOfDependants)

summary(as.factor(bondoraModel$VerificationType))
bondoraModel$VerificationType <- ifelse(is.na(bondoraModel$VerificationType), 0, bondoraModel$VerificationType)
bondoraModel$VerificationType <- as.factor(bondoraModel$VerificationType)
levels(bondoraModel$VerificationType) <- c('Undefined', 'Income Unverified', 'Income unverified, cross-referenced by phone', 'Income verified', 'Income and expenses verified')
levels(bondoraModel$VerificationType) <- ifelse(levels(bondoraModel$VerificationType) %in% c('Income and expenses verified', 'Income verified'), 'verified', 'unverified')

summary(as.factor(bondoraModel$Gender))


sapply(bondoraModel, function(x) sum(is.na(x)))
# only 2 numerical features are left with NAs. 
# They can be imputted later

# only closed loans
bondoraModel.ended <- bondoraModel[bondora$ReportAsOfEOD > bondora$MaturityDate_Original, ]
nrow(bondoraModel.ended)
# only 12047 observations


PlannedInterestTillDate <- bondora[as.numeric(rownames(bondoraModel.ended)), 'PlannedInterestTillDate']


# compute real interest rate


EstimateTotalInterest <- function(Amo, r, m, LoanD){
  q <- 1 + (r / m)
  MonthlyPay <- (Amo * (q ^ LoanD) * (q - 1)) / ((q ^ LoanD) - 1)
  return(MonthlyPay * LoanD - Amo)
}

computeRI <- function(Amount, r, m, LoanD, PlannedInt, diff = 0.1){
  rBegin <- r
  if(is.na(Amount) | is.na(r) | is.na(LoanD) | is.na(PlannedInt)){
    return(NA)
  }
  change <- 0.0005
  TotInterest <- EstimateTotalInterest(Amount, r, m, LoanD)
  while(abs(TotInterest - PlannedInt) > diff){
    if(TotInterest > PlannedInt){
      r <- r - change
    }else{
      r <- r + change
    }
    if(r < 0.02 | (r > rBegin + 0.2)){
      print('didnt found')
      return(NA)
    }
    TotInterest <- EstimateTotalInterest(Amount, r, m, LoanD)
  }
  return(r)
}


PlannedInterestTillDate <- bondora[as.numeric(rownames(bondoraModel.ended)), 'PlannedInterestTillDate']
LoanDuration <- bondora[as.numeric(rownames(bondoraModel.ended)), 'LoanDuration']
Amount <- bondoraModel.ended$Amount
r <- bondora[as.numeric(rownames(bondoraModel.ended)), 'Interest']
m <- 12
realInterest <- numeric()
for(obs in seq(Amount)){
  realInterest <- c(realInterest, computeRI(Amount[obs], r[obs], m, LoanDuration[obs], PlannedInterestTillDate[obs], diff = Amount[obs] * 0.07))
  if(obs %% 100 == 0){
    print(obs)
  }
}
bondoraModel.ended$realInterest <- realInterest

getValue <- function(df, rows, col){
  val <- ifelse(is.na(df[as.numeric(rows), col]), 0, df[as.numeric(rows), col])
  return(val)
}

# all amount received
library(tvm)
cshfloats <- read.csv("C:/Users/Stanislaw/Desktop/WebScrapping/Bondora/RepaymentsData.csv", stringsAsFactors = F)
cshfloats <- charToDate(cshfloats)
cshfloats$paid <- cshfloats$PrincipalRepayment + cshfloats$InterestRepayment + cshfloats$LateFeesRepayment

ids <- getValue(bondora, rownames(bondoraModel.ended), 'LoanId')
Amount <- getValue(bondora, rownames(bondoraModel.ended), 'Amount')
DateStarted <- bondora[rownames(bondoraModel.ended), 'LoanDate']
RecoveryAmount <- getValue(bondora, rownames(bondoraModel.ended), 'PrincipalRecovery') +
  getValue(bondora, rownames(bondoraModel.ended), 'InterestRecovery')
# Recovery date is not known, so let it be DefaultDate + 365 days
DefaultDate <- bondora[rownames(bondoraModel.ended), 'DefaultDate']
RecoveryDate <- DefaultDate + 365

XirrOfLoan <- numeric()
for(obs in seq(ids)){
  flows <- cshfloats[cshfloats$loan_id == ids[obs], ]
  flows <- flows[order(flows$Date), ]
  if(is.na(RecoveryDate[obs]) | RecoveryAmount[obs] == 0){
    tryCatch({
      XirrOfLoan <- c(XirrOfLoan, xirr(c(-Amount[obs], flows$paid), c(DateStarted[obs], flows$Date)))
    }, warning = function(w) {
      XirrOfLoan <<- c(XirrOfLoan, -1)
    }, error = function(e) {
      print(length(flows$paid))
      print('error no recovery')
      XirrOfLoan <<- c(XirrOfLoan, -1)
    })
  }else{
    tryCatch({
      XirrOfLoan <- c(XirrOfLoan, xirr(c(-Amount[obs], flows$paid, RecoveryAmount[obs]), c(DateStarted[obs], flows$Date, RecoveryDate[obs])))
    }, warning = function(w) {
      XirrOfLoan <<- c(XirrOfLoan, 0)
    }, error = function(e) {
      print('error ecovery')
      XirrOfLoan <<- c(XirrOfLoan, 0)
    })
  }
}

ReceivedToGave <- numeric()
for(obs in seq(ids)){
  flows <- cshfloats[cshfloats$loan_id == ids[obs], ]
  flows <- flows[order(flows$Date), ]
  flowsSum <- sum(flows$paid)
  if(is.na(RecoveryDate[obs]) | RecoveryAmount[obs] == 0){
    ReceivedToGave <- c(ReceivedToGave, flowsSum / Amount[obs])
  }else{
    ReceivedToGave <- c(ReceivedToGave, (flowsSum + RecoveryAmount[obs]) / Amount[obs])
  }
}

bondoraModel.ended$Xirr <- XirrOfLoan
bondoraModel.ended$GainLoss <- ifelse(bondoraModel.ended$Xirr < 0, 0, 1)
write.csv(bondoraModel.ended, 'C:/Users/Stanislaw/Desktop/WebScrapping/Bondora/Project/bondoraModel_ended.csv', row.names = F)

# PYTHON PART - FEATURE SELECTION


#end


# Explanatory part
load.libraries <- c('data.table', 'testthat', 'gridExtra', 'corrplot', 'GGally', 'ggplot2', 'e1071', 'dplyr')
install.lib <- load.libraries[!load.libraries %in% installed.packages()]
for(libs in install.lib) install.packages(libs, dependences = TRUE)
sapply(load.libraries, require, character = TRUE)

getwd()
setwd("/Users/irynabazaka/Desktop/AE_models")
bondora <- read.csv("bondoraPythonFS.csv")

dim(bondora)
summary(bondora)
head(bondora)
prop.table(table(bondora$GainLoss))
# in 23% of cases investore lost their money

depend <- 'GainLoss'
independ <- names(bondora)
independ <- independ[independ != depend]
num <- sapply(bondora[independ], function(x) is.numeric(x))
num <- names(bondora[independ][unname(num)])
num <- num[num != 'Default' & num != 'IsHarju']
factors <- independ[!independ %in% num]
# imputting

# use column Default to imput mean based on that variable
nas <- sapply(bondora, function(x) sum(is.na(x)))
nas[nas > 0]
# WhenDefault_Days has null columns because not all borrowers in dataset went default.
# the others value must be imputted

# assumption: colToDivide is binary
imputByMean <- function(col, data, colToDivide){
  colNas <- data[is.na(data[, col]), c(col, colToDivide)]
  colNas[, col] <- ifelse(colNas[, colToDivide] == 0, mean(data[data[, colToDivide] == 0, col], na.rm = T), mean(data[data[, colToDivide] == 1, col], na.rm = T))
  return(colNas[, col])
}
bondora$FreeCash[is.na(bondora$FreeCash)] <- imputByMean('FreeCash', bondora, 'IsHarju')
bondora$DebtToIncome[is.na(bondora$DebtToIncome)] <- imputByMean('DebtToIncome', bondora, 'IsHarju')

nas <- sapply(bondora, function(x) sum(is.na(x)))
nas[nas > 0]
# ok


levels(bondora$HomeOwnershipType)[9] <- 'tenant prefur'
levels(bondora$HomeOwnershipType)[10] <- 'tenant unfur'
levels(bondora$HomeOwnershipType)[8] <- 'Owner encumb'
## Functions for barplots and distributions
bondora.dt <- data.table(bondora)
bondoraCat <- bondora.dt[,.SD, .SDcols = factors]
bondoraNum <- bondora.dt[,.SD,.SDcols = num]

plotHist <- function(data_in, i, fill) {
  f <- as.factor(bondora[, fill])
  data <- data.frame(x=data_in[[i]])
  p <- ggplot(data=data, aes(x=factor(x))) + geom_bar(aes(fill = f)) + xlab(colnames(data_in)[i]) + theme_light() + 
    theme(axis.text.x = element_text(angle = 45, hjust =1))+ labs(fill = fill) 
  return (p)
}

doPlots <- function(data_in, fun, fill, ii, ncol=3) {
  pp <- list()
  for (i in ii) {
    p <- fun(data_in=data_in, fill, i=i)
    pp <- c(pp, list(p))
  }
  do.call("grid.arrange", c(pp, ncol=ncol))
}

plotDen <- function(data_in, i, fill){
  data <- data.frame(x = data_in[[i]])
  quant3 <- quantile(data$x, probs = 0.75, na.rm = T)
  f <- as.factor(bondora[data$x < 4 * unname(quant3), fill])
  data <- data.frame(x = data$x[data$x < 4 * unname(quant3)])
  print(paste(colnames(data_in)[i], ': removed', length(data_in[[i]]) - nrow(data), 'observations'))
  p <- ggplot(data= data, aes(x, fill = f)) + geom_density(position = 'stack') +
    xlab(paste0((colnames(data_in)[i]), '\n', 'Skewness: ',round(skewness(data_in[[i]], na.rm = TRUE), 2))) + 
    theme_bw() + labs(fill = fill) 
  return(p)
  
}  


# Barplots for categorical variables
doPlots(bondoraCat, fun = plotHist, fill = 'GainLoss', ii = 1:10, ncol = 3)

# Comments:
# The most borrowers (71,66 %) in our dataset are located in EE (what's the name of country?) and where 82,82 % of them are fully employed.
# What isn't surprised, men are 1,4 times more likely to take loan than women.
# The most popular language among borrowers is Estonia <- which is 64,38 % of all announced languages).
# The most popular rating HR, which is the riskiest “investment grade” rating. Minimum 
#  expected  losses per risk rating HR is 25% of amonut. (https://support.bondora.com/hc/en-us/articles/212798989-Risk-scoring)

# plot without massive outliers
doPlots(bondoraNum, fun = plotDen, fill = 'GainLoss', ii = 1:8, ncol = 2)
doPlots(bondoraNum, fun = plotDen, fill = 'GainLoss', ii = 9:14, ncol = 2)



makeFormula <- function(dep, indep){
  #indep <- indep[indep != interacts1 & indep != interacts2]
  form <- paste0(dep, ' ~ ')
  for(var in indep){
    if(var == indep[length(indep)]){
      form <- paste0(form, var)
    }else{
      form <- paste0(form, var, ' + ')
    }
  }
  return(form)
}


corr = cor(bondora[num], use = 'complete.obs', method = 'pearson')
corrplot(corr, method="circle")
# must exclude correlated highly variables 

# StableIncome, IncomeTotal
# Amount, AppliedAmount

num <- num[num != 'StableIncome' & num != 'AppliedAmount' & num != 'AmountOfPreviousLoansBeforeLoan ']
corr = cor(bondora[num], use = 'complete.obs', method = 'pearson')
corrplot(corr, method="circle")
# ok

independ2 <- c(num, factors)


library(MASS)
# example for factor correlations
for(var in factors){
  print(var)
  tab <- table(bondora[, 'IsHarju'], bondora[, var])
  test <- chisq.test(tab)
  print(paste('IsHarju', var, test$p.value))
}


makeReg <- function(form, data, type){
  if(type == 'linear'){
    reg <- lm(form, data = data)
  }else{
    reg <- glm(form, data = data, family = binomial(link = type))
  }
  summ <- summary(reg)
  coefs <- summ$coefficients
  coefs <- coefs[, 4]
  #coefs <- coefs[coefs > 0.05 & names(coefs) != names(coefs)[1]]
  return(list(reg, coefs))
}

joinSignif <- function(model1, model2){
  ano <- anova(model1, model2, test="LRT")
  pval <- ano$`Pr(>Chi)`[2]
  print(ano)
  return(pval <= 0.05) # if null can be rejected
}

# one hot encoder
# ADD THE CODE TO ELIMINATE ONE LEVEL THAT WOULD BE THE 

factToBin <- character()
for(col in factors[factors != 'Default' & factors != 'IsHarju']){
  levels_ <- levels(bondora[, col])
  # eliminate first level to be the default case
  levels_ <- levels_[levels_ != levels_[1]]
  for(lev in levels_){
    name <- paste0(trimws(col), gsub(' ', '_', lev))
    bondora[, name] <- ifelse(bondora[, col] == lev, 1, 0)
    lev <- gsub(' ', '_', lev)
    factToBin <- c(factToBin, name)
  }
  bondora[, col] <- NULL
}


### Przykład

regularization <- function(vars, type, data){
  formula <-makeFormula(depend, vars)
  # step 1
  excluded <- character()
  regOutput <- makeReg(formula, data, type)
  model <- regOutput[1][[1]]
  coefs <- regOutput[2][[1]]
  insig <- coefs[coefs > 0.05 & names(coefs) != '(Intercept)']
  sig <- coefs[coefs <= 0.05 & names(coefs) != '(Intercept)']
  if(length(insig) > 0){
    formula <- makeFormula(depend, names(sig))
    regSig <- makeReg(formula, data, type)
    model2 <- regSig[1][[1]]
    if(joinSignif(model, model2)){
      # exclude most insignificant var
      excluded <- names(insig)[insig == max(insig)]
      coefs <- names(coefs)
      coefs <- coefs[!coefs %in% excluded]
      coefs <- coefs[coefs != '(Intercept)']
      oneByOne(coefs, excluded, model, type, data)
    }else{
      return(model2)
    }
  }
}

# BEORE MODELING, check possible interactions

doPlots(bondoraCat, fun = plotHist, fill = 'IsHarju', ii = 1:10, ncol = 3)
# candidats for interactions: EducationxIsHarju, ESxIsHarju, FIxIsHarju
doPlots(bondoraCat, fun = plotHist, fill = 'IsHarju', ii = 1:10, ncol = 3)



doPlots(bondoraNum, fun = plotDen, fill = 'IsHarju', ii = 1:8, ncol = 2)
doPlots(bondoraNum, fun = plotDen, fill = 'IsHarju', ii = 9:14, ncol = 2)
# ExsistingLiabilitiesxIsHarju

# Maybe age x IncomeTotal?

withoutInters <- length(names(bondora)) + 1

# Adding Interactions
matr <- model.matrix(lm1 <- lm(GainLoss ~ EducationVocational_education:IsHarju, data = bondora))[, 2]
EducationVocational_educationXIsHarju <- unname(matr)
bondora$EducationVocational_educationXIsHarju <- EducationVocational_educationXIsHarju

matr <- model.matrix(lm1 <- lm(GainLoss ~ EducationVocational_education:CountryES, data = bondora))[, 2]
EducationVocational_educationXCountryES <- unname(matr)
bondora$EducationVocational_educationXCountryES <- EducationVocational_educationXCountryES

matr <- model.matrix(lm1 <- lm(GainLoss ~ IsHarju:ExistingLiabilities, data = bondora))[, 2]
IsHarjuXExistingLiabilities <- unname(matr)
bondora$IsHarjuXExistingLiabilities <- IsHarjuXExistingLiabilities

matr <- model.matrix(lm1 <- lm(GainLoss ~ Age:IncomeTotal, data = bondora))[, 2]
AgeXIncomeTotal <- unname(matr)
bondora$AgeXIncomeTotal <- AgeXIncomeTotal


interacts <- names(bondora)[withoutInters : length(names(bondora))]

givePValue <- function(data, n){
  formula = makeFormula('GainLoss', n)
  reg <- makeReg(formula, bondora, 'logit')
  coefs <- reg[2][[1]]
  return(coefs[2])
}

# now add some non linear relationships.
nonlinears <- character()
for(n in num){
  bondora[, paste0(n, '2')] <- bondora[, n] ^ 2
  pVal1 <- givePValue(bondora, n)
  pVal2 <- givePValue(bondora, paste0(n, '2'))
  better <- min(pVal1, pVal2)
  if(pVal1 == better){
    print(paste(names(pVal1), 'is better!'))
  }else{
    print(paste(names(pVal2), 'is better!'))
    nonlinears <- c(nonlinears, names(pVal2))
  }
}

for(n in num){
  bondora[, paste0(n, '2')] <- bondora[, n] ^ 2
  nonlinears <- c(nonlinears, paste0(n, '2'))
}


allVars <- c(num, factToBin, interacts, nonlinears)

logit <- regularization(allVars, 'logit', bondora)
summary(logit)

probit <- regularization(allVars, 'probit', bondora)
summary(probit)

# linear probability model
lpm = regularization(allVars, 'linear', bondora)
summary(lpm)



logit$aic
probit$aic
# Probit is better, because AIC statistic has lower value.

# pseudo Rs
library("BaylorEdPsych")

PseudoR2(probit)

# link test
bondora$yhat <- log(probit$fitted.values/(1 - probit$fitted.values))
bondora$yhat2 <- bondora$yhat^2
linkTest = glm(GainLoss ~ yhat + yhat2, data = bondora, family = binomial(link="probit"))
summary(linkTest)
# ok? I guess

install.packages('glmnet')
library(glmnet)

variables <- names(probit$coefficients)
# exclude intercept
variables <- variables[2:length(variables)]

#convert training data to matrix format
formula <- makeFormula('GainLoss', variables)
x <- model.matrix(eval(parse(text = formula)), bondora)
y <- bondora$GainLoss
# grid search
regLasso <- cv.glmnet(x, y, alpha = 1, family = 'binomial', type.measure = 'auc')
# dont insert plot in raport
plot(regLasso)

# grid search for lambda results
# select lambda as big as possible while AUC is as high as possible

print(paste0('max auc: ', max(regLasso$cvm)))

# print all auc combinations
regLasso$cvm


# 26 auc seem to be good compromise
regLasso$cvm[24]
#corresponding lambda value
regLasso$lambda[24]
coefs <- coef(regLasso, s = regLasso$lambda[24])

# extract coefs > 0 
coefs.df <- data.frame(name = coefs@Dimnames[[1]][coefs@i + 1], coefficient = coefs@x)
# train logit model once again
formula <- makeFormula('GainLoss', coefs.df$name[-1])

probit2 <- glm(formula, data = bondora, family = binomial(link = 'probit'))
summary(probit2)

bondora$yhat <- log(probit2$fitted.values/(1 - probit2$fitted.values))
bondora$yhat2 <- bondora$yhat^2
linkTest = glm(GainLoss ~ yhat + yhat2, data = bondora, family = binomial(link="probit"))
summary(linkTest)

# not a big drop in aic, accepted with better generalization when model has less variables
probit$aic
probit2$aic

# Marginal effect
library(mfx)
probitmfx(formula, data = bondora, atmean = TRUE)

PseudoR2(probit2)
# Goodness-of-fit tests
library(ResourceSelection)
hoslem.test(bondora$GainLoss, probit2$fitted.values)
