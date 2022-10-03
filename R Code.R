##################################################################################################
###############   LOAD LIBRARIES NEEDED FOR THE ASSIGNMENT   #####################################
##################################################################################################
library(tidyverse)
library(Hmisc)
library(dplyr)
library(ggplot2)
library(data.table)
library(fs)
library(patchwork)
library(reshape2)
library(party)
library(partykit)
library(cluster)
library(ggpubr)
library(factoextra)
library(class)
library(plyr)
library(readr)
library(caret)
library(tree)
library(rpart)
library(rattle)
library(GGally)
library(lmtest)
library(car)
library(stringr)
library(rpart.plot)
library(viridis)
library(hrbrthemes)
library(NbClust)


##################################################################################################
#######################  IMPORTING ALL 2014 DATA  ################################################
##################################################################################################

file_paths <- fs::dir_ls("./2014")
file_paths

monthly_data <- list()

for (i in seq_along(file_paths)){
  monthly_data[[i]] <- read_csv(
    file = file_paths[[i]]
  )
}

monthly_data <- set_names(monthly_data, file_paths)

dim(monthly_data[[1]])
head(monthly_data)
length(monthly_data)
class(monthly_data[[1]]$`Number of Homicide Convictions`)

##################################################################################################
########   Adding All The Monthly Data Set Of 2014 To Get A Yearly / Annually Data   #############
############  Excluding The County Column and All The Columns With Percentages   #################
##################################################################################################

data_2014 <- matrix(0, nrow = 43, ncol = 25)
n <- length(monthly_data)
for (i in (1:n)){
  data_2014 = data_2014 + monthly_data[[i]][-c(1, seq(3, 51, 2))]
}

head(data_2014)
class(data_2014)
class(data_2014$`Number of Homicide Convictions`)

##################################################################################################
##############   Adding Row Names (Counties) To The 2014 Data    #################################
##################################################################################################

first_month_data <- read.csv("./principal_offence_category_january_2014.csv")
first_month_data

county <- first_month_data[,1]
county
length(county)
class(county)
dim(data_2014)

rownames(data_2014) = county
data_2014

##################################################################################################
###########   Saving The 2014 Data To CSV format In My Working Directory  ########################
##################################################################################################

getwd()
write.csv(data_2014,"./2014_Data.csv", row.names = TRUE)

##################################################################################################

##################################################################################################
#######################  IMPORTING ALL 2015 DATA  ################################################
##################################################################################################

file_paths0 <- fs::dir_ls("./2015")
file_paths0

monthly_data0 <- list()

for (i in seq_along(file_paths0)){
  monthly_data0[[i]] <- read_csv(
    file = file_paths0[[i]]
  )
}

monthly_data0 <- set_names(monthly_data0, file_paths0)

class(monthly_data0)
length(monthly_data0)
class(monthly_data0[[1]]$`Number of Homicide Convictions`)

length(monthly_data0)
dim(monthly_data0[[1]])

##################################################################################################
########   Adding All The Monthly Data Set Of 2015 To Get A Yearly / Annually Data   #############
############  Excluding The County Column and All The Columns With Percentages   #################
##################################################################################################

data_2015 <- matrix(0, nrow = 43, ncol = 25)
n <- length(monthly_data0)
for (i in (1:n)){
  data_2015 = data_2015 + monthly_data0[[i]][-c(1, seq(3, 51, 2))]
}

head(data_2015)
class(data_2015)
class(data_2015$`Number of Homicide Convictions`)

##################################################################################################
##############   Adding Row Names (Counties) To The 2015 Data    #################################
##################################################################################################

rownames(data_2015) = county
data_2015

##################################################################################################
###########   Saving The 2015 Data To CSV format In My Working Directory  ########################
##################################################################################################

write.csv(data_2015,"./2015_Data.csv", row.names = TRUE)

##################################################################################################

##################################################################################################
#######################  IMPORTING ALL 2016 DATA  ################################################
##################################################################################################

file_paths1 <- fs::dir_ls("./2016")
file_paths1

monthly_data1 <- list()

for (i in seq_along(file_paths1)){
  monthly_data1[[i]] <- read_csv(
    file = file_paths1[[i]]
  )
}

monthly_data1 <- set_names(monthly_data1, file_paths1)

class(monthly_data1)
length(monthly_data1)
dim(monthly_data1[[1]])
class(monthly_data1[[1]]$`Number of Homicide Convictions`)

##################################################################################################
########   Adding All The Monthly Data Set Of 2016 To Get A Yearly / Annually Data   #############
############  Excluding The County Column and All The Columns With Percentages   #################
##################################################################################################

data_2016 <- matrix(0, nrow = 43, ncol = 25)
n <- length(monthly_data1)
for (i in (1:n)){
  data_2016 = data_2016 + monthly_data1[[i]][-c(1, seq(3, 51, 2))]
} 

data_2016
class(data_2016)
class(data_2016$`Number of Homicide Convictions`)

##################################################################################################
##############   Adding Row Names (Counties) To The 2016 Data    #################################
##################################################################################################

rownames(data_2016) = county
head(data_2016)

##################################################################################################
###########   Saving The 2016 Data To CSV format In My Working Directory  ########################
##################################################################################################

write.csv(data_2016,"./2016_Data.csv", row.names = TRUE)

##################################################################################################

##################################################################################################
#######################  IMPORTING ALL 2017 DATA  ################################################
##################################################################################################

file_paths2 <- fs::dir_ls("./2017")
file_paths2

monthly_data2 <- list()

for (i in seq_along(file_paths2)){
  monthly_data2[[i]] <- read_csv(
    file = file_paths2[[i]]
  )
}

monthly_data2 <- set_names(monthly_data2, file_paths2)

class(monthly_data2)
length(monthly_data2)
dim(monthly_data2[[1]])
class(monthly_data2[[1]]$`Number of Homicide Convictions`)

#################################################################################################
########   Adding All The Monthly Data Set Of 2017 To Get A Yearly / Annually Data   ############
############  Excluding The County Column and All The Columns With Percentages   ################
#################################################################################################

data_2017 <- matrix(0, nrow = 43, ncol = 25)
n <- length(monthly_data2)
for (i in (1:n)){
  data_2017 = data_2017 + monthly_data2[[i]][-c(1, seq(3, 51, 2))]
}

data_2017
class(data_2017)
class(data_2017$`Number of Homicide Convictions`)

################################################################################################
##############   Adding Row Names (Counties) To The 2017 Data    ###############################
################################################################################################

rownames(data_2017) = county
head(data_2017)

################################################################################################
###########   Saving The 2014 Data To CSV format In My Working Directory  ######################
################################################################################################

write.csv(data_2017,"./2017_Data.csv", row.names = TRUE)

################################################################################################

data_2014
data_2015
data_2016
data_2017

################################################################################################
###############  VISUALIZATION FOR 2014, 2015, 2016, And 2017   ################################
################################################################################################

#### For 2014  #################################################################################

# Create The Data
S_No <- 1

National <- cbind(S_No, data_2014[1,])
National

National <- melt(National,  id.vars = 'S_No', variable.name = 'crimes_2014')

###########  For Unsuccessful Crime Convictions ####################################################

# Basic Bar Chart
National[c(seq(2, 24, 2), 25),] %>%
  ggplot(aes(x = crimes_2014, y = value, fill = crimes_2014)) + 
  geom_col() +
  coord_flip() +
  theme_classic() +
  theme(legend.position = 'none') 

# Basic Pie Chart
ggplot(National[c(seq(2, 24, 2), 25),], aes(x = "", y = value, fill = crimes_2014)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  theme_void() 

###########  For Crime Convictions ########################

#  Basic Bar Chart
National[seq(1, 23, 2),] %>%
  ggplot(aes(x = crimes_2014, y = value, fill = crimes_2014)) +
  geom_col() +
  coord_flip() +
  theme_classic() +
  theme(legend.position = 'none') 

# Basic Pie Chart
ggplot(National[seq(1, 23, 2),], aes(x = "", y = value, fill = crimes_2014)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  theme_void() 

#### For 2015  ##################################################################################

# Create The Data
S_No <- 1

National0 <- cbind(S_No, data_2015[1,])
National0

National0 <- melt(National0,  id.vars = 'S_No', variable.name = 'crimes_2015')

###########  For Unsuccessful Crime Convictions ####################################################

#  Basic Bar Chart
National0[c(seq(2, 24, 2), 25),] %>%
  ggplot(aes(x = crimes_2015, y = value, fill = crimes_2015)) +
  geom_col() +
  coord_flip() +
  theme_classic() +
  theme(legend.position = 'none')

# Basic Pie Chart
ggplot(National0[c(seq(2, 24, 2), 25),], aes(x = "", y = value, fill = crimes_2015)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  theme_void() 

###########  For Crime Convictions #######################

#  Basic Bar Chart
National0[seq(1, 23, 2),] %>%
  ggplot(aes(x = crimes_2015, y = value, fill = crimes_2015)) +
  geom_col() +
  coord_flip() +
  theme_classic() +
  theme(legend.position = 'none') 

# Basic Pie Chart
ggplot(National0[seq(1, 23, 2),], aes(x = "", y = value, fill = crimes_2015)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  theme_void() 

#### For 2016  ##################################################################################

# Create The Data
S_No <- 1

National1 <- cbind(S_No, data_2016[1,])
National1

National1 <- melt(National1,  id.vars = 'S_No', variable.name = 'crimes_2016')

###########  For Unsuccessful Crime Convictions ###################################################

#  Basic Bar Chart
National1[c(seq(2, 24, 2), 25),] %>%
  ggplot(aes(x = crimes_2016, y = value, fill = crimes_2016)) +
  geom_col() +
  coord_flip() +
  theme_classic() +
  theme(legend.position = 'none')

# Basic Pie Chart
ggplot(National1[c(seq(2, 24, 2), 25),], aes(x = "", y = value, fill = crimes_2016)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  theme_void() 

###########  For Crime Convictions #########################

#  Basic Bar Chart
National1[seq(1, 23, 2),] %>%
  ggplot(aes(x = crimes_2016, y = value, fill = crimes_2016)) +
  geom_col() +
  coord_flip() +
  theme_classic() +
  theme(legend.position = 'none') 

# Basic Pie Chart
ggplot(National1[seq(1, 23, 2),], aes(x = "", y = value, fill = crimes_2016)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  theme_void() 

#### For 2017  ##################################################################################

# Create The Data
S_No <- 1

National2 <- cbind(S_No, data_2017[1,])
National2

National2 <- melt(National2,  id.vars = 'S_No', variable.name = 'crimes_2017')

###########  For Unsuccessful Crime Convictions ####################################################

#  Basic Bar Chart 
National2[c(seq(2, 24, 2), 25),] %>%
  ggplot(aes(x = crimes_2017, y = value, fill = crimes_2017)) +
  geom_col() +
  coord_flip() +
  theme_classic() +
  theme(legend.position = 'none') 

# Basic Pie Chart
ggplot(National2[c(seq(2, 24, 2), 25),], aes(x = "", y = value, fill = crimes_2017)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  theme_void() 

###########  For Crime Convictions #################################################################

#  Basic Bar Chart
National2[seq(1, 23, 2),] %>%
  ggplot(aes(x = crimes_2017, y = value, fill = crimes_2017)) +
  geom_col() +
  coord_flip() +
  theme_classic() +
  theme(legend.position = 'none') 

# Basic Pie Chart
ggplot(National2[seq(1, 23, 2),], aes(x = "", y = value, fill = crimes_2017)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  theme_void() 

################################################################################################
##################  VISUALIZATION FOR THE FINAL DATA  ##########################################
################################################################################################

### The Final Data  ######
## Creating The Final Data  ####

final.data <- data_2014 + data_2015 + data_2016 + data_2017
final.data
head(final.data)

#### Save The Final Data As A CSV File ########################################################
write.csv(final.data,"./Final Data.csv", row.names = TRUE)

### Creating The Data For Visualization  ######################################################

crimes <- c("Number of Homicide Convictions", "Number of Homicide Unsuccessful", 
            "Number of Offences Against The Person Convictions", "Number of Offences Against The Person Unsuccessful", 
            "Number of Sexual Offences Convictions", "Number of Sexual Offences Unsuccessful", 
            "Number of Burglary Convictions", "Number of Burglary Unsuccessful", "Number of Robbery Convictions", "Number of Robbery Unsuccessful", 
            "Number of Theft And Handling Convictions", "Number of Theft And Handling Unsuccessful", 
            "Number of Fraud And Forgery Convictions", "Number of Fraud And Forgery Unsuccessful", 
            "Number of Criminal Damage Convictions", "Number of Criminal Damage Unsuccessful", 
            "Number of Drugs Offences Convictions", "Number of Drugs Offences Unsuccessful", 
            "Number of Public Order Offences Convictions", "Number of Public Order Offences Unsuccessful",
            "Number of All Other Offences (excluding Motoring) Convictions", "Number of All Other Offences (excluding Motoring) Unsuccessful", 
            "Number of Motoring Offences Convictions", "Number of Motoring Offences Unsuccessful", 
            "Number of Admin Finalised Unsuccessful")
values <- c(3319, 806, 412840, 125626, 39379, 14832, 56110, 9690, 18066, 4888, 354882, 32078, 34753, 
            5692, 89958, 15654, 173578, 11598, 151052, 26819, 63963, 12118, 337414, 56143, 34318)

dat2 <- data.frame(crimes, values)
dim(dat2)
dat2

###########  For Unsuccessful Crime Convictions ####################################################

#  Basic Bar Chart 
dat2[c(seq(2, 24, 2), 25),] %>%
  ggplot(aes(x = crimes, y = values, fill = crimes)) +
  geom_col() +
  coord_flip() +
  theme_classic() +
  theme(legend.position = 'none') 

# Basic Pie Chart
ggplot(dat2[c(seq(2, 24, 2), 25),], aes(x = "", y = values, fill = crimes)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  theme_void()

###########  For Crime Convictions ################################################################

#  Basic Bar Chart
dat2[seq(1, 23, 2),] %>%
  ggplot(aes(x = crimes, y = values, fill = crimes)) +
  geom_col() +
  coord_flip() +
  theme_classic() +
  theme(legend.position = 'none') 

# Basic Pie Chart
ggplot(dat2[seq(1, 23, 2),], aes(x = "", y = values, fill = crimes)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  theme_void() 


################################################################################################
#####  Data For Both Convictions and Unsuccessful Convictions For All Four Years  ##############
################################################################################################

####    Data For 2014   ############################################################################

Crime_Conviction_2014 <- rowSums(data_2014[,seq(1, 23, 2)])
Crime_Conviction_2014 <- as.data.frame(Crime_Conviction_2014)
Crime_Conviction_2014

Unsuccessful_Conviction_2014 <- rowSums(data_2014[,c(25,seq(2, 24, 2))])
Unsuccessful_Conviction_2014 <- as.data.frame(Unsuccessful_Conviction_2014)
Unsuccessful_Conviction_2014

####    Data For 2015   ############################################################################

Crime_Conviction_2015 <- rowSums(data_2015[,seq(1, 23, 2)])
Crime_Conviction_2015 <- as.data.frame(Crime_Conviction_2015)
Crime_Conviction_2015

Unsuccessful_Conviction_2015 <- rowSums(data_2015[,c(25,seq(2, 24, 2))])
Unsuccessful_Conviction_2015 <- as.data.frame(Unsuccessful_Conviction_2015)
Unsuccessful_Conviction_2015

####    Data For 2016   ############################################################################

Crime_Conviction_2016 <- rowSums(data_2016[,seq(1, 23, 2)])
Crime_Conviction_2016 <- as.data.frame(Crime_Conviction_2016)
Crime_Conviction_2016

Unsuccessful_Conviction_2016 <- rowSums(data_2016[,c(25,seq(2, 24, 2))])
Unsuccessful_Conviction_2016 <- as.data.frame(Unsuccessful_Conviction_2016)
Unsuccessful_Conviction_2016

####    Data For 2017   ############################################################################

Crime_Conviction_2017 <- rowSums(data_2017[,seq(1, 23, 2)])
Crime_Conviction_2017 <- as.data.frame(Crime_Conviction_2017)
Crime_Conviction_2017

Unsuccessful_Conviction_2017 <- rowSums(data_2017[,c(25,seq(2, 24, 2))])
Unsuccessful_Conviction_2017 <- as.data.frame(Unsuccessful_Conviction_2017)
Unsuccessful_Conviction_2017

######  Merging All The Yearly Data Set For Both Crime Conviction And Unsuccessful Conviction 
#######   Into One Data Set  #####

Crime_data <- cbind(Crime_Conviction_2014, Unsuccessful_Conviction_2014, Crime_Conviction_2015, 
                    Unsuccessful_Conviction_2015, Crime_Conviction_2016, Unsuccessful_Conviction_2016,
                    Crime_Conviction_2017, Unsuccessful_Conviction_2017)
dim(Crime_data)


####################################################################################################
#######  Simple Basic Bar Chart, Pie chart With Legend Outside And Circle Bar Chart  ###############
####################################################################################################

# Create Data
Crime_and_Year <- c("Crime_Conviction_2014", "Unsuccessful_Conviction_2014", "Crime_Conviction_2015", 
                    "Unsuccessful_Conviction_2015", "Crime_Conviction_2016", "Unsuccessful_Conviction_2016",
                    "Crime_Conviction_2017", "Unsuccessful_Conviction_2017")
Value <- c(545703, 107111, 470423, 98558, 391466, 79875, 327722, 64718)
dat1 <- data.frame(Crime_and_Year, Value)
dat1

# Basic Bar Chart For Crime Conviction
a1<- ggplot(dat1[seq(1, 7, 2),], aes(x = Crime_and_Year, y = Value, fill = Crime_and_Year)) + 
  geom_bar(stat = "identity") + 
  coord_flip() +
  theme_classic() +
  theme(legend.position = 'none') 

# Basic Pie Chart For Crime Conviction
b1 <- ggplot(dat1[seq(1, 7, 2),], aes(x = "", y = Value, fill = Crime_and_Year)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  theme_void()


# Basic Circle Bar Chart For Crime Conviction
c1 <- ggplot(dat1[seq(1, 7, 2),], aes(x = Crime_and_Year, y = Value, fill = Crime_and_Year)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  theme_void() 

######  Mixed Plot For Conviction ##############
joint1 <- (c1|b1)/a1

########  Combine The Plot  ###################################################
joint1 + 
  plot_annotation(tag_levels = 'A') +
  plot_layout(
    guides = 'collect'
  )

# Basic Bar Chart For Unsuccessful Conviction
a2 <-ggplot(dat1[seq(2, 8, 2),], aes(x = Crime_and_Year, y = Value, fill = Crime_and_Year)) + 
  geom_bar(stat = "identity") + 
  coord_flip() +
  theme_classic() +
  theme(legend.position = 'none')

# Basic Pie Chart For Unsuccessful Conviction
b2 <- ggplot(dat1[seq(2, 8, 2),], aes(x = "", y = Value, fill = Crime_and_Year)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  theme_void()

# Basic Circle Bar Chart For Unsuccessful Conviction
c2 <- ggplot(dat1[seq(2, 8, 2),], aes(x = Crime_and_Year, y = Value, fill = Crime_and_Year)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  theme_void()


######  Mixed Plot For Conviction ##############
joint2 <- (c2|b2)/a2

########  Combine The Plot  ###################################################
joint2 + 
  plot_annotation(tag_levels = 'A') +
  plot_layout(
    guides = 'collect'
  )


####################################################################################################
#########  Multiple Bar Chart  #####################################################################
####################################################################################################

# create a data set

Year <- c(rep(2014, 2), rep(2015, 2), rep(2016, 2), rep(2017, 2))
Crime <- rep(c('Conviction', 'Unsuccessful Conviction'), 4)
Value <- c(545703, 107111, 470423, 98558, 391466, 79875, 327722, 64718)
Value
dat <- data.frame(Year, Crime, Value)


###### IMPORTANT PLOT  ##### Grouped
######  Multiple Bar Plot
pl1 <- ggplot(dat, aes(fill=Crime, y=Value, x=Year)) + 
  geom_bar(position="dodge", stat="identity") +
  theme_classic()

##### Stacked Bar Plot
pl2 <- ggplot(dat, aes(fill=Crime, y=Value, x=Year)) + 
  geom_bar(position="stack", stat="identity") +
  theme_classic()

######## To Join Two Together  ############################################
ss <-(pl1/pl2)
ss + plot_layout(
  guides = 'collect'
) + plot_annotation(tag_levels = 'A')

################################################################################################
#####################    DESCRIPTIVE ANALYSIS   ################################################
################################################################################################
summary(data_2014[-1,])
summary(data_2015[-1,])
summary(data_2016[-1,])
summary(data_2017[-1,])

################################################################################################
######################    LINEAR REGRESSION ANALYSIS   #########################################
################################################################################################

#############  Our Data Set ####################################################

head(final.data)

########################### Data For Crime Convictions Only  ###################################

conv.data <- (final.data[-1, seq(1, 23, 2)])
conv.data1 <- conv.data[,c(2, 3, 12)]
head(conv.data1)
head(conv.data1)

##############################################################################################
############ We Shall Test Two Hypotheses Here, 1: The Output Of The Both Functions,       ###
###   2: The Time The Both Functions Takes To Run (How Fast It Takes To Run)  ################
##############################################################################################

################  We First Run The Linear Model For Both Mine and The R Function  #############

#############  My Linear Model (Akash Linear Model)  ##########################################
Akash_lm = function(data){
  n = nrow(data)
  p = ncol(data)
  data = as.matrix(data)
  X = cbind(1, data[,-1])
  Y = as.matrix(data[,1])
  XX = solve(t(X)%*%X)
  XY = t(X)%*%Y
  beta = XX%*%XY
  rownames(beta) = c("Intercept", "Number of Sexual Offences Convictions", 
                     "Number of Motoring Offences Convictions")
  return(round(beta, 4))
}

################## R Linear Model Function (lm)  ##############################################

############## Comparing Their Outputs (Akash_lm & lm)  #######################################
mine_lm <- Akash_lm(conv.data1)
r_lm<- lm(`Number of Offences Against The Person Convictions`~., data = conv.data1)

mine_lm
r_lm

summary(mine_lm)
summary(r_lm)

############  Checking The Time They Take To Run (Speed of Akash_lm & lm)  ####################
#### For Akash_lm ######
start_time <- Sys.time()
Akash_lm(trans.data)
end_time <- Sys.time()
end_time - start_time

#### For lm ###########
start_time1 <- Sys.time()
lm(`Number of Offences Against The Person Convictions`~., data = trans.data)
end_time1 <- Sys.time()
end_time1 - start_time1

###############################################################################################
##### We Continue With Our Regression Analysis For Machine Learning and Prediction Purpose ####
###############################################################################################

##################################################################################################
# To check the effect of Number of Sexual Offences Convictions and Number of Motoring Offences ###
# Convictions on the Number of Offences Against The Person Convictions and To predict the Number # 
# of Offences Against The Person Convictions using the Number of Sexual Offences Convictions and #
# Number of Motoring Offences Convictions                                                      ###
##################################################################################################

# Checking For Correlation between the dependent variable and the independent variables      
cor(conv.data1[-1], conv.data1$`Number of Offences Against The Person Convictions`)
ggpairs(conv.data1)

### Building a Linear Model ####################################################################
lm1 <- lm(conv.data1$`Number of Offences Against The Person Convictions`~., data = conv.data1)
summary(lm1)

################################################################################################
##### Test For Linear Regression Assumptions   #################################################
################################################################################################

##### Normality Assumption  ####################################################################

# Perform Shapiro-Wilk Normality Test (W)
shapiro.test(lm1$residuals) ## Failed Normality Assumption.

# Normality plot
plot(lm1, which = 2)  

##### Homoscedasticity Assumption  #############################################################

# Perform Breusch-Pagan Test (BP)
bptest(lm1) # Passed Homoscedasticity Assumption

# Homoscedasticity Plot
plot(lm1, which = 1)

##### Autocorrelation Assumption  ##############################################################

# Perform Durbin-Watson test (DW)
dwtest(lm1) ## Passed Autocorrelation Assumption


###########   Multicollinearity Assumption #####################################################

# Perform Variance Inflation Factor Test (VIF) 
vif(lm1)  ##  Passed Multicollinearity Assumption

################################################################################################
### Some of the Assumptions were met and some were not met, so we need to transform the data  ##
### We will transform the data using Cube Root Transformation                               ####
################################################################################################

trans.data <- (conv.data1)^(1/3)
head(trans.data)

# Checking For Correlation between the dependent variable and the independent variables      
cor(trans.data[-1], trans.data$`Number of Offences Against The Person Convictions`)

### Building a Linear Model ####################################################################
lm2 <- lm(trans.data$`Number of Offences Against The Person Convictions`~., data = trans.data)
summary(lm2)

################################################################################################
##### Again We Test For Linear Regression Assumptions   ########################################
################################################################################################

##### Normality Assumption Again ###############################################################

# Perform Shapiro-Wilk Normality Test (W)
shapiro.test(lm2$residuals) ## Passed Normality Assumption.

# Normality plot
plot(lm2, which = 2)  

##### Homoscedasticity Assumption Again ########################################################

# Perform Breusch-Pagan Test (BP)
bptest(lm2) # Passed Homoscedasticity Assumption

# Homoscedasticity Plot
plot(lm2, which = 1)

##### Autocorrelation Assumption Again ########################################################

# Perform Durbin-Watson test (DW)
dwtest(lm2)  ## Passed Autocorrelation Assumption

###########   Multicollinearity Assumption Again ##############################################

# Perform Variance Inflation Factor Test (VIF) 
vif(lm2)  ##  Passed Multicollinearity Assumption

################################################################################################
### All Assumptions were met so we move to splitting the data into train and test for machine ##
### Learning Purpose                                                                        ####
################################################################################################


##### Random Sample ###########
set.seed(123)
ran.samp <- sample(1:nrow(trans.data), size = 0.7 * nrow(trans.data), replace = FALSE)
ran.samp

##### Splitting the data into train and test ###########
train.data <- trans.data[ran.samp,]
head(train.data)
test.data <- trans.data[-ran.samp,]
head(test.data)

##### Building the model with the train data set ##########

lm3 <- lm(train.data$`Number of Offences Against The Person Convictions`~., data = train.data)
lm3
summary(lm3)

##### ANOVA Table To Test For Significant Of The Independent Variables  #######
anova(lm3)

####### Predicting the test data using our built model  ##############
lm.pred <- predict(lm3, newdata = test.data)

####### overview of Predicted and Actual values  ########
data.frame(Predicted = lm.pred, Actual = test.data[,1])
dd <- data.frame(Predicted = lm.pred^3, Actual = test.data[,1]^3)
dd

######### RMSE #######################################################

RMSE(pred = lm.pred, obs = test.data$`Number of Offences Against The Person Convictions`)

### Scatter Plot Showing The Relationship Between The Predicted and The Actual Value ###########
dd %>%
  ggplot(aes(x = Predicted, y = Actual)) +
  geom_jitter(size=3 , alpha = 0.5) +
  geom_smooth(method = 'lm')

################################################################################################
################# K-MEANS CLUSTERING ###########################################################
################################################################################################

##### Creating The data##################################################
County <- c(county[-1], county[-1], county[-1], county[-1])
County
county[-1]

vn <- rbind(data_2014[-1, seq(1, 23, 2)], data_2015[-1, seq(1, 23, 2)], data_2016[-1, seq(1, 23, 2)], 
            data_2017[-1, seq(1, 23, 2)])
vn
rownames(vn) <- NULL
dim(vn)
length(County)
vn0 <- cbind(County, vn)
dim(vn0)
head(vn0)

####### Standardizing The data #########################################
vn.std <- scale(vn0[-1])
vn.std
head(vn.std)

############# Elbow method Test For K Value Selection  #################
fviz_nbclust(vn.std, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")


######### K-Means clustering (K=3 From Our Elbow Test) ##########################################
set.seed(1234)
results <- kmeans(vn.std, 4)
results

###### To see the Attribute of the K-Means Clustering   ################
attributes(results)

##### To see the Center of the K-Means Clustering   ####################
results$centers

##### To see the Cluster of the K-Means Clustering   ###################
results$cluster


######  To Plot The Clusters ##########################################
fviz_cluster(results, data = vn0[, -1],
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#00AFBB"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)

########   To add The Total Conviction Column to the data  ###########
vnn <- cbind(Total.Conviction = rowSums(vn0[, seq(2, 13, 1)]), vn0)

########   To add the Clusters Column to The Data   ################## 
vnn <- cbind(Class = as.factor(results$cluster), vnn)
head(vnn)

####### To see the Cluster with the highest average Conviction #######
vnn %>%
  group_by(Class) %>%
  dplyr:: summarise(Average = mean(Total.Conviction))

#########  Box Plot Showing Crime Convictions By Class  #######################
boxplot(formula = `Number of Homicide Convictions` + `Number of Offences Against The Person Convictions` +
          `Number of Sexual Offences Convictions` + `Number of Burglary Convictions` + 
          `Number of Robbery Convictions` + `Number of Theft And Handling Convictions` + 
          `Number of Fraud And Forgery Convictions` + `Number of Criminal Damage Convictions` +
          `Number of Drugs Offences Convictions` + `Number of Public Order Offences Convictions` + 
          `Number of All Other Offences (excluding Motoring) Convictions` + 
          `Number of Motoring Offences Convictions` ~ Class, data = vnn[,c(1, 4:15)], col = 2:4, 
        ylab = "All Crime Conviction Value", main = "Box Plot Showing Crime Convictions By Class")

####  To see the Counties with High, Moderate, Low, and Very Low  #### 
### Convictions with respect to their Clusters (1, 2, 3, 4)      #####
vnn %>%
  filter(Class==1) %>%
  select(County)


############################################################################################################
############  K-NEAREST NEIGHBOR (KNN) ALGORITHMN  ##########################################################
############################################################################################################

###### Load Data  ##################################################
vn1 <- vnn[,c(1, 4:15)]

####### see the structure   ########################################
head(vn1) 
dim(vn1)

## Generate a random number that is 70% of the total number of rows in data set. ####
set.seed(1234)
ran1 <- sample(1:nrow(vn1), size = 0.7 * nrow(vn1), replace = FALSE) 
length(ran1)
ran1

######## Apply normalization  #####################################
nor <-function(x) { (x -min(x))/(max(x)-min(x)) }

## Run normalization on first 4 columns of data set because they are the predictors  ####
vn1_norm <- as.data.frame(lapply(vn1[,-1], nor))
head(vn1_norm)

##extract training set
vn1_train <- vn1_norm[ran1,] 
head(vn1_train)

##extract testing set
vn1_test <- vn1_norm[-ran1,]
head(vn1_test)

## Extract 1st column of train dataset because it will be used as 'cl' argument in knn function #####
vn1_train_class <- vn1[ran1, 1]

## Extract 1st column of test dataset to measure the accuracy
vn1_test_class <- vn1[-ran1, 1]

## Checking for the total number of rows for train target category  #######
NROW(vn1_train_class)
sqrt(117)

### Test For K value selection ############################################
set.seed(1234)
i = 1
k.optm = 1
for (i in 1:12) {
  knn.mod <- knn(train = vn1_train, test = vn1_test, cl = vn1_train_class, k = i)
  k.optm[i] <- 100 * sum(vn1_test_class == knn.mod)/NROW(vn1_test_class)
  k = i
  cat(k, '=', k.optm[i], '\n')
}

#####  Plotting The K Values ######################################
plot(k.optm, type = 'b', xlab = 'K-value', ylab = 'Accuracy Leve')

########  Run KNN function   ####################################
set.seed(1234)
pred <- knn(vn1_train, vn1_test, cl = vn1_train_class, k = 4)
pred

######## Plotting The Prediction   ##############################
plot(pred, col = 2:5)

#####  Table for Confusion Matrix  ############################## 
tab <- table(pred,vn1_test_class)
tab

#### Performing Confusion Matrix To Get Accuracy and Precision ##
confusionMatrix(tab)
confusionMatrix(vn1_test_class, pred)

###################################################################################################
########################## DECISION TREE CLASSIFICATION ###########################################
###################################################################################################

#########   Load data ############################################
vn1
head(vn1)
dim(vn1)

##########  Random Sample selection #############################
set.seed(1234)
ran.samp1 <- sample(1:nrow(vn1), size = 0.7 * nrow(vn1), replace = FALSE)

##########  Split The Data Into Train and Test  #################
train.vn1 <- vn1[ran.samp1,]
head(train.vn1)
test.vn1 <- vn1[-ran.samp1,]
head(test.vn1)

########## We Build The Decision Tree Model  ####################
dtm <- rpart(Class ~ ., train.vn1, method = "class")
summary(dtm)
dtm

##########  Plotting The Decision Tree Model ###################
plot(dtm)  ## No Text Here, We Need To Add The Text
text(dtm)  ## We Add Text and Can't Still Get A Good Decision Tree Plot

######### We Use The rpart.plot Function For A Better Decision Tree Plot #######
rpart.plot(dtm, main = "Decision Tree For Crime Convictions") 

####### Give Us A Better Look Of The Plot  #####################################
rpart.plot(dtm, type = 4, extra = 101, main = "Decision Tree For Crime Convictions") 

######## A More Nicer and Fancy Plot Decision Tree  #############################
fancyRpartPlot(dtm, main = "Decision Tree For Crime Convictions")

#######  We Can Predict Now Using The Built Model  #############################
pred1 <- predict(dtm, test.vn1, type = "class")
pred1

####### Table Of The Model For Confusion Matrix  ###############################
tab1 <- table(test.vn1$Class, pred1)
tab1

#######  Confusion Matrix To Get Accuracy Level  ###############################
confusionMatrix(tab1)

#####################################################################################################

############   Cross Validation For Model Improvement ###################################
set.seed(1234)
trControl <- trainControl(method  = "cv",
                          number  = 5)
fit1 <- train(Class ~ .,
              method     = "knn",
              tuneGrid   = expand.grid(k = 1:10),
              trControl  = trControl,
              metric     = "Accuracy",
              data       = vn1)

fit1








