#Importing Necessary Libraries
library(tidyverse)#For data Manipulation & contains dplyr
library(janitor)# Powerful library that is used to check duplicate items & data cleaning
library(plotly) #Interactive visualization
library(esquisse)
getwd()
HH<-read.csv("C:/Users/HarryPorter.HARRYPORTER/Documents/HH.csv")
HH
CLHIV<-read.csv("C:/Users/HarryPorter.HARRYPORTER/Documents/CLHIV.csv")
CLHIV
compare_df_cols(HH,CLHIV) #Comparing data sets to be merged
compare_df_cols_same(HH,CLHIV)#Result is true, meaning Cbind or Rbind can be performed
Final_data<-merge(HH,CLHIV,all=TRUE)
Final_data
attach(Final_data)
head(Final_data) #Viewing the first 6 rows of the merged data set
view(Final_data) #Viewing the merged data set

#DATA CLEANING PROCESS
is.na(Final_data)#Checking for NA values
sum(is.na(Final_data))# Sum of missing values=76086
colSums(is.na(Final_data))
get_dupes(Final_data) #Checking for the duplicate values
clean_names(Final_data)
names(Final_data) #Displaying the column names for easy understanding
#Eliminating empty rows and columns if any
Final_data%>%
  remove_empty(c("rows","cols"))->cleaned_data
attach(cleaned_data)
cleaned_data
is.na(cleaned_data) #Checking for any missing values in the cleaned data set
cleaned_data<-subset(cleaned_data,select = -c(CGUID,CUID))#Drop unnecessary columns
cleaned_data

#Converting Data contents to upper cases as original data was had mixed cases
cleaned_data=data.frame(lapply(Final_data, 
                  function(variables){
                    if(is.character(variables)){
                      return(toupper(variables))
                    }else {
                      return(variables)
                    }
                  }),
           stringsAsFactors = FALSE)
cleaned_data #Renaming is highly recommended since R objects are IMMUTABLE!

#Re-coding Values to Missing
cleaned_data$ViralLoadStatus[cleaned_data$ViralLoadStatus==909]<-NA
cleaned_data$ViralLoadStatus[cleaned_data$ViralLoadStatus==999]<-NA
cleaned_data$ChildPSSMship[cleaned_data$ChildPSSMship==909]<-NA

na.omit(cleaned_data)->cleaned_data
cleaned_data

#Data Manipulation with tidyverse library
as_tibble(cleaned_data) #viewing data set under its specific class
#Results : dbl: double (real numbers),chr: character vectors, strings, texts,& int: integers
cleaned_data$ViralLoadStatus
glimpse(cleaned_data)
#EXPLORATORY DATA ANALYSIS
library(tidyverse)
library(DataExplorer)
library(SmartEDA)
library(ISLR)
plot_bar(cleaned_data,by="ChildPSSMship")
ExpCatViz(
  cleaned_data%>%
    select(Sex,HIV_Status)
)
attach(cleaned_data)
library(ggstatsplot)#Visualization with statistical details
ggbarstats(data = cleaned_data,x=Sex,y=HIV_Status,label = "both") #By sex
ggbarstats(data = cleaned_data,x=Sex,y=ViralLoadStatus,label = "both")# ViralLoadStatus
ggbarstats(data = cleaned_data,x=EducationStatus,y=ViralLoadStatus,label = "both") #EducationStatus
library(dlookr)#For beautifying tables
library(flexdashboard)
library(flextable)
cleaned_data<-subset(cleaned_data,select = -c(CGUID,CUID))#Drop unnecessary columns
cleaned_data
na.omit(cleaned_data)->cleaned_data

cleaned_data%>%
  group_by(Sex)%>%
  univar_numeric()
na.omit(cleaned_data)->cleaned_data

cleaned_data%>%
  group_by(Schooled)%>%
  univar_numeric()

cleaned_data%>%
  diagnose_numeric()%>%
  flextable()
#Summary tools
library(summarytools)
dfSummary(cleaned_data) #Provides a summary for the data set
glimpse(cleaned_data)
library(gtsummary)
attach(cleaned_data)
library(DataExplorer)
plot_histogram(cleaned_data)
plot_density(cleaned_data)
#Gets perfect with dplyr
cleaned_data%>%
  select(ViralLoadResults,Age)%>%
  plot_density()#Viral Load age,skewness-measure of symmetry
library(moments)
skewness(Age,na.rm = TRUE)#negatively skewed --0.3665036
skewness(DistancetoFacility,na.rm = TRUE)#Positively skewed +10.73901
agostino.test(cleaned_data$CGAge)#Research gap---for D' Agostino skewness test,values should range between 8 and 46340 to test for the hypothesis whether the data is skewed
#Kurtosis to test the presence of the outliers and normality
anscombe.test(Age) #Not normally distributed data(probable outliers),Also observe P-values
anscombe.test(CGAge)
#Proper statistical Test
# Since the data is not normally distributed, we apply non-parametric tests such as Mann-Whitney & Kruskal
#Kibrary data explorer
#NORMALITY TEST
library(ggpubr)
#dlookr to perform Shapiro-Wilk normality test
library(dlookr)
normality(cleaned_data)%>%
  mutate_if(is.numeric,~round(.,3))%>%
  flextable() #All the output fails to show normal distribution
cleaned_data%>%
  group_by(factor(Safe),HIV_Status,factor(ViralLoadStatus))%>%
  normality()


library(dplyr)
library(ggplot2)

l<-cleaned_data %>%
  filter(!(Healthy %in% "") | is.na(Healthy)) %>%
  filter(!(Safe %in% "") | is.na(Safe)) %>%
  filter(!(Stable %in% "") | is.na(Stable)) %>%
  filter(!(Schooled %in% c("", " ")) | is.na(Schooled)) %>%
  ggplot() +
  aes(x = Relatioship, fill = CGHVStatus) +
  geom_bar(position = "dodge") +
  scale_fill_hue(direction = 1) +
  labs(title = "Relationship vs CGHIV Status", subtitle = "Relationship between care giver and the HIV status") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "bottom")
ggplotly(l)

plot_boxplot(cleaned_data,by="Age") #Do really these groups differ significantly?
plot_boxplot(cleaned_data,by="CGAge")


library(dplyr)
library(ggplot2)

cleaned_data %>%
 filter(!(Healthy %in% "")) %>%
 filter(!(Safe %in% "")) %>%
 filter(!(Stable %in% "")) %>%
 filter(!(Schooled %in% c("", " "))) %>%
 ggplot() +
 aes(x = CGEducationLevel, fill = CGSex) +
 geom_bar() +
 scale_fill_hue(direction = 1) +
 labs(title = "Level of Education of the Care Givers") +
 theme_minimal()->p
ggplotly(p)
library(dplyr)
library(ggplot2)
#Exporting data to Excel
library("writexl")
library(readr)
#Exporting clead data set syntax to both xlsx & csv extensions respectively
write_xlsx(cleaned_data, "C:/Users/HarryPorter.HARRYPORTER/Documents_cleaned_data1.xlsx")
write.csv(cleaned_data,file = "C:/Users/HarryPorter.HARRYPORTER/Documents_cleaned_data.csv")

