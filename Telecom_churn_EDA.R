library(dplyr)
library(ggplot2)
telecom=read.csv(file.choose(),header = T,stringsAsFactors = T)

View(telecom)

glimpse(telecom)

str(telecom)

telecom$customerID=NULL

sum(is.na(telecom))

sapply(telecom, function(x) sum(is.na(x)))

sum(is.na(telecom$TotalCharges))/nrow(telecom)

telecom=na.omit(telecom)
table(telecom$Churn)


telecom$SeniorCitizen=ifelse(telecom$SeniorCitizen==1,"Yes","No")

ggplot(telecom, aes(x=gender,fill=Churn))+ geom_bar()

ggplot(telecom, aes(x=SeniorCitizen,fill=Churn))+ geom_bar(position = 'fill')

ggplot(telecom, aes(x=Partner,fill=Churn))+ geom_bar(position = 'fill')

ggplot(telecom, aes(x=Dependents,fill=Churn))+ geom_bar(position = 'fill')

ggplot(telecom, aes(x=PhoneService,fill=Churn))+ geom_bar(position = 'fill')

ggplot(telecom, aes(x=MultipleLines,fill=Churn))+ geom_bar(position = 'fill')

#Gender - The churn percent is almost equal in case of Male and Females
#The percent of churn is higher in case of senior citizens
#Customers with Partners and Dependents have lower churn rate as compared to those who don't have partners & Dependents.
ggplot(telecom, aes(x=InternetService,fill=Churn))+ geom_bar()
ggplot(telecom, aes(x=OnlineSecurity,fill=Churn))+ geom_bar(position = 'fill')
ggplot(telecom, aes(x=OnlineBackup,fill=Churn))+ geom_bar(position = 'fill')
ggplot(telecom, aes(x=DeviceProtection,fill=Churn))+ geom_bar(position = 'fill')
ggplot(telecom, aes(x=TechSupport,fill=Churn))+ geom_bar(position = 'fill')
ggplot(telecom, aes(x=StreamingTV,fill=Churn))+ geom_bar(position = 'fill')
#Churn rate is much higher in case of Fiber Optic InternetServices.
#Customers who do not have services like No OnlineSecurity , OnlineBackup and TechSupport have left the platform in the past month.
ggplot(telecom, aes(x=StreamingMovies,fill=Churn))+ geom_bar()
ggplot(telecom, aes(x=Contract,fill=Churn))+ geom_bar(position = 'fill')
ggplot(telecom, aes(x=PaperlessBilling,fill=Churn))+ geom_bar(position = 'fill')
ggplot(telecom, aes(x=PaymentMethod,fill=Churn))+ geom_bar(position = 'fill')
#A larger percent of Customers with monthly subscription have left when compared to Customers with one or two year contract.
#Churn percent is higher in case of cutsomers having paperless billing option.
#Customers who have ElectronicCheck PaymentMethod tend to leave the platform more when compared to other options.
ggplot(telecom, aes(y= tenure, x = "", fill = Churn)) + 
  geom_boxplot()
# The median tenure for customers who have left is around 10 months.
ggplot(telecom, aes(y= MonthlyCharges, x = "", fill = Churn)) + 
  geom_boxplot()
#MonthlyCharges: Customers who have churned, have high monthly charges. The median is above 75.
ggplot(telecom, aes(y= TotalCharges, x = "", fill = Churn)) + 
  geom_boxplot()
#TotalCharges:The median Total charges of customers who have churned is low
ggplot(telecom, aes(tenure,fill=Churn))+ geom_bar(position = 'fill')

correaltion=telecom[,c('tenure','MonthlyCharges','TotalCharges')]

as.data.frame(cor(correaltion))
# Total charges has positive correlation with Tenure
# Now finding the oultiers
boxplot(telecom$MonthlyCharges)$out

# Calculating the asserted value in getting the information tenured for the justification and maintenance.


