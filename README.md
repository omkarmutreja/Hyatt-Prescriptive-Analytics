# Hyatt-Prescriptive-Analytics
This is a data science project done in R to analyze the Hyatt Hotel Dataset in different countries. The main aim of the project is to give recommendation to the Hotel on different measures to increase the Net Promoter Score.
I have created an interactive display of my results using R-shiny application which is dynamic and super interactive.

## Project Objective:
The objective of the data analysis project is to analyze data collected through the customer feedback of the Hyatt hotels throughout the world. The focus is to identify patterns and provide substantial recommendations to any hotel or hotels to improve the facilities and amenities offered. This will in turn help increase the hotels profit and customer satisfaction.

## Scope:
The scope of the project is limited to a few hotels in the United States. The survey data for the months of November 2014, December 2014 and January 2015 have been analyzed, pre-processed and analyzed again.

## Deliverables:
1) Identification of patterns based on the performance of the hotels in US
2) Visualization of different hotel chains based on performance and feedback received on basis of the services offered
3) Recommendation of the services offered to improve the business performance and revenue

## Industry Analysis:
In the past decade, the industry has thrived, as both leisure and business travel have ascended alongside GDP. The hospitality industry's backbone is comprised of customer service, a concept shared by all segments of the industry. This business may focus on one or all facets of hospitality. How accomplished you and your staff are at serving others will determine your business' level of success. However, though costs and challenges will increase, owning or managing several facets of hospitality can provide you with many more opportunities to generate success. The U.S. travel and tourism industry generated    over $1.5 trillion in economic output in 2016. In 2016 travelers spent more than $293 billion on traveller accommodations. This sector supports more than 1.4 million US jobs.

The Hotel Industry Customer Experience Benchmarks have been identified which are as follows: 
•	Ease of reservations
•	Check-In process
•	Room cleanliness and comfort
•	Food services
•	In-Room entertainment (TV, movies)
•	Amenities
•	In-Room internet service
•	Staff courtesy and helpfulness
•	Loyalty programs
•	Call center
•	Website


## NPS Trends:
Net Promoter Score, or NPS, measures customer experience and predicts business growth. This proven metric transformed the business world and now provides the core measurement for customer experience management programs the world round.
NPS is calculated using the answer to a key question, using a 0-10 scale: How likely is it that you would recommend [brand] to a friend or colleague? Subtracting the percentage of Detractors (score 0-6) from the percentage of Promoters (score 9-10) yields the Net Promoter Score, which can range from a low of -100 (if every customer is a Detractor) to a high of 100 (if every customer is a Promoter). Promoters (score 9-10) are loyal enthusiasts who will keep buying and refer others, fueling growth. Passives (score 7-8) are satisfied but unenthusiastic customers who are vulnerable to competitive offerings. Detractors (score 0-6) are unhappy customers who can damage your brand and impede growth through negative word-of-mouth.

## Business Questions:
The focus is to improve the services provided by the Hyatt hotels to facilitate the customer recommendations and reviews. Customer recommendations and reviews are important factors for any hotel industry. We have performed descriptive analysis and data mining to analyze the survey data set to answer the following questions:

•	How should HYATT hotels reduce the friction in the customer experience?
•	Which country has the highest detractor ratio? 
•	Which state within that country is showcasing poor customer feedback?
•	Which category of hotels is leading the poor customer feedback trend within the specific state?
•	What facilities and services have a positive and negative impact on the NPS count of the hotel?
•	Which services should be given priority for improvement?
•	Are there any trends to categorize hotels based on customer feedback?
•	What services and facilities are driving transformation of customer experience from detractors, passive to promotors?

## Data Identification:
The survey data of November 2014, December 2014 and January 2015 have 1151316 customer data with around 78 relevant attributes of the hotel. 

## Data Preprocessing:
This survey data has null values which will possibly create a hindrance to obtain accurate trends. Omitting the NA values is not advisable as we may lose out on important trends. Initially, all the null values are replaced with blank values and then replaced with the string ‘NA’. But the best idea is to remove the NA values ‘Likelihood to Recommend’ and ‘NPS_Type’ as they are the key factors for this recommendation analysis. By doing this we drill down to 185091 customer data instances. 

## Descriptive Statistics:
Descriptive statistics are used to describe the basic features of the data in a study. They provide simple summaries about the sample and the measures. Together with simple graphics analysis, they form the basis of virtually every quantitative analysis of data. 

## Recommendation Analysis:
### Modeling Techniques:
Instead of query based approach, we focused on model based approach and we started with linear regression model. We performed linear modeling with dependent variables Likelihood_Recommend_H and secondly NPS_Type.
Linear Modeling to understand and predict the behavior of customers based ‘Likelihood to Recommend’. Initially we chose a few relevant variables according to the descriptive analysis and kept performing linear modeling until we obtained statistically relevant independent variables.

Apriori Algorithm is a data mining algorithm for mining frequent combination of services and facilities which specifically caters to customer preferences.

SVM: Machine learning algorithms like Support Vector Machines (SVM) are used to implement supervised learning of data for classification and prediction. SVM facilities the identification of a combination of services and facilities which will create a positive, neutral, and negative customer experience.

## Interactive Data Visualization:
The graphs and charts created in R is not interactive hence we implemented Shiny apps for the same. 

## Conclusion:
1) To improve customer feedback from detractors, the hotel must focus on improving the services and facilities which have been proven to significantly influence customer feedback
2) Services must be packaged and promoted in a combination of Guest Rooms, Tranquility, Hotel Condition, Customer Satisfaction, Staff cared, and Internet Usage tailored to specific class and category of customers
3) Hyatt Hotel in El Segundo City of California should make significant improvements by providing better staff services to customers
4) The Hotel should focus on maintaining a high standard for Hotel Rooms and Hotel Conditions
5) Room upgrades, premium internet services, and personal staff could be provided to frequent customers with detractor and passive ratings
6) Restaurants and cafes significantly influence customer feedback towards the hotel; Survey forms and feedback questionnaire could be filled up to customize food and bar menu 
7) Special rules must be set-up to ensure noise free environment and tranquility of customers 


