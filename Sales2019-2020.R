library(tidyverse)
library(plotly)
library(DT)
library(scales)
Sales_Export_2019_2020 <- read_csv("C:/Users/Matthew Afoakwah/Downloads/archive (3)/Sales-Export_2019-2020.csv")

#Number of rows and column in dataset.
dim(Sales_Export_2019_2020)
#Column Names.
names(Sales_Export_2019_2020)

#Number of NA value in Database.
sum(is.na(Sales_Export_2019_2020))

#Determining different attributes in each char attribute.
unique(Sales_Export_2019_2020$country)

unique(Sales_Export_2019_2020$category)

unique(Sales_Export_2019_2020$customer_name)

unique(Sales_Export_2019_2020$sales_manager)

unique(Sales_Export_2019_2020$sales_rep)

unique(Sales_Export_2019_2020$device_type)    

#Removing duplicates
distinct(Sales_Export_2019_2020)

#identifying number of rows
nrow(Sales_Export_2019_2020)

#Identifying outliers. 
boxplot(Sales_Export_2019_2020$cost)
boxplot(Sales_Export_2019_2020$order_value_EUR)

#Previewing dataset.
glimpse(Sales_Export_2019_2020)



#####Data Wrangling

#Total cost of for each country based on category, and ratio of sales per country.
CountryTotalcostExport <- Sales_Export_2019_2020 %>%
  group_by(country)%>%
  mutate(costTotal = sum(cost)) %>%
  group_by(country, category) %>%
  summarise(costCategoryTotal = sum(cost), costCategoryPercentage = (costCategoryTotal / costTotal)*100)
  
#Total sale of each country by category
 CountryTotalSaleExport <- Sales_Export_2019_2020 %>%
  group_by(country)%>%
  mutate(saleTotal = sum(order_value_EUR)) %>%
  group_by(country, category) %>%
  summarise(saleCategoryTotal = sum(order_value_EUR), saleCategoryPercentage = (saleCategoryTotal / saleTotal)*100) 
  

#Removing Duplicate rows.
CountryTotalcostExport <- distinct(CountryTotalcostExport)
CountryTotalSaleExport <- distinct(CountryTotalSaleExport)

#Creating interactive datatable.
CountryTotalSaleExport %>%
  datatable()

#Total cost of each category by country
t <- ggplot(data = CountryTotalcostExport, aes(x = country, y = costCategoryTotal, colour = category)) + geom_point() + geom_line(aes(group = category)) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + labs(title = "Total Cost of Each Countries Exports by Category (2019-2020))", x = "Country", y = "Cost Total")  

ggplotly(t) 

#Total Sales of each category by country 
ggplot(data = CountryTotalSaleExport, aes(x = country, y = saleCategoryTotal, fill = category)) +
  geom_col() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + labs(title = "Total Sales of Each Countries Exports by Category (2019-2020))", x = "Country", y = "Cost Total")  


#Total profit by country 
CountryProfit <- Sales_Export_2019_2020 %>%
  group_by(country) %>%
  mutate(CostTotal = sum(cost), SaleTotal = sum(order_value_EUR)) %>%
  summarise(GrossProfit = SaleTotal - CostTotal)

#Remove duplicate rows
CountryProfit <- distinct(CountryProfit)

#Gross Profit by country
ggplot(data = CountryProfit, aes(x = reorder(country, GrossProfit), y = GrossProfit)) + geom_point() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + labs(title = "Cross Profit Per Country (2019-2020))", x = "Country", y = "Total Gross Profit")

#Total Gross Profit by Category
CategoryProfit <- Sales_Export_2019_2020 %>%
  group_by(category) %>%
  mutate(CostTotal = sum(cost), SaleTotal = sum(order_value_EUR)) %>%
  summarise(GrossProfit = SaleTotal - CostTotal)

#Removing duplicate rows.
CategoryProfit <- distinct(CategoryProfit)

CProfit <- ggplot(data = CategoryProfit, aes(x = category, y = GrossProfit, fill = category, )) + geom_tile(aes()) +  scale_y_continuous(labels = scales::comma) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
ggplotly(CProfit)


#Total profit by country and category 
CoProfit <- Sales_Export_2019_2020 %>%
  group_by(country) %>%
  mutate(CostTotal = sum(cost), SaleTotal = sum(order_value_EUR), GrossProfit = SaleTotal - CostTotal) %>%
  group_by(country, category) %>%
  summarise(ctotal = sum(cost), stotal = sum(order_value_EUR), GProfit = stotal - ctotal, GrossProfit, ProfitPercentage = GProfit/ GrossProfit)


CoProfit <- distinct(CoProfit)
CoProfit$ProfitPercentage <- label_percent(big.mark = ",", suffix = "%")(CoProfit$GProfit / CoProfit$GrossProfit)

CoProfit %>%
  datatable()
pie_label <- label_percent(big.mark = ",", suffix = "%")(CoProfit$GProfit / CoProfit$GrossProfit)

````````````````
piechar <- ggplot(data = CoProfit, aes(x = , y = ProfitPercentage, fill = category)) + geom_col(position = "fill") +
 +  coord_polar(theta = "y") + theme_void() +labs(title = "Ratio of Profit per")


print(piechar)

spear <-CoProfit %>%
  filter(country == country)

# Loop through each unique country
for (country in unique(CoProfit$country)) {
  country_data <- CoProfit %>%
    filter('country' == 'country') 
  
  pie_label2 <-paste0(round(100 * country_data$GProfit/country_data$GrossProfit, 2), "%")
  
   
  pie_chart <- ggplot(data = country_data, aes(x = "", y = GProfit, fill = category)) +
    geom_bar(stat = "identity") +
    coord_polar(theta = "y") +
    theme_void() +  geom_text(aes(label = pie_label2), position = position_stack(vjust = 0.5)) +
    labs(title = paste("Pie Chart -", country))
  
  print(pie_chart)
  pie_label2 <- is.null(pie_label2)
}


````





