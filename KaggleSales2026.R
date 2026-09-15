###Load the library
library(dplyr) 
library(readxl)
library(ggplot2)
library(MASS)
library(car)
library(glmnet)
library(DescTools)
library(usethis)
###Read dataset
setwd("C:/Users/hhhhh/OneDrive - Bournemouth University/Kaggle/Sales")
data <- read.csv("sales.csv")

###Variable checking
summary(data)

count(data,branch)
count(data,city)
count(data,customer_type)
count(data,gender)
count(data,product_name)
count(data,product_category)

###Check total sales and total quantity
data_new<-data %>% 
  group_by(product_category)%>%
  mutate(total_sales = sum(total_price)) %>%
  mutate(total_quantity = sum(quantity)) %>%
  ungroup()

###Add average price of product
data_new<-data_new %>%
  dplyr::mutate(
    ave_pri_app = mean(unit_price[product_name == "Apple"]),
    ave_pri_det = mean(unit_price[product_name == "Detergent"]),
    ave_pri_not = mean(unit_price[product_name == "Notebook"]),
    ave_pri_ora = mean(unit_price[product_name == "Orange Juice"]),
    ave_pri_sha = mean(unit_price[product_name == "Shampoo"])
   )

###Classify price as three tiers
data_new <- data_new %>% 
  mutate(
    total_price_level = case_when(
      total_price >= 1.21 & total_price <= 38.38 ~ "low_price",
      total_price > 38.38 & total_price <= 176.07 ~ "middle_price",
      total_price > 176.07 & total_price <= 433.99 ~ "high_price",
      TRUE ~ "other"
    ),
    customer_type = factor(customer_type),
    total_price_level = factor(total_price_level,
                               levels = c("low_price", "middle_price", "high_price"),
                               ordered = TRUE)
  )

###Check total sales of product category
data_new %>% 
  group_by(product_category)%>%
  count(total_sales,total_quantity)

###Data Visualisation
data_new %>%
  filter(product_name == "Apple") %>%
  ggplot(aes(x = "Apple", y = unit_price)) +
  geom_boxplot(fill = "yellow") +
  labs(title = "Distribution of unit price", y = "unit_price($)") +
  theme_minimal()

data_new %>%
  filter(product_name == "Detergent") %>%
  ggplot(aes(x = "Detergent", y = unit_price)) +
  geom_boxplot(fill = "yellow") +
  labs(title = "Distribution of unit price", y = "unit_price($)") +
  theme_minimal()


data_new %>%
  filter(product_name == "Notebook") %>%
  ggplot(aes(x = "Notebook", y = unit_price)) +
  geom_boxplot(fill = "yellow") +
  labs(title = "Distribution of unit price", y = "unit_price($)") +
  theme_minimal()

data_new %>%
  filter(product_name == "Orange Juice") %>%
  ggplot(aes(x = "Orange Juice", y = unit_price)) +
  geom_boxplot(fill = "yellow") +
  labs(title = "Distribution of unit price", y = "unit_price($)") +
  theme_minimal()

data_new %>%
  filter(product_name == "Shampoo") %>%
  ggplot(aes(x = "Shampoo", y = unit_price)) +
  geom_boxplot(fill = "yellow") +
  labs(title = "Distribution of unit price", y = "unit_price($)") +
  theme_minimal()

ggplot(data_new,aes(x = product_category))+
  geom_bar()

ggplot(data_new, aes(x = city, fill = total_price_level))+
  geom_bar(position = "dodge", color = "black", alpha = 0.8) +
  labs(
    title = "Total price level in each city",
    x = "city",
    y = "count",
    fill = "total_price_level"
  ) +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal()

ggplot(data_new, aes( x = city, fill = customer_type))+
  geom_bar(position = "dodge", color = "black", alpha = 0.8) +
  labs(
    title = "Customer_type in each city",
    x = "city",
    y = "count",
    fill = "customer_type"
  ) +
  scale_fill_brewer(palette = "Set5") +
  theme_minimal()

###Boxplot for unit price
ggplot(data_new, aes(y = unit_price)) +
  geom_boxplot(fill = "yellow") +
  labs(title = "Distribution of unit price", y = "unit_price($)") +
  theme_minimal()

###Boxplot for quantity
ggplot(data_new, aes(y = quantity)) +
  geom_boxplot(fill = "yellow") +
  labs(title = "Distribution of quantity", y = "quantity") +
  theme_minimal()

###Boxplot for tax
ggplot(data_new, aes(y = tax)) +
  geom_boxplot(fill = "yellow") +
  labs(title = "Distribution of tax", y = "tax($)") +
  theme_minimal()

###Boxplot for total price
ggplot(data_new, aes(y = total_price)) +
  geom_boxplot(fill = "yellow") +
  labs(title = "Distribution of Total Order Value", y = "Total Price($)") +
  theme_minimal()

###Boxplot for reward_points
ggplot(data_new, aes(y = reward_points)) +
  geom_boxplot(fill = "yellow") +
  labs(title = "Distribution of reward points", y = "reward_points") +
  theme_minimal()

###Density of total_price
ggplot(data, aes(total_price)) +
  geom_density(fill = "yellow", alpha = 0.7) +
  labs(title = "Density Distribution of total_price",
       x = "total price", y = "Density") +
  theme_bw()

###Lasso regression
x <- model.matrix(total_price_level ~ city + customer_type + gender + product_category,
                  data=data_new)[,-1]
y <- factor(data_new$total_price_level,ordered = FALSE)


set.seed(123)
cv_lasso_multi <- cv.glmnet(x,y,alpha=1,family = "multinomial")
plot(cv_lasso_multi)

lasso <- glmnet(x,y,alpha=1,family="multinomial",lambda = cv_lasso_multi$lambda.min)
coef(lasso)

###Ordinal Logistic Regression
reg1 <- polr(total_price_level ~ customer_type,
             data = data_new,
             Hess = TRUE)
summary(reg1)
car::Anova(reg1)
PseudoR2(reg1, which = "McFadden") 

reg2 <- polr(total_price_level ~ city + customer_type + gender + product_name + product_category,
             data = data_new,
             Hess = TRUE)
summary(reg2)
car::Anova(reg2)
PseudoR2(reg2, which = "McFadden") 


reg3 <- polr(total_price_level ~ customer_type * product_category + city + gender + product_name,
             data = data_new,
             Hess = TRUE)
summary(reg3)
car::Anova(reg3)
PseudoR2(reg3, which = "McFadden") 


