library(dplyr)
library(tidyr)

#1: Clean up brand names
gsub(pattern = "Phillips|phillips|phillipS|philips|fillips|phlips|phllips", replacement = "phillips", refine_original_2$company)
gsub(pattern = "Akzo|akzo|AKZO|ak zo", replacement = "akzo", refine_original_2$company)
gsub(pattern = "Van Houten|van Houten|van houten", replacement = "van houten", refine_original_2$company)
gsub(pattern = "Unilever", replacement = "unilever", refine_original_2$company)

# 2: Separate product code and number
separate(refine_original_2, `Product code / number`, c("product_code", "product_number"))

# 3: Add product categories
(a <- 1)
product_cat_2 <- c()
while (a <= length(refine_original_2$`Product code / number`)){
  if (product_code[a] == "p"){
    product_cat_2[a] <- "Smartphone"
  }else if(product_code[a] == "v"){
    product_cat_2[a] <- "TV"
  }else if(product_code[a] == "x"){
    product_cat_2[a] <- "Laptop"
  }else {
    (product_cat_2[a] <- "Tablet")
  }
  a <- a + 1
}
refine_original_2 %>% mutate(product_cat_2)

# 4: Add full address for geocoding
refine_original_2 %>% unite(col = "full_address", ... = c(address, city, country), sep = ", ") %>% mutate()
View(refine_original_2)

# 5: Create dummy variables for company and product category
a <- 1
company_phillips <- c()
company_akzo <- c()
company_van_houten <- c()
company_unilever <- c()
while (a <= length(refine_original_2$company)){
  if(refine_original_2$company[a] == "phillips"){
    company_phillips[a] <- 1
    company_akzo[a] <- 0
    company_van_houten[a] <- 0
    company_unilever[a] <- 0
  }else if(refine_original_2$company[a] == "akzo"){
    company_phillips[a] <- 0
    company_akzo[a] <- 1
    company_van_houten[a] <- 0
    company_unilever[a] <- 0
  }else if(refine_original_2$company == "van_houten"){
    company_phillips[a] <- 0
    company_akzo[a] <- 0
    company_van_houten[a] <- 1
    company_unilever[a] <- 0
  }else {
    company_phillips[a] <- 0
    company_akzo[a] <- 0
    company_van_houten[a] <- 0
    company_unilever[a] <- 1
  }
  a <- a + 1
}

a <- 1
product_smartphone <- c()
product_tv <- c()
product_laptop <- c()
product_tablet <- c()
while (a <= length(product_cat_2)){
  if(product_cat_2[a] == "Smartphone"){
    product_smartphone[a] <- 1
    product_tv[a] <- 0
    product_laptop[a] <- 0
    product_tablet[a] <- 0
  }else if(product_cat_2[a] == "TV"){
    product_smartphone[a] <- 0
    product_tv[a] <- 1
    product_laptop[a] <- 0
    product_tablet[a] <- 0
  }else if(product_cat_2[a] == "Laptop"){
    product_smartphone[a] <- 0
    product_tv[a] <- 0
    product_laptop[a] <- 1
    product_tablet[a] <- 0
  }else {
    product_smartphone[a] <- 0
    product_tv[a] <- 0
    product_laptop[a] <- 0
    product_tablet[a] <- 1
  }
  a <- a + 1
}

refine_original_2 %>% mutate(company_phillips) %>% mutate(company_akzo) %>% mutate(company_van_houten) %>% mutate(company_unilever)
refine_original_2 %>% mutate(product_laptop) %>% mutate(product_tv) %>% mutate(product_tablet) %>% mutate(product_tv)