library(dplyr)
library(tidyr)

refine_original_new <- refine_original_2

#1: Clean up brand names
#company_copy <- refine_original_2$company
refine_original_new$company <- gsub(pattern = "Phillips|phillips|phillipS|philips|fillips|phlips|phllips|phillps", replacement = "phillips", refine_original_new$company)
refine_original_new$company <- gsub(pattern = "Akzo|akzo|AKZO|ak zo|akz0", replacement = "akzo", refine_original_new$company)
refine_original_new$company <- gsub(pattern = "Van Houten|van Houten|van houten", replacement = "van houten", refine_original_new$company)
refine_original_new$company <- gsub(pattern = "Unilever|unilver", replacement = "unilever", refine_original_new$company)


# 2: Separate product code and number
refine_original_new <- separate(refine_original_new, `Product code / number`, c("product_code", "product_number"))


# 3: Add product categories
product_cat_2 <- refine_original_new$product_code
product_cat_2 <- gsub(pattern = "p", replacement = "Smartphone", x = product_cat_2)
product_cat_2 <- gsub(pattern = "x", replacement = "Laptop", x = product_cat_2)
product_cat_2 <- gsub(pattern = "v", replacement = "TV", x = product_cat_2)
product_cat_2 <- gsub(pattern = "q", replacement = "Tablet", x = product_cat_2)

refine_original_new <- refine_original_new %>% mutate(product_cat_2)


# 4: Add full address for geocoding
refine_original_new <- refine_original_new %>% unite(col = "full_address", ... = c(address, city, country), sep = ", ") %>% mutate()


# 5a: Create dummy variables for company category
company_phillips <- refine_original_new$company
company_akzo <- refine_original_new$company
company_van_houten <- refine_original_new$company
company_unilever <- refine_original_new$company

company_phillips <- gsub(pattern = "phillips", replacement = 1, x = company_phillips)
company_phillips <- gsub(pattern = "akzo|van houten|unilever", replacement = 0, x = company_phillips)

company_akzo <- gsub(pattern = "akzo", replacement = 1, x = company_akzo)
company_akzo <- gsub(pattern = "phillips|van houten|unilever", replacement = 0, x = company_akzo)

company_van_houten <- gsub(pattern = "van houten", replacement = 1, x = company_van_houten)
company_van_houten <- gsub(pattern = "phillips|akzo|unilever", replacement = 0, x = company_van_houten)

company_unilever <- gsub(pattern = "unilever", replacement = 1, x = company_unilever)
company_unilever <- gsub(pattern = "phillips|akzo|van houten", replacement = 0, x = company_unilever)

refine_original_new <- refine_original_new %>% mutate(company_phillips) %>% mutate(company_akzo) %>% mutate(company_van_houten) %>% mutate(company_unilever)


# 5b: Create dummy variables for product category
product_smartphone <- product_cat_2
product_tv <- product_cat_2
product_laptop <- product_cat_2
product_tablet <- product_cat_2

product_smartphone <- gsub(pattern = "Smartphone", replacement = 1, x = product_smartphone)
product_smartphone <- gsub(pattern = "TV|Laptop|Tablet", replacement = 0, x = product_smartphone)

product_tv <- gsub(pattern = "TV", replacement = 1, x = product_tv)
product_tv <- gsub(pattern = "Smartphone|Laptop|Tablet", replacement = 0, x = product_tv)

product_laptop <- gsub(pattern = "Laptop", replacement = 1, x = product_laptop)
product_laptop <- gsub(pattern = "TV|Smartphone|Tablet", replacement = 0, x = product_laptop)

product_tablet <- gsub(pattern = "Tablet", replacement = 1, x = product_tablet)
product_tablet <- gsub(pattern = "TV|Laptop|Smartphone", replacement = 0, x = product_tablet)

refine_original_new <- refine_original_new %>% mutate(product_smartphone) %>% mutate(product_tv) %>% mutate(product_laptop) %>% mutate(product_tablet)

View(refine_original_new)