

# Stepwise Regression 
# Import data


library(readxl)
Data_SR <- read_excel("Documents/TAMU/Spring'24/ISEN-616/Project Report /Data_Step_Regression.xlsx")
head(Data_SR)
dim(Data_SR) # There are 27 rows and 59 columns in the data 
columns=colnames(Data_SR)
print(columns)

# Drop the first 6 columns - Run, A,B,C,D,E
Reg_data= Data_SR[,-c(1:6)]
head(Reg_data)
dim(Reg_data)


install.packages("olsrr")
library(olsrr)

model1 <- lm(Y.Avg ~ . - `S^2` - `ln_S^2` , data = Reg_data) # Kept all the variables including the aliased varibles D and E 
ols_step_both_p(model1)
model1_results <- ols_step_both_p(model1, pent=0.015, prem=0.02)
print(model1_results)

# Checking the multicollinearity in the data 
cor_matrix <- cor(Reg_data)
print(cor_matrix)



# Dropping the effects once the adjusted R^2 has reached 91.25% 

columns1=colnames(Reg_data)
print(columns1)
model2 <- lm(Y.Avg~. - `S^2` - `ln_S^2` -BC_ll    -CE_ql    -AD_qq    -C_q  -D_l  -BE_ql  -B_q  -E_l  -CD_ql  -BC_qq  -BC_ql  -B_l -BE_ll, data= Data_SR)
ols_step_both_p(model2, pent=0.010, prem=0.01)




columns1=colnames(Reg_data)
print(columns1)

colnames(Reg_data)[colnames(Reg_data) == "ln_S^2"] <- "Dipersion.effect"
new_columns=colnames(Reg_data)
print(new_columns)

# Running stepwise Regression on the dispersion effects 
Model1_dispersion <- lm(Dispersion.effect~ . - Y.Avg - `S^2`, data = Reg_data)
ols_step_both_p(Model1_dispersion)
