

# Load Packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(timetk)) install.packages("timetk", repos = "http://cran.us.r-project.org")
if(!require(reshape2)) install.packages("reshape2", repos = "https://cran.us.r-project.org")
if(!require(Boruta)) install.packages("Boruta", repos = "https://cran.us.r-project.org")
if(!require(imputeTS)) install.packages("imputeTS", repos = "https://cran.us.r-project.org")
if(!require(modeltime)) install.packages("modeltime", repos = "https://cran.us.r-project.org")
if(!require(parsnip)) install.packages("parsnip", repos = "https://cran.us.r-project.org")
if(!require(recipes)) install.packages("recipes", repos = "https://cran.us.r-project.org")
if(!require(workflows)) install.packages("workflows", repos = "https://cran.us.r-project.org")

# Load in the raw Walmart data
train_df <- read_csv("data/train.csv")
stores_df <- read_csv("data/stores.csv")
features_df <- read_csv("data/features.csv")

# Visualize train_df and make mutations
head(train_df)
train_df <- train_df %>%
  mutate(
    Store = as_factor(Store),
    Dept = as_factor(Dept)
  )

# Visualize stores_df and make mutations
head(stores_df)
stores_df <- stores_df %>%
  mutate(
    Store = as_factor(Store),
    Type = as_factor(Type)
  )

# Visualize features_df, make mutations and fill NA with 0
head(features_df)
features_df <- features_df %>%
  mutate(
    Store = as_factor(Store)
  )
features_df[is.na(features_df)] <- 0

# Join datasets into master_df
master_df <- train_df %>% left_join(stores_df, by = "Store")
master_df <- master_df %>% 
  select(-IsHoliday) %>% 
  left_join(features_df, by = c("Store", "Date"))

# Create Unique Identifier across Store, Dept and Date
master_df <- master_df %>% unite(ID, Dept, Date, sep = "_", remove = FALSE) %>%
  unite(ID, Store, ID, sep = "_", remove = FALSE)

# Create Unique Identifier across Store and Dept
master_df <- master_df  %>%
  unite(StoreDept, Store, Dept, sep = "_", remove = FALSE)
head(master_df)

######################################
# Split Data into Exploratory and Test Set
######################################

# Keep dates which are less than or equal to 2012-02-10
exp_df <- master_df %>%
  filter(Date <= c("2012-02-10"))
# Keep dates which are greater than 2012-02-10
test_df <- master_df %>%
  filter(Date > c("2012-02-10"))
# Select desired features and remove redundant dataframes
test_df <- test_df %>% select(ID, StoreDept, Weekly_Sales, IsHoliday, Size)
rm(features_df, stores_df, train_df)

###################
# Decomposition
###################

# Visualise Store sales by store type
exp_df %>%
  group_by(Type, Date) %>%
  summarise(Weekly_Sales = mean(Weekly_Sales)) %>%
  ggplot(aes(Date, Weekly_Sales, color = Type)) +
  geom_line() +
  ggtitle("Weekly Sales by Store Type")

# Average Weekly Sales by Store Type
exp_sales <- exp_df %>%
  group_by(Type, Date) %>% # Group by Type and Date
  summarise(Weekly_Sales = mean(Weekly_Sales)) %>% # Mean of Weekly Sales
  ungroup() %>%
  spread(key = Type, value = Weekly_Sales) # Convert to wide data for lapply
# Convert exp_sales dataframe to timeseries with weekly frequency
weekly_ts <- lapply(exp_sales, function(t) ts(t, start = c(exp_sales[1, 1]), frequency = 365.25/7))
# Apply STL Function
stl_weekly_ts <- lapply(weekly_ts, function(t) stl(t, "periodic"))

# Observe Seasonality through decomposition
grid.arrange(
  autoplot(stl_weekly_ts$A$time.series[,1]) + labs(title = "Seasonality Component", subtitle = "Store Type: A", x = "", y = ""),
  autoplot(stl_weekly_ts$B$time.series[,1]) + labs(subtitle = "Store Type: B", x = "", y = ""),
  autoplot(stl_weekly_ts$C$time.series[,1]) + labs(subtitle = "Store Type: C", y = "")
)

# Observe Trend through decomposition
grid.arrange(
  autoplot(stl_weekly_ts$A$time.series[,2]) + labs(title = "Trend Component", subtitle = "Store Type: A", x = "", y = ""),
  autoplot(stl_weekly_ts$B$time.series[,2]) + labs(subtitle = "Store Type: B", x = "", y = ""),
  autoplot(stl_weekly_ts$C$time.series[,2]) + labs(subtitle = "Store Type: C", y = "")
)

# Observe Remainders through decomposition
grid.arrange(
  autoplot(stl_weekly_ts$A$time.series[,3]) + labs(title = "Remainder Component", subtitle = "Store Type: A", x = "", y = ""),
  autoplot(stl_weekly_ts$B$time.series[,3]) + labs(subtitle = "Store Type: B", x = "", y = ""),
  autoplot(stl_weekly_ts$C$time.series[,3]) + labs(subtitle = "Store Type: C", y = "")
)
rm(stl_weekly_ts, weekly_ts, exp_sales)

###################
# Feature Selection
###################

# Create Coorelation Heatmap
Cor_Df <- exp_df %>% select(-Date, -Type, -ID, -StoreDept) %>% mutate(
  Store = as.numeric(Store),
  Dept = as.numeric(Store),
  IsHoliday = as.numeric(IsHoliday)
) %>%
  cor() %>%
  round(2)
Cor_Df %>%
  melt() %>%
  ggplot(aes(Var1, Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(Var1, Var2, label = value)) +
  labs(x = "", y = "", fill = "Pearson's \nCorrelation", title = "Correlation Heat Map") +
  scale_fill_gradient2(mid="#FBFEF9",low="#0C6291",high="#A63446", limits=c(-1,1)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# View Features in table and remove redundant dataframes
Top_Features <- Cor_Df["Weekly_Sales",] %>% as.matrix() %>% abs()
Top_Features[order(Top_Features[,1], decreasing = T),]
rm(Top_Features, Cor_Df)

# Run Boruta Algorithm and plot results
set.seed(1, sample.kind = "Rounding")
boruta <- Boruta(Weekly_Sales ~ ., data = exp_df, doTrace = 2)
plot(boruta, xlab = "", las = 2, cex.axis = 0.7)

# Print Boruta Stats and remove redundant dataframes
boruta_stats <- as.vector(attStats(boruta))
boruta_stats[order(boruta_stats$medianImp, decreasing = TRUE),]
rm(boruta_stats)

###################
# Interpolation
###################

exp_df %>% 
  group_by(StoreDept) %>%
  summarise(count = n()) %>%
  ggplot(aes(count)) +
  geom_density() + 
  theme_classic() +
  ggtitle("Walmart Store_Dept Observations")

## Convert Data to wide format, spreading by StoreDept
wide_exp_df <- exp_df %>%
  group_by(StoreDept, Date) %>%
  summarise(Weekly_Sales = sum(Weekly_Sales))%>%
  ungroup() %>%
  spread(key = StoreDept, value = Weekly_Sales)
sum(is.na(wide_exp_df))

## Fill values with Seasonally Decomposed Interpolation
wide_exp_df <- na_seadec(wide_exp_df, algorithm = "interpolation")
sum(is.na(wide_exp_df))

## Fill values with Linear Interpolation
wide_exp_df <- na_interpolation(wide_exp_df, option = "linear")
sum(is.na(wide_exp_df))

## Fill remaining values with Last Observation Carried Forward
wide_exp_df <- na_locf(wide_exp_df)
sum(is.na(wide_exp_df))

## Convert Back to Tidy Format
tidy_df <- wide_exp_df %>%
  pivot_longer(!Date, names_to = "StoreDept", values_to = "Weekly_Sales") %>%
  arrange(StoreDept)
remove(wide_exp_df)

## Add Features Back tidy_df
tidy_df <- tidy_df %>% 
  separate(StoreDept, c("Store", "Dept"), "_", remove = FALSE) %>% 
  mutate(Store = as.numeric(Store),
         Dept = as.numeric(Dept),
         StoreDept = as_factor(StoreDept))

stores_df <- read_csv("data/stores.csv")
features_df <- read_csv("data/features.csv")

tidy_df <- left_join(tidy_df, stores_df, by = "Store")
tidy_df <- left_join(tidy_df, features_df, by = c("Store", "Date"))
tidy_df <- tidy_df %>% 
  select(Date, StoreDept, Store, Dept, Weekly_Sales, Size)

remove(stores_df, features_df)

###########################
# Create Loss Function WMAE
###########################

WMAE <- function(pred, actual) {
  weights <- if_else(actual$IsHoliday, 5,1)
  (sum(weights * abs(pred$Weekly_Sales - actual$Weekly_Sales)) / sum(weights))
}

############
# Modelling
############

# Create Multiple Linear Regression Model and View Results

W_LM <- lm(Weekly_Sales ~ Size, data = tidy_df)
W_LM_Model <- predict(W_LM, test_df[,c("Size")])
W_LM_Yhat <- data.frame(ID = test_df$ID,
                        Weekly_Sales = W_LM_Model)

results <- data_frame(
  Model = "Multiple Linear Regression", 
  WMAE = WMAE(W_LM_Yhat, test_df)
)

results %>% knitr::kable()

remove(W_LM, W_LM_Model, W_LM_Yhat)

################################################
## Data Preparation for Random Forest and XGBoost
################################################

nested_table <- tidy_df %>% 
  extend_timeseries(
    .id_var = StoreDept,
    .date_var =  Date,
    .length_future = 37 # Extend forecast by 37 weeks
  ) %>%
  nest_timeseries( # create a nested tibble for each StoreDept with past and future
    .id_var = StoreDept,
    .length_future = 37,
    .length_actual = 106
  ) %>%
  split_nested_timeseries( # Creates training and testing indicies
    .length_test = 53
  )

# Recipe for future prediction
model_recipe <- recipe(Weekly_Sales ~ Date + Size, data = extract_nested_train_split(nested_table)) %>%
  step_timeseries_signature(Date) %>% # Feature Engineering
  step_rm(matches("(hour)|(minute)|(second)|(am.pm)|(xts)|(iso)|(lbl)"), Date) %>%
  step_zv(all_predictors()) %>%
  step_dummy(all_nominal())

## Visualize Recipe
model_recipe %>% prep() %>% juice() 

## Random Forest Regressor
rfr <- workflow() %>%
  add_model(rand_forest("regression") %>% set_engine("ranger")) %>%
  add_recipe(model_recipe)

## Gradient Boosting Regressor
xgb <- workflow() %>%
  add_model(boost_tree("regression") %>% set_engine("xgboost")) %>%
  add_recipe(model_recipe )

## Fit model on training set

set.seed(1)
nested_models <- modeltime_nested_fit(
  nested_data = nested_table,
  # Add Models
  rfr,
  xgb)

# Error Logs
nested_models %>% extract_nested_error_report()

## Visualize model fitted to training set

nested_models %>% 
  extract_nested_test_forecast() %>%
  group_by(StoreDept) %>%
  filter(StoreDept %in% c("1_1", "1_2", "1_3", "1_4")) %>%
  plot_modeltime_forecast(
    .facet_ncol  = 2,
    .interactive = FALSE
  )

## Refit model for future forecast

set.seed(1)
refit_models <- nested_models %>%
  modeltime_nested_refit(
    control = control_nested_refit()
  )

## Visualize future forecast

refit_models %>% 
  extract_nested_future_forecast() %>%
  group_by(StoreDept) %>%
  filter(StoreDept %in% c("1_1", "1_2", "1_3", "1_4")) %>%
  plot_modeltime_forecast(
    .interactive = FALSE,
    .facet_ncol = 2
  )

## Convert forecast to dataframe

filtered_refit_models <- as.data.frame(refit_models %>% 
                                         extract_nested_future_forecast()) %>% filter(.model_desc != "ACTUAL") %>%
  select(StoreDept, .model_desc, .index, .value) %>% 
  spread(key = .model_desc, value = .value) %>%
  unite(ID, StoreDept, .index, sep = "_", remove = FALSE) %>%
  select(ID, RANGER, XGBOOST)

## Random forest results to WMAE

F_RandomForest<- filtered_refit_models %>% 
  select(ID, RANGER)

F_RandomForest <- inner_join(F_RandomForest, test_df, by = "ID") %>%
  select(ID, RANGER)

test_df_RF <- inner_join(F_RandomForest, test_df, by = "ID") %>%
  select(ID, Weekly_Sales, IsHoliday)

colnames(F_RandomForest)[2] <- "Weekly_Sales"

results <- bind_rows(results,
                     data_frame(
                       Model = "Random Forest",
                       WMAE = WMAE(F_RandomForest, test_df_RF)
                     ))

results %>% knitr::kable()

## Gradient Boosting results to WMAE

F_XGBOOST<- filtered_refit_models %>% 
  select(ID, XGBOOST)

F_XGBOOST <- inner_join(F_XGBOOST, test_df, by = "ID") %>%
  select(ID, XGBOOST)

test_df_XG <- inner_join(F_XGBOOST, test_df, by = "ID") %>%
  select(ID, Weekly_Sales, IsHoliday)

colnames(F_XGBOOST)[2] <- "Weekly_Sales"

results <- bind_rows(results,
                     data_frame(
                       Model = "Gradient Boosting",
                       WMAE = WMAE(F_XGBOOST, test_df_RF)
                     ))

results %>% knitr::kable()