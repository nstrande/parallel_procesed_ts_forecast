library(tidyverse)
library(prophet)
library(lubridate)
library(padr)


###################
### Import data ###
###################

df <- sweep::bike_sales



#################
### Transform ###
#################

df$ds <- floor_date(df$order.date, "month")

df_monthly <- df %>%
                  group_by(ds, category.secondary) %>%
                  summarise(y = sum(price)) %>% 
                  ungroup() # Change to monthly sales figures

df_padded <- df_monthly %>%
                  pad(group = "category.secondary", interval = 'month') # Padding on month

df_nested <- df_padded %>%
                  group_by(category.secondary) %>%
                  nest() %>% 
                  ungroup()


########################
### Train Test Split ###
########################

# Declare cutoff
train_test_split_ratio = 0.7

df_nested <- df_nested %>%
                mutate(df_length = map(data, ~length(.x$ds))) %>% 
                unnest(df_length) %>% 
                mutate(train_test_split_idx = as.integer(df_length*train_test_split_ratio))

# Train test split
df_nested <- df_nested %>%
                mutate(train_df = map2(.x=data,
                                       .y=train_test_split_idx,
                                       ~slice(.x, 1:.y))) %>% 
                mutate(test_df = map2(.x=data,
                                       .y=train_test_split_idx,
                                       ~slice(.x, .y+1:n()))) %>% 
                mutate(h = df_length-train_test_split_idx)



#######################
### Fit and predict ###
#######################

df_nested <- df_nested %>%
                mutate(model = map(train_df, ~prophet(.x)))

df_nested <- df_nested %>% 
                mutate(future = map2(.x=model,
                                      .y=h,
                                      ~make_future_dataframe(.x,
                                                             periods = .y,
                                                             freq = "month",
                                                             include_history = FALSE))) %>% 
                mutate(forecast = map2(.x=model,
                                       .y=future,
                                       ~predict(.x,
                                                .y)))



################
### Plotting ###
################

# Prepare final dataframe
df_actual <- df_nested %>% 
                  unnest(data) %>% 
                  select(c(category.secondary, ds, y)) %>% 
                  rename("date" = ds,
                         "actual" = y)

df_forecast <- df_nested %>% 
                  unnest(forecast) %>% 
                  select(c(category.secondary,
                           ds,
                           yhat,
                           yhat_lower,
                           yhat_upper)) %>% 
                  rename("date" = ds,
                         "predict" = yhat,
                         "predict_lower" = yhat_lower,
                         "predict_upper" = yhat_upper)

df_final <- df_actual %>% 
                   left_join(df_forecast, by = c("category.secondary", "date"))

# Plotting
plot_actual_vs_predict <- df_final %>%
                          ggplot(aes(x=date, y=actual), group='Actual', col='black') +
                                                geom_line() +
                                                facet_wrap(.~category.secondary, scales = "free") +
                                                geom_line(aes(x=date, y=predict), col='red', linetype = "dashed") +
                                                geom_ribbon(aes(ymin = predict_lower, ymax = predict_upper), alpha = 0.1) +
                                                scale_y_continuous(name = "Revenue (1.000)", labels = function(y) y / 1000) +
                                                labs(title="Revenue sold by category",
                                                     subtitle = "US Dollars",
                                                     x = "Date") +
                                                theme(legend.position="top")