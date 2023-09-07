library(metrica)
library(mgcv)

data_full <- DF[sample(.N, 1e6)]

vec_id <- data_full$cell %>% unique

vec_train_id <- sample(vec_id, size=floor(length(vec_id)*0.75))
vec_test_id <- vec_id[!vec_id %in% vec_train_id]

length(vec_train_id)
length(vec_test_id)


data_train <- data_full[cell%in%vec_train_id]
data_test <- data_full[cell%in%vec_test_id]


# assuming a model is fit
tmp_pred <- data_test %>% 
  mutate(pred = predict(mod, newdata=., type='response'))


metrica::R2(obs = data_test$pv,
            tmp_pred = data_test$pred)

metrica::RMSE(obs = data_test$pv,
              tmp_pred = data_test$pred)