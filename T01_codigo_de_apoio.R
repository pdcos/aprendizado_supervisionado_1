### ============ Trabalho 01 ============ ###

# Este eh o codigo a partir do qual voc?s devem desevolver o Trabalho 01


# A funcao abaixo auxilia na escrita dos modelos polinomiais. 
# Parametros:
#  "real_feature_names": conjunto de features continuas que sera considerado na
#                        criacao do modelo polinomial.
#
#  "categorical_feature_names": conjunto de features categoricas que sera 
#                               considerado na  criacao do modelo polinomial. Se
#                                voces desejarem um modelo sem variaveis categoricas
#                               basta nao passar nenhum valor para este parametro
#                               na chamada da funcao
#                       
# "degree": numero inteiro que indica ate qual grau polinomial as features continuas
#           em "real_feature_names" serao elevadas. 
#
# A funcao retorna a hipotese ja definida para realizar o treinamento do modelo. 
# Uma ilustracao de uma funcao similar aparece no Ex02.R na linha 490

getHypothesis <- function(real_feature_names, categorical_feature_names=F, degree=3){
    
    hypothesis_string <- "hypothesis <- formula(target ~ "
    for(d in 1:degree){
        for(i in 1:length(real_feature_names)){
            hypothesis_string <- paste(hypothesis_string, 
                                       "I(", real_feature_names[i], "^", d, ") + ",
                                       sep = "")
        }
    }
    
    if(typeof(categorical_feature_names) != "logical"){
        for(i in 1:length(categorical_feature_names)){
            hypothesis_string <- paste(hypothesis_string, 
                                       categorical_feature_names[i], " + ",
                                       sep = "")
        } 
    }
    
    
    hypothesis_string <- substr(hypothesis_string, 1, nchar(hypothesis_string)-3)
    hypothesis_string <- paste(hypothesis_string, ")")
    hypothesis <- eval(parse(text=hypothesis_string))
    return(hypothesis)
}


###======= Desenvolvam o trabalho a partir daqui =======###
library(corrplot)
library(glmnet)
library(caret)


train_set <- read.csv(file = 'T01_train_set.csv', header=TRUE, stringsAsFactors = TRUE)
val_set <- read.csv(file = 'T01_val_set.csv', header=TRUE, stringsAsFactors = TRUE)

#x_train <- train_set[1:18];y_train <- train_set[19];
#x_val <- val_set[1:18]; y_val <- val_set[19];


print("Treino: Número de exemplos e atributos")
dim(train_set)
print("Validação: Número de exemplos e atributos")
dim(val_set)


train_mean_features <- apply(train_set[c(1:17)],2, mean)
val_mean_features <- apply(val_set[c(1:17)],2, mean)
train_max_features <- apply(train_set[c(1:17)],2, max)
val_max_features <- apply(val_set[c(1:17)],2, max)
train_min_features <- apply(train_set[c(1:17)],2, min)
val_min_features <- apply(val_set[c(1:17)],2, min)
train_sd_features <- apply(train_set[c(1:17)],2, sd)
val_sd_features <- apply(val_set[c(1:17)],2, sd)

#Plotando correlacao
correlacao <- cor(train_set[,1:17])
correlacao
corrplot(correlacao, method = "color", type = "upper")



### Z-Norm ###
train_set[,1:17] <- sweep(train_set[,1:17], 2, train_mean_features, "-")
train_set[,1:17] <- sweep(train_set[,1:17], 2, train_sd_features, "/")
summary(train_set)

val_set[,1:17] <- sweep(val_set[,1:17], 2, train_mean_features, "-")
val_set[,1:17] <- sweep(val_set[,1:17], 2, train_sd_features, "/")
summary(val_set)

### MinMax normalization ###
#diff_test <- train_max_features - train_min_features
#train_set[,1:17] <- sweep(train_set[,1:17], 2, train_min_features, "-")
#train_set[,1:17] <- sweep(train_set[,1:17], 2, diff_test, "/")
#summary(train_set)

#diff_val <- val_max_features - val_min_features
#val_set[,1:17] <- sweep(val_set[,1:17], 2, val_min_features, "-")
#val_set[,1:17] <- sweep(val_set[,1:17], 2, diff_val, "/")
#summary(val_set)


feature_names <- c("n_tokens_title", "average_token_length", "num_keywords", "kw_avg_max", "global_subjectivity", "global_sentiment_polarity",
                   "global_rate_positive_words", "global_rate_positive_words", "rate_positive_words", "rate_negative_words", "avg_positive_polarity", 
                   "avg_negative_polarity", "log_n_tokens_content", "log_num_hrefs", "root2_num_self_hrefs", "log_self_reference_max_shares", 
                   "log_self_reference_avg_sharess")

categorical_feature_names <- ("weekday")
baseline_hypothesis <- getHypothesis(feature_names, categorical_feature_names=categorical_feature_names, degree=1)
baseline <- lm(formula=baseline_hypothesis, data=train_set)
valPred <- predict(baseline, val_set)
trainPred <- predict(baseline, train_set)

MAE <- function(preds, labels){
  mae_values <- sum(abs(preds-labels))/length(preds)
  return(mae_values)
}

MSE <- function(preds, labels){
  mse_values <- sum((preds-labels)**2)/length(preds)
  return(mse_values)
}

mae_train_baseline <- MAE(trainPred, train_set$target)
mae_train_baseline

mae_val_baseline <- MAE(valPred, val_set$target)
mae_val_baseline


total_mae_train_noCat <- c()
total_mae_val_noCat <- c()
for (i in 1:30){
  hypothesis <- getHypothesis(feature_names, categorical_feature_names, i)
  model_poly <- lm(formula=hypothesis, data=train_set)
  
  valPred <- predict(model_poly, val_set)
  trainPred <- predict(model_poly, train_set)
  
  mae_train <- MAE(trainPred, train_set$target)
  total_mae_train_noCat[i] <- mae_train
  
  mae_val <- MAE(valPred, val_set$target)
  total_mae_val_noCat[i] <- mae_val
  
}
total_mae_train_noCat
total_mae_val_noCat
plot(total_mae_train_noCat, xlab="Complexity", ylab="Error", 
     pch="+", col="red",  xaxt="n", 
     ylim=c(min(c(total_mae_train_noCat, total_mae_val_noCat, mae_val_baseline)),
            max(c(total_mae_train_noCat, total_mae_val_noCat, mae_val_baseline))))
