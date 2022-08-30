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


train_set <- read.csv(file = 'T01_train_set.csv')
val_set <- read.csv(file = 'T01_val_set.csv')

#x_train <- train_set[1:18];y_train <- train_set[19];
#x_val <- val_set[1:18]; y_val <- val_set[19];


print("Treino: Número de exemplos e atributos")
dim(train_set)
print("Validação: Número de exemplos e atributos")
dim(val_set)



head(train_set)
train_set$sunday <- as.numeric(train_set$weekday == "Sunday")
val_set$sunday <- as.numeric(val_set$weekday == "Sunday")
train_set$monday <- as.numeric(train_set$weekday == "Monday")
val_set$monday <- as.numeric(val_set$weekday == "Monday")
train_set$tuesday <- as.numeric(train_set$weekday == "Tuesday")
val_set$tuesday <- as.numeric(val_set$weekday == "Tuesday")
train_set$wednesday <- as.numeric(train_set$weekday == "Wednesday")
val_set$wednesday <- as.numeric(val_set$weekday == "Wednesday")
train_set$thursday <- as.numeric(train_set$weekday == "Thursday")
val_set$thursday <- as.numeric(val_set$weekday == "Thursday")
train_set$friday <- as.numeric(train_set$weekday == "Friday")
val_set$friday <- as.numeric(val_set$weekday == "Friday")
train_set$saturday <- as.numeric(train_set$weekday == "Saturday")
val_set$saturday <- as.numeric(val_set$weekday == "Saturday")






