options(scipen = 999)

#librerias
library(recommenderlab)
library(dplyr)
library(rsparse)


#carga de datos
datos<-read.csv("Datos/Transformados/Matriz_Datos.csv",row.names = 1)
datos[datos==0]<-NA
str(datos)

#estadísticos
datos_m<-as.matrix(datos)
datos_rrm<-as(datos_m,"realRatingMatrix")

colSums(datos_rrm)
rowSums(datos_rrm)
sum(colCounts(datos_rrm)<5)
rowCounts(datos_rrm)

max(datos,na.rm=T)
min(datos,na.rm=T)

hist(colSds(datos_rrm))
hist(rowSds(datos_rrm))
hist(colMeans(datos_rrm))
hist(rowMeans(datos_rrm))
hist(colCounts(datos_rrm))
hist(rowCounts(datos_rrm))

datos_rrm<-datos_rrm[,colCounts(datos_rrm)>125] ##########
datos_rrm<-datos_rrm[rowCounts(datos_rrm)>15,] ########

median(rowCounts(datos_rrm))
min(rowCounts(datos_rrm))


# Con muestra -------------------------------------------------------------


#CON MUESTREO
set.seed(73);filas<-sample(1:20847,4000)
set.seed(73);columnas<-sample(1:1468,500)

datos_muestra<-datos_rrm[filas,columnas]

datos_rrm_muestra<-as(datos_muestra,"realRatingMatrix")

set.seed(7);eval_schem_muestra<-evaluationScheme(datos_rrm_muestra,method="split",train=0.8,goodRating=1,given=1)

#ibcf 

eval_recommender_ibcf_muestra<-Recommender(getData(eval_schem_muestra,"train"),"IBCF",param=list(method="Pearson"))

eval_prediction_ibcf_muestra<-predict(object=eval_recommender_ibcf_muestra,newdata=getData(eval_schem_muestra,"known"),type="ratings")

eval_accuracy_ibcf_muestra<-calcPredictionAccuracy(eval_prediction_ibcf_muestra,getData(eval_schem_muestra,"unknow"),given=10,goodRating=1)

eval_prediction_ibcf_NL_muestra<-predict(object=eval_recommender_ibcf_muestra,newdata=getData(eval_schem_muestra,"known"),type="topNList",n=10)

eval_accuracy_ibcf_NL_muestra<-calcPredictionAccuracy(eval_prediction_ibcf_NL_muestra,getData(eval_schem_muestra,"unknow"),goodRating=2,given=10)

# UBCF - muestra
eval_recommender_ubcf_muestra <- Recommender(getData(eval_schem_muestra, "train"), "UBCF", param = NULL)

eval_prediction_ubcf_muestra <- predict(object = eval_recommender_ubcf_muestra, newdata = getData(eval_schem_muestra, "known"), type = "ratings")

eval_accuracy_ubcf_muestra <- calcPredictionAccuracy(eval_prediction_ubcf_muestra, getData(eval_schem_muestra, "unknow"), given = 10, goodRating = 2)

eval_prediction_ubcf_NL_muestra <- predict(object = eval_recommender_ubcf_muestra, newdata = getData(eval_schem_muestra, "known"), type = "topNList", n = 5)

eval_accuracy_ubcf_NL_muestra <- calcPredictionAccuracy(eval_prediction_ubcf_NL_muestra, getData(eval_schem_muestra, "unknow"), goodRating = 2, given = 10)


# Random - muestra
eval_recommender_random_muestra <- Recommender(getData(eval_schem_muestra, "train"), "random", param = NULL)

eval_prediction_random_muestra <- predict(object = eval_recommender_random_muestra, newdata = getData(eval_schem_muestra, "known"), type = "ratings")

eval_accuracy_random_muestra <- calcPredictionAccuracy(eval_prediction_random_muestra, getData(eval_schem_muestra, "unknow"))

eval_prediction_random_NL_muestra <- predict(object = eval_recommender_random_muestra, newdata = getData(eval_schem_muestra, "known"), type = "topNList", n = 5)

eval_accuracy_random_NL_muestra <- calcPredictionAccuracy(eval_prediction_random_NL_muestra, getData(eval_schem_muestra, "unknow"), goodRating = 2, given = 10)


# Popular - muestra
eval_recommender_popular_muestra <- Recommender(getData(eval_schem_muestra, "train"), "popular", param = NULL)

eval_prediction_popular_muestra <- predict(object = eval_recommender_popular_muestra, newdata = getData(eval_schem_muestra, "known"), type = "ratings")

eval_accuracy_popular_muestra <- calcPredictionAccuracy(eval_prediction_popular_muestra, getData(eval_schem_muestra, "unknow"))

eval_prediction_popular_NL_muestra <- predict(object = eval_recommender_popular_muestra, newdata = getData(eval_schem_muestra, "known"), type = "topNList", n = 5)

eval_accuracy_popular_NL_muestra <- calcPredictionAccuracy(eval_prediction_popular_NL_muestra, getData(eval_schem_muestra, "unknow"), goodRating = 2, given = 10)


# SVD - muestra
eval_recommender_SVD_muestra <- Recommender(getData(eval_schem_muestra, "train"), "SVD", param = NULL)

eval_prediction_SVD_muestra <- predict(object = eval_recommender_SVD_muestra, newdata = getData(eval_schem_muestra, "known"), type = "ratings")

eval_accuracy_SVD_muestra <- calcPredictionAccuracy(eval_prediction_SVD_muestra, getData(eval_schem_muestra, "unknow"))

eval_prediction_SVD_NL_muestra <- predict(object = eval_recommender_SVD_muestra, newdata = getData(eval_schem_muestra, "known"), type = "topNList", n = 5)

eval_accuracy_SVD_NL_muestra <- calcPredictionAccuracy(eval_prediction_SVD_NL_muestra, getData(eval_schem_muestra, "unknow"), goodRating = 2, given = 10)



resultados_datos_no_mod<-rbind(eval_accuracy_ibcf_NL,eval_accuracy_ubcf_NL,eval_accuracy_random_NL,eval_accuracy_popular_NL)




#####
#con muestra normalizada 
datos_rrm_muestra_normalizada<-normalize(datos_rrm_muestra)

set.seed(7);eval_schem_muestra_normalizada<-evaluationScheme(datos_rrm_muestra_normalizada,method="split",train=0.8,goodRating=1,given=1)

# IBCF - muestra normalizada
eval_recommender_ibcf_muestra_norm <- Recommender(getData(eval_schem_muestra_normalizada, "train"), "IBCF", param = list(method = "Pearson"))

eval_prediction_ibcf_muestra_norm <- predict(object = eval_recommender_ibcf_muestra_norm, newdata = getData(eval_schem_muestra_normalizada, "known"), type = "ratings")

eval_accuracy_ibcf_muestra_norm <- calcPredictionAccuracy(eval_prediction_ibcf_muestra_norm, getData(eval_schem_muestra_normalizada, "unknow"), given = 10, goodRating = 1)

eval_prediction_ibcf_NL_muestra_norm <- predict(object = eval_recommender_ibcf_muestra_norm, newdata = getData(eval_schem_muestra_normalizada, "known"), type = "topNList", n = 10)

eval_accuracy_ibcf_NL_muestra_norm <- calcPredictionAccuracy(eval_prediction_ibcf_NL_muestra_norm, getData(eval_schem_muestra_normalizada, "unknow"), goodRating = 2, given = 10)


# UBCF - muestra normalizada
eval_recommender_ubcf_muestra_norm <- Recommender(getData(eval_schem_muestra_normalizada, "train"), "UBCF", param = NULL)

eval_prediction_ubcf_muestra_norm <- predict(object = eval_recommender_ubcf_muestra_norm, newdata = getData(eval_schem_muestra_normalizada, "known"), type = "ratings")

eval_accuracy_ubcf_muestra_norm <- calcPredictionAccuracy(eval_prediction_ubcf_muestra_norm, getData(eval_schem_muestra_normalizada, "unknow"), given = 10, goodRating = 2)

eval_prediction_ubcf_NL_muestra_norm <- predict(object = eval_recommender_ubcf_muestra_norm, newdata = getData(eval_schem_muestra_normalizada, "known"), type = "topNList", n = 5)

eval_accuracy_ubcf_NL_muestra_norm <- calcPredictionAccuracy(eval_prediction_ubcf_NL_muestra_norm, getData(eval_schem_muestra_normalizada, "unknow"), goodRating = 2, given = 10)


# Random - muestra normalizada
eval_recommender_random_muestra_norm <- Recommender(getData(eval_schem_muestra_normalizada, "train"), "random", param = NULL)

eval_prediction_random_muestra_norm <- predict(object = eval_recommender_random_muestra_norm, newdata = getData(eval_schem_muestra_normalizada, "known"), type = "ratings")

eval_accuracy_random_muestra_norm <- calcPredictionAccuracy(eval_prediction_random_muestra_norm, getData(eval_schem_muestra_normalizada, "unknow"))

eval_prediction_random_NL_muestra_norm <- predict(object = eval_recommender_random_muestra_norm, newdata = getData(eval_schem_muestra_normalizada, "known"), type = "topNList", n = 5)

eval_accuracy_random_NL_muestra_norm <- calcPredictionAccuracy(eval_prediction_random_NL_muestra_norm, getData(eval_schem_muestra_normalizada, "unknow"), goodRating = 2, given = 10)


# Popular - muestra normalizada
eval_recommender_popular_muestra_norm <- Recommender(getData(eval_schem_muestra_normalizada, "train"), "popular", param = NULL)

eval_prediction_popular_muestra_norm <- predict(object = eval_recommender_popular_muestra_norm, newdata = getData(eval_schem_muestra_normalizada, "known"), type = "ratings")

eval_accuracy_popular_muestra_norm <- calcPredictionAccuracy(eval_prediction_popular_muestra_norm, getData(eval_schem_muestra_normalizada, "unknow"))

eval_prediction_popular_NL_muestra_norm <- predict(object = eval_recommender_popular_muestra_norm, newdata = getData(eval_schem_muestra_normalizada, "known"), type = "topNList", n = 5)

eval_accuracy_popular_NL_muestra_norm <- calcPredictionAccuracy(eval_prediction_popular_NL_muestra_norm, getData(eval_schem_muestra_normalizada, "unknow"), goodRating = 2, given = 10)


# SVD - muestra normalizada
eval_recommender_SVD_muestra_norm <- Recommender(getData(eval_schem_muestra_normalizada, "train"), "SVD", param = NULL)

eval_prediction_SVD_muestra_norm <- predict(object = eval_recommender_SVD_muestra_norm, newdata = getData(eval_schem_muestra_normalizada, "known"), type = "ratings")

eval_accuracy_SVD_muestra_norm <- calcPredictionAccuracy(eval_prediction_SVD_muestra_norm, getData(eval_schem_muestra_normalizada, "unknow"))

eval_prediction_SVD_NL_muestra_norm <- predict(object = eval_recommender_SVD_muestra_norm, newdata = getData(eval_schem_muestra_normalizada, "known"), type = "topNList", n = 5)

eval_accuracy_SVD_NL_muestra_norm <- calcPredictionAccuracy(eval_prediction_SVD_NL_muestra_norm, getData(eval_schem_muestra_normalizada, "unknow"), goodRating = 2, given = 10)



# predicciones con datos sin modificar ------------------------------------------------

#partición de datos
set.seed(7);eval_schem<-evaluationScheme(datos_rrm,method="split",train=0.8,given=10,goodRating=2)

nrow(getData(eval_schem,"train"))/nrow(datos_rrm)
nrow(getData(eval_schem,"known"))/nrow(datos_rrm)
nrow(getData(eval_schem,"unknown"))/nrow(datos_rrm)


#ibcf
eval_recommender_ibcf<-Recommender(getData(eval_schem,"train"),"IBCF",param=list(method="Pearson"))

eval_prediction_ibcf<-predict(object=eval_recommender_ibcf,newdata=getData(eval_schem,"known"),type="ratings")

eval_accuracy_ibcf<-calcPredictionAccuracy(eval_prediction_ibcf,getData(eval_schem,"unknow"),given=10,goodRating=1)

eval_prediction_ibcf_NL<-predict(object=eval_recommender_ibcf,newdata=getData(eval_schem,"known"),type="topNList",n=10)

eval_accuracy_ibcf_NL<-calcPredictionAccuracy(eval_prediction_ibcf_NL,getData(eval_schem,"unknow"),goodRating=2,given=10)


#ubcf
eval_recommender_ubcf<-Recommender(getData(eval_schem,"train"),"UBCF",param=NULL)

eval_prediction_ubcf<-predict(object=eval_recommender_ubcf,newdata=getData(eval_schem,"known"),type="ratings")

eval_accuracy_ubcf<-calcPredictionAccuracy(eval_prediction_ubcf,getData(eval_schem,"unknow"),given=10,goodRating=2)

eval_prediction_ubcf_NL<-predict(object=eval_recommender_ubcf,newdata=getData(eval_schem,"known"),type="topNList",n=5)

eval_accuracy_ubcf_NL<-calcPredictionAccuracy(eval_prediction_ubcf_NL,getData(eval_schem,"unknow"),goodRating=2,given=10)


#random
eval_recommender_random<-Recommender(getData(eval_schem,"train"),"random",param=NULL)

eval_prediction_random<-predict(object=eval_recommender_random,newdata=getData(eval_schem,"known"),type="ratings")

eval_accuracy_random<-calcPredictionAccuracy(eval_prediction_random,getData(eval_schem,"unknow"))

eval_prediction_random_NL<-predict(object=eval_recommender_random,newdata=getData(eval_schem,"known"),type="topNList",n=5)

eval_accuracy_random_NL<-calcPredictionAccuracy(eval_prediction_random_NL,getData(eval_schem,"unknow"),goodRating=2,given=10)


#popular
eval_recommender_popular<-Recommender(getData(eval_schem,"train"),"popular",param=NULL)

eval_prediction_popular<-predict(object=eval_recommender_popular,newdata=getData(eval_schem,"known"),type="ratings")

eval_accuracy_popular<-calcPredictionAccuracy(eval_prediction_popular,getData(eval_schem,"unknow"))

eval_prediction_popular_NL<-predict(object=eval_recommender_popular,newdata=getData(eval_schem,"known"),type="topNList",n=5)

eval_accuracy_popular_NL<-calcPredictionAccuracy(eval_prediction_popular_NL,getData(eval_schem,"unknow"),goodRating=2,given=10)


#SVDF
eval_recommender_SVD<-Recommender(getData(eval_schem,"train"),"SVD",param=NULL)

eval_prediction_SVD<-predict(object=eval_recommender_SVD,newdata=getData(eval_schem,"known"),type="ratings")

eval_accuracy_SVD<-calcPredictionAccuracy(eval_prediction_SVD,getData(eval_schem,"unknow"))

eval_prediction_SVD_NL<-predict(object=eval_recommender_SVD,newdata=getData(eval_schem,"known"),type="topNList",n=5)

eval_accuracy_SVD_NL<-calcPredictionAccuracy(eval_prediction_SVD_NL,getData(eval_schem,"unknow"),goodRating=2,given=10)


#ALS
eval_recommender_ALS<-Recommender(getData(eval_schem,"train"),"ALS",param=NULL)

eval_prediction_ALS<-predict(object=eval_recommender_ALS,newdata=getData(eval_schem,"known"),type="ratings")

eval_accuracy_ALS<-calcPredictionAccuracy(eval_prediction_ALS,getData(eval_schem,"unknow"),given=10,goodRating=2)

eval_prediction_ALS_NL<-predict(object=eval_recommender_ALS,newdata=getData(eval_schem,"known"),type="topNList",n=5)

eval_accuracy_ALS_NL<-calcPredictionAccuracy(eval_prediction_ALS_NL,getData(eval_schem,"unknow"),goodRating=2,given=10)


#results
resultados_datos_no_mod<-rbind(eval_accuracy_ibcf_NL,eval_accuracy_ubcf_NL,eval_accuracy_random_NL,eval_accuracy_popular_NL,eval_accuracy_SVD_NL)


# con datos normalizados --------------------------------------------------

datos_rrm_normalizados<-normalize(datos_rrm)

#partición de datos
set.seed(7);eval_schem<-evaluationScheme(datos_rrm_normalizados,method="split",train=0.8,given=10,goodRating=2)

#ibcf
eval_recommender_ibcf<-Recommender(getData(eval_schem,"train"),"IBCF",param=list(method="Pearson"))

eval_prediction_ibcf<-predict(object=eval_recommender_ibcf,newdata=getData(eval_schem,"known"),type="ratings")

eval_accuracy_ibcf<-calcPredictionAccuracy(eval_prediction_ibcf,getData(eval_schem,"unknow"),given=10,goodRating=1)

eval_prediction_ibcf_NL<-predict(object=eval_recommender_ibcf,newdata=getData(eval_schem,"known"),type="topNList",n=10)

eval_accuracy_ibcf_NL<-calcPredictionAccuracy(eval_prediction_ibcf_NL,getData(eval_schem,"unknow"),goodRating=2,given=10)



# con datos binarizados ---------------------------------------------------


datos_rrm_binarizados<-binarize(datos_rrm,minRating=1)

#partición de datos
set.seed(7);eval_schem_bin<-evaluationScheme(datos_rrm_binarizados,method="split",train=0.8,given=10,goodRating=2)

#ibcf
eval_recommender_ibcf_bin<-Recommender(getData(eval_schem_bin,"train"),"IBCF",param=list(method="Pearson"))

eval_prediction_ibcf_NL_bin<-predict(object=eval_recommender_ibcf_bin,newdata=getData(eval_schem_bin,"known"),type="topNList",n=10)

eval_accuracy_ibcf_NL_bin<-calcPredictionAccuracy(eval_prediction_ibcf_NL_bin,getData(eval_schem_bin,"unknow"),goodRating=2,given=10)

# UBCF
eval_recommender_ubcf_bin <- Recommender(getData(eval_schem_bin, "train"), "UBCF", param = list(method = "Pearson"))

eval_prediction_ubcf_NL_bin <- predict(object = eval_recommender_ubcf_bin,newdata = getData(eval_schem_bin, "known"),type = "topNList",n = 10)

eval_accuracy_ubcf_NL_bin <- calcPredictionAccuracy(eval_prediction_ubcf_NL_bin,getData(eval_schem_bin, "unknow"),goodRating = 2, given = 10)

