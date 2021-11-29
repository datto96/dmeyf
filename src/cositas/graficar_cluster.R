require("data.table")
require("ggplot2")


#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

setwd( "~/buckets/b1/" )

#leo el dataset , aqui se puede usar algun super dataset con Feature Engineering
datasetOri  <- fread( "datasets/semillerio_dataset_lag1.csv.gz")
setorder(  datasetOri,  numero_de_cliente, -foto_mes )   #ordeno, pero a la inversa

datasetOri[   , morire := 0 ]
datasetOri[ clase_ternaria=="BAJA+1" , morire := 1 ]  #si tengo un BAJA+1 , ese mes se que voy a morir

datasetOri[  , morire := cummax( morire ), numero_de_cliente ]   #calculo el maximo acumulado hace atras
datasetOri[  , meses_muerte := cumsum( morire ), numero_de_cliente ]   #calculo la suma acumulada


datasetOri[  meses_muerte==0,  meses_muerte := NA ]
datasetOri[  , morire := NULL ]

datasetOri  <-  datasetOri[ foto_mes>=201901  & foto_mes<=202011, ]
gc()

dataset  <- fread( "datasets/dataset_clusterizado_v100.csv.gz")

# me quedo solo con los numeros de cliente y cluster
dataset  <- dataset[, .SD, .SDcols = c("numero_de_cliente", "cluster2")]
gc()

datasetOri <- merge(datasetOri, dataset, by = "numero_de_cliente")
seguimiento <- datasetOri[ foto_mes <= 202011, .SD, .SDcols = -c("clase_ternaria")]

cluster_sizes <- dataset[ , .N,  cluster2 ]  #tamaño de los clusters

rm(datasetOri, dataset)
gc()


means <- seguimiento[, lapply(.SD, median, na.rm=TRUE), by=list(meses_muerte, cluster2) , .SDcols=-c("numero_de_cliente", "meses_muerte")]
stdevs <- seguimiento[, lapply(.SD, sd, na.rm=TRUE), by=list(meses_muerte, cluster2) , .SDcols=-c("numero_de_cliente", "meses_muerte")]
setorder(means, cols = foto_mes)

campos_buenos <- setdiff(colnames(means), c("foto_mes", "cluster2", "meses_muerte"))

pdf( paste0( paste0("./work/clusters_variables.pdf" ) ), 12, 8)
#campo = "ctrx_quarter"
for( campo in  campos_buenos ) {
  #tbl <- means[,.SD, .SDcols = c("meses_muerte", "cluster2", campo)]
  tbl <- seguimiento[,.SD, .SDcols = c("meses_muerte", "cluster2", campo)]
  print(
    ggplot(tbl,
           aes(x=-meses_muerte,
               y=get(campo),
               color=as.character(cluster2)
           )
    ) +
      geom_smooth(level = 0.97) +
      theme(axis.text.x = element_text(angle = 90)) +
      scale_x_continuous(breaks=seq(-20, 0)) +
      ggtitle(paste0("Media de clusters - ", campo, " - ", paste(cluster_sizes$N, collapse = ", "))) +
      xlab("Meses a baja") +
      ylab(campo)
  )
}

dev.off()

