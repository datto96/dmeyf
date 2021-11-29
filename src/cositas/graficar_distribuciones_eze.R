#Necesita para correr en Google Cloud
#32 GB de memoria RAM
#256 GB de espacio en el disco local
#8 vCPU

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("ggplot2")
library(reshape2)

#Aqui comienza el programa
setwd("~/buckets/b1")

#cargo el dataset donde voy a entrenar
dataset  <- fread("datasets/dataset_clusterizado_v100.csv.gz")

#ordeno el dataset
setorder( dataset,  foto_mes )

campos_buenos  <-  setdiff(  colnames( dataset),  c("numero_de_cliente","foto_mes","cluster2" ) )

pdf("./work/boxplots.pdf", 20, 8)
for( campo in  campos_buenos )
{
  data <- dataset[ foto_mes<=202011 , c('cluster2',campo), with=FALSE]
  
  print(
    ggplot(data, aes(x=as.character(cluster2),y=get(campo), fill=as.character(cluster2))) +
      geom_boxplot(show.legend=FALSE) + labs(x='Cluster', y = as.character(campo))
  )
}
dev.off()
