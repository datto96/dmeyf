#Necesita para correr en Google Cloud
#64 GB de memoria RAM
#256 GB de espacio en el disco local
#4 vCPU


#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")

setwd("C:\\Users\\ldattoli\\Desktop\\Maestria\\2C\\DMeEyF") #establezco la carpeta donde voy a trabajar

version  <- "v001"  #cambiar cada vez, asi se tiene versionado del dataset

dataset  <- fread( "./datasetsOri/paquete_premium_202011.csv" )
dataset  <- copy(  dataset[  , c("numero_de_cliente","clase_ternaria"),  with=FALSE] )

gc()


#leo TODOS los archivos que estan en la carpeta  modelitos
#y hago el join con  dataset  <numero_de_cliente, foto_mes, clase_ternaria>

archivos  <- list.files( pattern="\\.csv", path="./modelitos/" )
for( archivo  in archivos )
{
  darchivo  <- fread( paste0("./modelitos/", archivo ) )
  dataset  <- merge( dataset, darchivo, by=c("numero_de_cliente") )
}

gc()

fwrite( dataset,
        file=paste0( "./datasets/dataset_stacking_", version, ".csv.gz"),
        sep="," )

