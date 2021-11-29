#Necesita para correr en Google Cloud
#64 GB de memoria RAM
#256 GB de espacio en el disco local
#10 vCPU

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("rlist")
require("yaml")
require("xgboost")
require("primes")  #para generar semillas
require("caret")

directory.root <- "~/buckets/b1/"
setwd( directory.root )

kexperimento  <- NA

kscript         <- "2000_berreta"
karch_dataset   <- "./datasets/base_dataset_FINAL03.csv.gz"  #el dataset que voy a utilizar
#karch_realidad  <- "./datasets/realidad_202101.csv"  #el dataset que voy a utilizar

ktest_mes_hasta  <- 202011  #Esto es lo que uso para testing
ktest_mes_desde  <- 202011

kgen_mes_hasta1   <- 202010  #hasta donde voy a entrenar
kgen_mes_desde1   <- 202001  #desde donde voy a entrenar

kgen_mes_hasta2   <- 202001  #hasta donde voy a entrenar
kgen_mes_desde2   <- 201901  #desde donde voy a entrenar

kgen_meses_malos <- 202006  #el mes que voy a eliminar del entreanamiento

kgen_subsampling <- 1.0     #esto es NO hacer undersampling

campos_malos  <- c()   #aqui se deben cargar todos los campos culpables del Data Drifting

#------------------------------------------------------------------------------
#Funcion que lleva el registro de los experimentos

get_experimento  <- function()
{
  if( !file.exists( "./maestro.yaml" ) )  cat( file="./maestro.yaml", "experimento: 1000" )
  
  exp  <- read_yaml( "./maestro.yaml" )
  experimento_actual  <- exp$experimento
  
  exp$experimento  <- as.integer(exp$experimento + 1)
  Sys.chmod( "./maestro.yaml", mode = "0644", use_umask = TRUE)
  write_yaml( exp, "./maestro.yaml" )
  Sys.chmod( "./maestro.yaml", mode = "0444", use_umask = TRUE) #dejo el archivo readonly
  
  return( experimento_actual )
}
#------------------------------------------------------------------------------
#Aqui empieza el programa

if( is.na(kexperimento ) )   kexperimento <- get_experimento()  #creo el experimento

#en estos archivos quedan los resultados
dir.create( paste0( "./work/E",  kexperimento, "/" ) )     #creo carpeta del experimento dentro de work

kresultados  <- paste0("./work/E",  kexperimento, "/E",  kexperimento, "_", kscript, ".txt" )  #archivo donde dejo el resultado


#cargo el dataset que tiene los 36 meses
dataset  <- fread(karch_dataset)

#valvula de seguridad para evitar valores infinitos
#paso los infinitos a NULOS
infinitos      <- lapply(names(dataset),function(.name) dataset[ , sum(is.infinite(get(.name)))])
infinitos_qty  <- sum( unlist( infinitos) )
if( infinitos_qty > 0 )
{
cat( "ATENCION, hay", infinitos_qty, "valores infinitos en tu dataset. Seran pasados a NA\n" )
dataset[mapply(is.infinite, dataset)] <- NA
}
  
  
#valvula de seguridad para evitar valores NaN  que es 0/0
#paso los NaN a 0 , decision polemica si las hay
#se invita a asignar un valor razonable segun la semantica del campo creado
nans      <- lapply(names(dataset),function(.name) dataset[ , sum(is.nan(get(.name)))])
nans_qty  <- sum( unlist( nans) )
if( nans_qty > 0 )
{
cat( "ATENCION, hay", nans_qty, "valores NaN 0/0 en tu dataset. Seran pasados arbitrariamente a 0\n" )
cat( "Si no te gusta la decision, modifica a gusto el programa!\n\n")
dataset[mapply(is.nan, dataset)] <- 0
}

#cargo los datos donde voy a aplicar el modelo
dtest  <- copy( dataset[ foto_mes>= ktest_mes_desde &  foto_mes<= ktest_mes_hasta,  ] )

#creo la clase_binaria2   1={ BAJA+2,BAJA+1}  0={CONTINUA}
dataset[ , clase01:= ifelse( clase_ternaria=="CONTINUA", 0, 1 ) ]

#agrego la marca de lo que necesito
#SI hago undersampling de los CONTINUA
vector_azar  <- runif( nrow(dataset) )

#los campos que se van a utilizar
campos_buenos  <- setdiff( colnames(dataset), 
                           c("clase_ternaria","clase01", "generacion", campos_malos) )


dataset[    foto_mes>= kgen_mes_desde1  &
              foto_mes<= kgen_mes_hasta1 & 
              !( foto_mes %in% kgen_meses_malos ) &
              ( clase01==1 | vector_azar < kgen_subsampling ),
            generacion:= 2020L ]  #donde genero el modelo

dtrain1  <- xgb.DMatrix(data=    data.matrix(  dataset[ generacion==2020 , campos_buenos, with=FALSE]),
                         label=   dataset[ generacion==2020, clase01])

dataset[    foto_mes>= kgen_mes_desde2  &
              foto_mes<= kgen_mes_hasta2 & 
              !( foto_mes %in% kgen_meses_malos ) &
              ( clase01==1 | vector_azar < kgen_subsampling ),
            generacion:= 2019L ]

dtrain2  <- xgb.DMatrix(data=    data.matrix(  dataset[ generacion==2019 , campos_buenos, with=FALSE]),
                         label=   dataset[ generacion==2019, clase01])

dtrains = list("2020" = dtrain1,"2019" = dtrain2)

rm( "dataset" )   #libero memoria para el dataset
gc()              #garbage collection


#Estos hiperparametros salieron de la optimizacion bayesiana del script 962
#ganancia  7706250  ( sobre la mitad de 202011 )
#hiperparametros encontrados en la iteracion bayesiana 41 de un total de 100 inteligentes

#ATENCION
#aqui deben ir los mejores valores que salieron de la optimizacion bayesiana

#donde voy a guardar los resultados
tb_resultados  <- data.table( traingen= integer(),
                              semilla= integer(), 
                              subsampling= numeric(),
                              ganancia= numeric(),
                              semillapubpriv= integer(),
                              ganancia_pub= numeric(),
                              ganancia_priv= numeric()
)

set.seed( 13 )   #dejo fija esta semilla
CANTIDAD_SEMILLAS  <- 10

#me genero un vector de semilla buscando numeros primos al azar
primos  <- generate_primes(min=100000, max=1000000)  #genero TODOS los numeros primos entre 100k y 1M
ksemillas  <- sample(primos)[ 1:CANTIDAD_SEMILLAS ]   #me quedo con CANTIDAD_SEMILLAS primos al azar
ksemillas  <- c( 999983, ksemillas )
ksemillapubpriv <- sample(primos)[ 1:CANTIDAD_SEMILLAS ]   #me quedo con CANTIDAD_SEMILLAS primos al azar
ksemillapubpriv  <- c( 999983, ksemillapubpriv )

for(traingen in names(dtrains))   #itero por las semillas
{ 

  print(paste(c("Entrenando el modelo con dtrain de año:", traingen), collapse = " "))
  dentrenamiento <- dtrains[[traingen]]
  for(  semillita  in  ksemillas )
  {
   gc()

    param_buenos  <- list( objective = "binary:logistic",
                           verbosity = 0,
                           grow_policy=  "lossguide",
                           tree_method = "hist",
                           eta = 0.010289098226641,
                           colsample_bytree = 0.148171863329694,
                           max_leaves = 684,
                           max_depth = 17,
                           alpha = 52.9510741276153,
                           lambda = 160.714175112341,
                           max_bin = 131,
                           min_child_weight = 0.584095485749355,
                           scale_pos_weight =  334.537618027577,
                           subsample = 0.860808228710788,
                           gamma = 0.237469859485962)
    
    #asigno la semilla a esta corrida
    
    set.seed( semillita )
    
    #genero el modelo, los hiperparametros son siempre los mismos, la semilla CAMBIA                         
    modelo  <- xgb.train(param = param_buenos,
                         nrounds = 1987,       # CAMBIAR!!!
                         data = dentrenamiento)
    
    param_buenos$seed  <- semillita 
      
    # aplico el modelo a los datos que elegi para testing  202011
    prediccion  <- predict( modelo, data.matrix( dtest[ , campos_buenos, with=FALSE]))
    
    tbl  <- as.data.table( list( "prob"=prediccion,  "gan"=  dtest[ , ifelse( clase_ternaria=="BAJA+2", 48750, - 1250)] ))
    
    setorder( tbl,  -prob )
    tbl[ , gan_acum :=  cumsum( gan ) ]
    ganancia  <-  tbl[ , max(gan_acum) ]
    
    for(semillitapubpriv in ksemillapubpriv)
    {
      set.seed(semillitapubpriv)
      pubIndex <- createDataPartition(dtest$clase01, p = .3, 
                                      list = FALSE, 
                                      times = 1)
      
      # aplico el modelo a los datos que elegi para testing  202011 public
      prediccionpub  <- predict( modelo, data.matrix( dtest[ pubIndex, campos_buenos, with=FALSE]) )
      tbl_pub  <- as.data.table( list( "prob"=prediccionpub,  "gan"=  dtest[pubIndex , ifelse( clase_ternaria=="BAJA+2", 48750, - 1250)] ))
      setorder( tbl_pub,  -prob )
      tbl_pub[ , gan_acum :=  cumsum( gan ) ]
      ganancia_pub_norm  <-  tbl_pub[ , max(gan_acum)]/0.3
      
      # aplico el modelo a los datos que elegi para testing  202011 private
      prediccionpriv  <- predict( modelo, data.matrix( dtest[ -pubIndex, campos_buenos, with=FALSE]) )
      tbl_priv  <- as.data.table( list( "prob"=prediccionpriv,  "gan"=  dtest[-pubIndex , ifelse( clase_ternaria=="BAJA+2", 48750, - 1250)] ))
      setorder( tbl_priv,  -prob )
      tbl_priv[ , gan_acum :=  cumsum( gan ) ]
      ganancia_priv_norm   <- tbl_priv[ , max(gan_acum)]/ 0.7

      tb_resultados  <- rbind( tb_resultados, list( traingen,
                                                    semillita, 
                                                    kgen_subsampling,
                                                    ganancia,
                                                    semillitapubpriv,
                                                    ganancia_pub_norm,
                                                    ganancia_priv_norm)) 
      
      #en cada iteracion GRABO
      fwrite(  tb_resultados,
               file= kresultados,
               sep= "\t" )
    }
    
  }
  
}


