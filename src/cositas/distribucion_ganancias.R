rm( list=ls() )  #remove all objects
gc() 


require("data.table")
require("rlist")
require("yaml")
require("ggplot2")

directory.root <- "~/buckets/b1/"
#setwd( directory.root )

karch_ganancias1  <- "./work/E1007_2000_berreta.txt"
dataset_ganancias1  <- fread(karch_ganancias1)
karch_ganancias2  <- "./work/E1008_2000_berreta.txt"
dataset_ganancias2  <- fread(karch_ganancias2)
karch_ganancias3  <- "./work/work_E5022_E5022_2000_berreta.txt"
dataset_ganancias3 <- fread(karch_ganancias3)
karch_ganancias4  <- "./work/E1010_2000_berreta.txt"
dataset_ganancias4 <- fread(karch_ganancias4)
karch_ganancias5  <- "./work/E1011_2000_berreta.txt"
dataset_ganancias5 <- fread(karch_ganancias5)

p <- ggplot(dataset_ganancias1, aes(x=ganancia)) + 
  geom_histogram()
p

gan_min_Public   <- pmin( dataset_ganancias1[  traingen==2020, min(ganancia_pub) ],
                          dataset_ganancias2[  traingen==2020, min(ganancia_pub) ],
                          dataset_ganancias3[  traingen==2020, min(ganancia_pub) ],
                          dataset_ganancias4[  traingen==2020, min(ganancia_pub) ],
                          dataset_ganancias5[  traingen==2020, min(ganancia_pub) ])

gan_min_Private  <- pmin( dataset_ganancias1[  traingen==2020, min(ganancia_priv) ],
                          dataset_ganancias2[  traingen==2020, min(ganancia_priv) ],
                          dataset_ganancias3[  traingen==2020, min(ganancia_priv) ],
                          dataset_ganancias4[  traingen==2020, min(ganancia_priv) ],
                          dataset_ganancias5[  traingen==2020, min(ganancia_priv) ])

gan_max_Public   <- pmax( dataset_ganancias1[  traingen==2020, max(ganancia_pub) ],
                          dataset_ganancias2[  traingen==2020, max(ganancia_pub) ],
                          dataset_ganancias3[  traingen==2020, max(ganancia_pub) ],
                          dataset_ganancias4[  traingen==2020, max(ganancia_pub) ],
                          dataset_ganancias5[  traingen==2020, max(ganancia_pub) ])

gan_max_Private  <- pmax( dataset_ganancias1[  traingen==2020, max(ganancia_priv) ],
                          dataset_ganancias2[  traingen==2020, max(ganancia_priv) ],
                          dataset_ganancias3[  traingen==2020, max(ganancia_priv) ],
                          dataset_ganancias4[  traingen==2020, max(ganancia_priv) ],
                          dataset_ganancias5[  traingen==2020, max(ganancia_priv) ])

puntos_cantidad  <- length(dataset_ganancias2[ traingen==2020, ganancia_priv ])

los70  <- sample( 1:puntos_cantidad, 70 )


mayores  <-  sum( dataset_ganancias2[  traingen==2020, ganancia_priv ] >  dataset_ganancias1[  , ganancia_priv ] )

superioridad  <-   mayores / 200

#grafico  Private vs Public  todos los puntos
plot( x= dataset_ganancias1[  traingen==2020, ganancia_pub ],
      y= dataset_ganancias1[  traingen==2020, ganancia_priv ],
      main= "Ganancias Private vs Public",
      xlab= "Ganancia PUBLIC",
      ylab= "Ganancia PRIVATE",
      xlim= c(gan_min_Public, gan_max_Public ),
      ylim= c(gan_min_Private, gan_max_Private ),
      col= "blue",
      pch= 15
)

points( x= dataset_ganancias2[  traingen==2020, ganancia_pub ],
        y= dataset_ganancias2[  traingen==2020, ganancia_priv ],
        col= "red",
        pch= 15 
)

points( x= dataset_ganancias3[  traingen==2020, ganancia_pub ],
        y= dataset_ganancias3[  traingen==2020, ganancia_priv ],
        col= "green",
        pch= 15 
)

points( x= dataset_ganancias4[  traingen==2020, ganancia_pub ],
        y= dataset_ganancias4[  traingen==2020, ganancia_priv ],
        col= "orange",
        pch= 15 
)

points( x= dataset_ganancias5[  traingen==2020, ganancia_pub ],
        y= dataset_ganancias5[  traingen==2020, ganancia_priv ],
        col= "pink",
        pch= 15 
)

#dibujo las medias
abline( v= mean(dataset_ganancias1[  traingen==2020, ganancia_pub ]) , col="blue" )
abline( h= mean(dataset_ganancias1[  traingen==2020, ganancia_priv ]) , col="blue" )

abline( v= mean(dataset_ganancias2[  traingen==2020, ganancia_pub ]) , col="red" )
abline( h= mean(dataset_ganancias2[  traingen==2020, ganancia_priv ]) , col="red" )

abline( v= mean(dataset_ganancias3[  traingen==2020, ganancia_pub ]) , col="green" )
abline( h= mean(dataset_ganancias3[  traingen==2020, ganancia_priv ]) , col="green" )

abline( v= mean(dataset_ganancias4[  traingen==2020, ganancia_pub ]) , col="orange" )
abline( h= mean(dataset_ganancias4[  traingen==2020, ganancia_priv ]) , col="orange" )

abline( v= mean(dataset_ganancias5[  traingen==2020, ganancia_pub ]) , col="pink" )
abline( h= mean(dataset_ganancias5[  traingen==2020, ganancia_priv ]) , col="pink" )


legend("topleft", 
       legend= c("E1007", "E1008", "E5022", "E1010", "E1011"),
       col= c("blue", "red", "green", "orange", "pink"),
       lty= c(1,1),
       pch= c(20,20), 
)

text( x= 25.7 ,
      y= 20.5 ,
      labels= paste0( "lag1+delta superador el ", superioridad ) )








gan_min  <- pmin( dataset_ganancias1[  traingen==2020, min(ganancia) ],
                          dataset_ganancias2[  traingen==2020, min(ganancia) ],
                          dataset_ganancias3[  traingen==2020, min(ganancia) ],
                          dataset_ganancias4[  traingen==2020, min(ganancia) ],
                          dataset_ganancias5[  traingen==2020, min(ganancia) ])

gan_max   <- pmax( dataset_ganancias1[  traingen==2020, max(ganancia) ],
                          dataset_ganancias2[  traingen==2020, max(ganancia) ],
                          dataset_ganancias3[  traingen==2020, max(ganancia) ],
                          dataset_ganancias4[  traingen==2020, max(ganancia) ],
                          dataset_ganancias5[  traingen==2020, max(ganancia) ])

p1<-hist(dataset_ganancias1[  traingen==2020, ganancia ])
p2<-hist(dataset_ganancias2[  traingen==2020, ganancia ])
p3<-hist(dataset_ganancias3[  traingen==2020, ganancia ])
p4<-hist(dataset_ganancias4[  traingen==2020, ganancia ])
p5<-hist(dataset_ganancias5[  traingen==2020, ganancia ])

plot( p1, col="blue", main= "Ganancias", xlab= "Ganancia", xlim=c(gan_min, gan_max ))
plot( p2, col="red", main= "Ganancias", xlab= "Ganancia", xlim=c(gan_min, gan_max ),add=T)
plot( p3, col="green", main= "Ganancias", xlab= "Ganancia", xlim=c(gan_min, gan_max ),add=T)
plot( p4, col="orange", main= "Ganancias", xlab= "Ganancia", xlim=c(gan_min, gan_max ),add=T)
plot( p5, col="pink", main= "Ganancias", xlab= "Ganancia", xlim=c(gan_min, gan_max ),add=T)

abline( v= mean(dataset_ganancias1[  traingen==2020, ganancia ]) , col="blue" )
abline( v= mean(dataset_ganancias2[  traingen==2020, ganancia ]) , col="red" )
abline( v= mean(dataset_ganancias3[  traingen==2020, ganancia ]) , col="green" )
abline( v= mean(dataset_ganancias4[  traingen==2020, ganancia ]) , col="orange" )
abline( v= mean(dataset_ganancias5[  traingen==2020, ganancia ]) , col="pink" )
