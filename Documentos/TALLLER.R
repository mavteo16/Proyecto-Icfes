#greamos una gravica
sam<-c(Movies_gross_rating)
sam$Genre

as.data.frame(table(sam$Genre))

tabla<-as.data.frame(table(sam$Genre))
transform(tabla,
          freqAC=cumsum(tabla