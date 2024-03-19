coordenadasPositivas = function(v){
  res <- 0
  for(i in v){
    if(i >= 0){
      res <- res + i  
    }
  }
  return(res)
}
coordenadasPositivas(-1:3)
# ej 2
secuencia = seq(0,1,0.2)

efeDePe = function(p){p*(1-p)}

plot(secuencia,efeDePe(secuencia))

secuencia = seq(0,1,0.001)

plot(secuencia,efeDePe(secuencia))

# ej 3

grilla = seq(-50,50,1)

plot(grilla,sin(grilla),col ="red")
points(grilla,cos(grilla),col = "gold")
points(grilla,cos(grilla^2), col = "green")

# ej 4

df = read.csv("autos.txt",sep= " ")

# tercera fila
df[3,]
# segunda columna
df[,2]
# calidad del auto mas barato

df[df$precio == min(df$precio),]$calidad

#suma de precios para las primeras 4 filas

sum(df$precio[1:4])

#suma por columna

apply(df,2,sum)

#suma por fila

apply(df,1,sum)

plot(df$precio,df$calidad)

df <- df[order(df$precio),]

# ej 4
# los nombres de los autos con valor 4 en la variable gear (cantidad de engranajes delanteros)

df = mtcars

row.names(df[(df$gear == 4),])

# los nombres de los autos con 4 engranajes delanteros y transmisi´on manual

row.names(df[(df$gear == 4) & (df$am == 1),])

# la cantidad de autos que tienen 4 engranajes delanteros o transmisi´on manual

length(df[(df$gear == 4) | (df$am == 1),])

#convertir la variable am en un factor

df$am = factor(df$am)

#6. Simular, utilizando el comando rnorm(1000), 1000 realizaciones de una variable alea-
#toria con distribuci´on normal est´andar, guardar los valores en una variable x. Realizar
#un histograma, un boxplot y un qqnorm a partir de x.

x <- rnorm(1000)

hist(x)
boxplot(x)
qqnorm(x)
