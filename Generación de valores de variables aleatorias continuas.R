# Cargar la biblioteca
library(ggplot2)

# Definir las constantes
E <- 5
lambda <- 1
num_samples <- 10000
a_triangular <- 1
c_triangular <- 2
b_triangular <- 3 * E
a_uniform <- 1
b_uniform <- 2 * E
alpha_weibull <- 1  # Asumiendo alfa = 1 para Weibull

# Generar num_samples de U ~ Uniform(0,1)
U <- runif(num_samples)

# Variable aleatoria Y = ln(U)/-lambda
Y <- log(U) / -lambda

# Distribución Triangular
U_triangular <- runif(num_samples)
F_inv_triangular <- ifelse(U_triangular < (c_triangular - a_triangular) / (b_triangular - a_triangular),
                           a_triangular + sqrt(U_triangular * (b_triangular - a_triangular) * (c_triangular - a_triangular)),
                           b_triangular - sqrt((1 - U_triangular) * (b_triangular - a_triangular) * (b_triangular - c_triangular)))

# Distribución Uniforme
X_uniform <- a_uniform + (b_uniform - a_uniform) * runif(num_samples)

# Distribución Weibull
U_weibull <- runif(num_samples)
X_weibull <- lambda * (-log(1 - U_weibull))^(1 / alpha_weibull)

# Ahora las variables Y, F_inv_triangular, X_uniform, y X_weibull contienen los valores de las muestras aleatorias

# Crear histogramas
par(mfrow=c(2,2), mar=c(3,3,2,2)) # para que se muestren 2x2 gráficos en la misma página
hist(Y, main="Variable Aleatoria Y", xlab="Valor", ylab="Frecuencia")
hist(F_inv_triangular, main="Distribución Triangular", xlab="Valor", ylab="Frecuencia")
hist(X_uniform, main="Distribución Uniforme", xlab="Valor", ylab="Frecuencia")
hist(X_weibull, main="Distribución Weibull", xlab="Valor", ylab="Frecuencia")

# Crear dataframes
Y_df <- data.frame(Y)
F_inv_triangular_df <- data.frame(F_inv_triangular)
X_uniform_df <- data.frame(X_uniform)
X_weibull_df <- data.frame(X_weibull)

# Ver las primeras filas de cada dataframe
print("Las primeras filas de la variable aleatoria Y son:")
print(head(Y_df))

print("Las primeras filas de la distribución triangular son:")
print(head(F_inv_triangular_df))

print("Las primeras filas de la distribución uniforme son:")
print(head(X_uniform_df))

print("Las primeras filas de la distribución Weibull son:")
print(head(X_weibull_df))
