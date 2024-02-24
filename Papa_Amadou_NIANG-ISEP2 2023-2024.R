# Tracé de la fonction f(x,y)

f <- function(x,y){x^2 + y^2 + cos(x+y) + sqrt(y^2 + 1/(1+x^2))}
x <- seq(-50, 50, length = 50) # 50 valeurs de x entre -2 et 2
y <- seq(-50, 50, length = 50) # 50 valeurs de y entre -2 et 2
xy <- expand.grid(x = x, y = y) # grille de 50 x 50 points
z <- f(xy$x, xy$y) # valeurs de la fonction sur la grille
z <- matrix(z, nrow = 50, ncol = 50)
persp(x, y, z, theta = 30, phi = 20, col = "lightblue", shade = 0.5,xlab = "x", ylab = "y", zlab = "f(x,y)", main = "Graphique de la fonction f(x,y)")

# Tracé des dérivés
# Par rapport à x

g <- function(x,y){2 * x - sin(x + y) - (x * (y^2 + 1/(1 + x^2))^(-1/2))/(1 + x^2)^2}
z_dx <- g(xy$x, xy$y) # valeurs de la fonction sur la grille
z_dx <- matrix(z_dx, nrow = 50, ncol = 50)
persp(x, y, z_dx, theta = 30, phi = 20, col = "lightgreen", shade = 0.5, 
      xlab = "x", ylab = "y", zlab = "df/dx", main = "Graphique de la dérivée partielle de f par rapport à x")

# Par rapport à y

h <- function(x,y){2 * y - sin(x + y) + (y * (y^2 + 1/(1 + x^2))^(-1/2))/2}
z_dy <- h(xy$x, xy$y) # valeurs de la fonction sur la grille
z_dy <- matrix(z_dy, nrow = 50, ncol = 50)
persp(x, y, z_dy, theta = 30, phi = 20, col = "lightpink", shade = 0.5, 
      xlab = "x", ylab = "y", zlab = "df/dy", main = "Graphique de la dérivée partielle de f par rapport à y")

# Définir la fonction f
f <- function(x) {
  x[1]^2 + x[2]^2 + cos(x[1]+x[2]) + sqrt(x[2]^2 + 1/(1+x[1]^2))
}

# Choisir un point de départ
start <- c(0,0)

# Recherche du minimum
min <- optim(par = start, fn = f)
min

# Recherche du maximum (en inversant le signe de la fonction)
max <- optim(par = start, fn = function(x) -f(x))
max