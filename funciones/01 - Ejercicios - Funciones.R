obtenerGananciasProductoSucursalExterno <- function(suc, prod) {
  if (class(prod) != "numeric" || prod < 1 || prod > 7) {
    return("Ingrese un numero de producto entre 1 y 7")
  }
  if (class(suc) != "numeric" || suc < 1 || suc > 3) {
    return("Ingrese un numero de sucursal entre 1 y 3")
  }
  gananciaTotal <- 0
  for (n in matrizGananciaMayorista[suc, prod]) {
    gananciaTotal <- gananciaTotal + n
  }
  for (j in matrizGananciaMayoristaInflacion[suc, prod]) {
    gananciaTotal <- gananciaTotal + j
  }
  return(paste("La ganancia total del producto", prod, "de la sucursal", suc, "fue de $", gananciaTotal))
}

obtenerGananciasProductoExterno <- function(prod) {
  if (class(prod) != "numeric" || prod < 1 || prod > 7) {
    return("Ingrese un numero de producto entre 1 y 7")
  }
  gananciaTotal <- 0
  for (n in matrizGananciaMayorista[, prod]) {
    gananciaTotal <- gananciaTotal + n
  }
  for (j in matrizGananciaMayoristaInflacion[, prod]) {
    gananciaTotal <- gananciaTotal + j
  }
  return(paste("La ganancia total del producto", prod, "fue de $", gananciaTotal))
}

obtenerGananciasSucursalExterno <- function(suc) {
  if (class(suc) != "numeric" || suc < 1 || suc > 3) {
    return("Ingrese un numero de sucursal entre 1 y 3")
  }
  gananciaTotal <- 0
  for (n in matrizGananciaMayorista[suc, ]) {
    gananciaTotal <- gananciaTotal + n
  }
  for (j in matrizGananciaMayoristaInflacion[suc, ]) {
    gananciaTotal <- gananciaTotal + j
  }
  return(paste("La ganancia total de la sucursal", suc, "fue de $", gananciaTotal))
}