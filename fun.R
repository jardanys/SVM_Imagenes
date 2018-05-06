#***********************************************************************************************************************
#********************************************    FUNCIONES DE AYUDA    *************************************************
#***********************************************************************************************************************

## Carga imagenes
load_image_file = function(filename) {
      ret = list()
      f = file(filename, 'rb')
      readBin(f, 'integer', n = 1, size = 4, endian = 'big')
      n    = readBin(f, 'integer', n = 1, size = 4, endian = 'big')
      nrow = readBin(f, 'integer', n = 1, size = 4, endian = 'big')
      ncol = readBin(f, 'integer', n = 1, size = 4, endian = 'big')
      x = readBin(f, 'integer', n = n * nrow * ncol, size = 1, signed = FALSE)
      close(f)
      data.frame(matrix(x, ncol = nrow * ncol, byrow = TRUE))
}


## Carga labels
load_label_file = function(filename) {
      f = file(filename, 'rb')
      readBin(f, 'integer', n = 1, size = 4, endian = 'big')
      n = readBin(f, 'integer', n = 1, size = 4, endian = 'big')
      y = readBin(f, 'integer', n = n, size = 1, signed = FALSE)
      close(f)
      y
}

## Función para graficar un número
show_digit <- function(numero, col = gray(12:1 / 12), ...) {
      image(matrix(as.matrix(numero[-785]), nrow = 28)[, 28:1], col = col, ...)
}
