
magick_gravity_names <- magick::gravity_types()

magick_filter_names <- magick::filter_types()


convert_r_colour_to_magick_colour <- function(col) {
  if (is.null(col) || is.na(col) || length(col) == 0) {
    return('none')
  }
  rgb(t(col2rgb(col)), maxColorValue = 255)
}

my_abind <- function(arr, mat) {
  
  stopifnot(is.array(arr))
  stopifnot(is.matrix(mat))
  if (!identical(head(dim(arr), -1), dim(mat))) {
    stop("Dimension missmatch. Array: ", deparse(dim(arr)), "  Matrix: ", deparse(dim(mat)))
  }
  
  new_dim    <- dim(arr)
  new_dim[3] <- new_dim[3] + 1
  
  array(c(arr, mat), dim = new_dim)
}


img <- AllImDat[[1]]
convert_img_to_array <- function(img) {
  
  stopifnot(inherits(img, 'magick-image'))
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # extract the RGB array from that image
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  arr <- as.numeric(magick::image_data(img))
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # If this is a grey image (i.e. a 2d matrix), then promote it
  # to a 3d array by copying the grey into R,G and B planes
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (length(dim(arr)) == 2) {
    arr <- array(c(arr, arr, arr), dim = c(dim(arr), 3))
  }
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Add an alpha channel if there isn't one already
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (dim(arr)[3] == 3) {
    alpha_matrix <- matrix(1, nrow=dim(arr)[1], ncol = dim(arr)[2])
    arr          <- my_abind(arr, alpha_matrix)
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Sanity check: Assert everything image is RGBA
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stopifnot(dim(arr)[3] == 4)
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Transpose the image if requested.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # if (transpose) {
  #   arr <- aperm(arr, c(2, 1, 3))
  # }
  
  arr
}