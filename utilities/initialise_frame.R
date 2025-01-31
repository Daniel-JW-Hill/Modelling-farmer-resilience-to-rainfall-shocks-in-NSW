
#initialises frame for simulations. 

initialise_frame = function(rows, columns, index_value) {
  frame = matrix(0, nrow = rows, ncol = columns)
  frame[, 1] = index_value
  return(frame)
}
