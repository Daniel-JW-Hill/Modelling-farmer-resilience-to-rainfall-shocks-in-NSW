
# Creates numeric entry option for rainfall indices. 

createRainfallNumeric = function(inputname, label){
  
  numericInput(
    inputId = inputname,
    label   = label,
    value   = 0,
    min     = -0.75,
    max     = 1,
    step    = 0.05
  )
  
  
}