
# creates slider for rainfall inputs
createRainfallSlider <- function(inputId, label) {
  sliderTextInput(
    inputId,
    label,
    choices = c(
      'Very dry (-1)', -0.8, -0.6, -0.45, -0.3, -0.15,
      'Typical rainfall (0)', 0.2, 0.35, 0.5, 0.7, 0.85, 1.0, 1.2,
      'Very wet (1.35)'
    ),
    selected = 'Typical rainfall (0)'
  )
}