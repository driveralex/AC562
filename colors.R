findcolor <- function(input_val,low_val, high_val,inputcolor )
{
  range <- high_val - low_val;
  step.R <- floor(inputcolor.R * (input_val-low_val) / range) 
  step.G <- floor(inputcolor.G * (input_val-low_val) / range)
  step.B <- floor(inputcolor.B * (input_val-low_val) / range)
  step <- c(step.R/256,step.G/256,step.B/256)
}



inputcolor.R <- 146
inputcolor.G <- 39
inputcolor.B <- 143

lowval <- 10
highval <- 20

out <- findcolor(12,lowval,highval, inputcolor)

print(out)

