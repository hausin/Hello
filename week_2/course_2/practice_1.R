
SayHello<- function(name){
  print(paste("Hello,", name))
}
SayHello('Pecu')

GetBmi<-function(my.height.cm, my.weight.kg){
  my.height.m<- my.height.cm/100
  my.bmi<- my.weight.kg/(my.height.m)^2
  return(my.bmi)
}

CheckBmiLevel <- function (my.height.cm, my.weight.kg) {
  # Call the GetBmi function we just made
  my.bmi <- GetBmi(my.height.cm, my.weight.kg)
    
    if (my.bmi >= 35) {
      return(paste("Your bmi: ", my.bmi, ", 重度肥胖!"))
    } else if (my.bmi >= 30) {
      return(paste("Your bmi: ", my.bmi, ", 中度肥胖!"))
    } else if (my.bmi >= 27) {
      return(paste("Your bmi: ", my.bmi, ", 輕度肥胖!"))
    } else if (my.bmi >= 24) {
      return(paste("Your bmi: ", my.bmi, ", 過重!"))
    } else if (my.bmi >= 18.5) {
      return(paste("Your bmi: ", my.bmi, ", 正常範圍"))
    } else {
      return(paste("Your bmi: ", my.bmi, ", 過輕!"))
    }  
}

bmi.level.msg <- CheckBmiLevel(176, 70) 
bmi.level.msg # "Your bmi:  22.5981404958678 , 正常範圍"

