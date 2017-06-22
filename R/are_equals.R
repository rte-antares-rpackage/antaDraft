are_equals <- function(CTY, CTA){
  browser()
  is.na(CTY) | is.na(CTA) | abs(CTY-CTA)>.Machine$double.eps
}

