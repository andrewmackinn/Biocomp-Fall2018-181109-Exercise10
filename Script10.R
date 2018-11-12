dat = read.table("data.txt", sep = ",", header = TRUE, stringsAsFactors = FALSE)
nnlike= function(p,x,y){
  a =p[1]
  b=p[2]
  c=p[3]
  sigma = exp(p[4])
  expected = a +b*x +c*x^2
  nll = sum(dnorm(x=y, mean = expected, sd = sigma, log = TRUE))
}
initalGuess = c(1,1,1,1)
fit = optim(par = initialGuess, fn =nllike, x = dat$x, y = dat$y)
print(fit)


            

#expected = a +b*x +c*x^2
#r <1
#k = 1/alpha 
#alpha <.1