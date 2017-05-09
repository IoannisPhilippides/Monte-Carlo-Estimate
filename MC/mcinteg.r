# Estimating int_0^1 exp(x) dx
B=1000
u=runif(B)
T=c(exp(u),exp(1-u))
mean(exp(u)); mean(T); exp(1)-1

m=mt=numeric(1000)
for (i in 1:1000){u=runif(100)
                  m[i]=mean(exp(u)); mt[i]=mean(c(exp(u),exp(1-u)))}
var(m); var(mt)
mean((m-exp(1)+1)^2); mean((mt-exp(1)+1)^2)
  

# Estimating a normal tail probability
B=1000
z=rnorm(B); x=z; y=z+5
mean(x>5); mean( exp(-5*y+12.5)*(y>5)); 1-pnorm(5)

m=mt=numeric(1000)
for (i in 1:1000){z=rnorm(B); x=z; y=z+5
                  m[i]=mean(x>5); mt[i]=mean( exp(-5*y+12.5)*(y>5))}
p=1-pnorm(5)
sqrt(mean((m-p)^2))/p; sqrt(mean((mt-p)^2))/p
