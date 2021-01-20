
prop.p = function(p1,p2,n1,n2){

p = (p1*n1 + p2*n2)/(n1 + n2)

x = p*(1-p)*((1/n1) + (1/n2))

z = (p1 - p2)/(sqrt(abs(x)))

p = 2*pnorm(-abs(z))

p
}

p1 = 5
p2 = 10
n1 = 100
n2 = 100

