import math
def factorail(n):
    fac=1
    for num in range(1, n+1):
        fac*=num
    return fac
def expontail1(z):
    return math.e**z
# Without using the library
def poisson_pmf(lam, k):
    if k<0 :
        return 0
    return (expontail1(-lam)*(lam**k)/factorail(k))
poisson_pmf(1.5, 2)