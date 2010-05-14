from pylab import *

x = range(2,20)

P = figure(1)
plot(x,x, figure=P)
P.savefig("test1.png")

Q = figure(2)
loglog(x, figure=Q)
Q.savefig("test2.png")

