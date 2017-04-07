#Test 3dplot

x.ab = x.snitt.d/y.snitt.d$sf_d_ab
y.hs = x.snitt.d/y.snitt.d$sf_d_hs
z.ns = x.snitt.d/y.snitt.d$sf_d_ns


plot3D::scatter3D(as.matrix(x.ab), as.matrix(y.hs), as.matrix(z.ns), xlab="TOTEX/ABO", ylab="TOTEX/HS", zlab="TOTEX/NS")
plot3D::surf3D(as.matrix(x.ab), as.matrix(y.hs), as.matrix(z.ns), xlab="TOTEX/ABO", ylab="TOTEX/HS", zlab="TOTEX/NS")
#, xlab="TOTEX/ABO", ylab="TOTEX/HS", zlab="TOTEX/NS"
my.array=array(0,c(119,1,3))
