
bacon=function(X,alpha=0.15,const=4, ID)
{
  p=ncol(X)
  n=nrow(X)
  ID=rownames(X)
  m=const*p
  mx=colMeans(X)
  Sx=var(X)
  d=sqrt(mahalanobis(X,center=mx,cov=Sx))
  o=order(d)
  Xs=X[o[1:m],]
  test=TRUE
  while(test)
  {
    m=colMeans(Xs)
    S=var(Xs)
    d=sqrt(mahalanobis(X,center=m,cov=S))
    h=floor((n+p+1)/2)
    r=nrow(Xs)
    chr=max(0,(h-r)/(h+r))
    cnp=1+(p+1)/(n-1)+2/(n-1-3*p)
    cnpr=chr+cnp
    crit=cnpr*qchisq(1-alpha,df=p)
    ind=d<crit
    Xs=X[ind,]
    test=r!=sum(ind)
  }
  return(data.frame(X=X,outlier=ind,dist=d))
}

# READ DATA
xdata=read.csv("bacondata.csv")
X=xdata[,-1]
Xout=bacon(X)
Xout
