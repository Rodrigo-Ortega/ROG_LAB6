library(EconGeo)
?RCA
## generate a region - industry matrix
set.seed(31)
mat <- matrix(sample(0:100,20,replace=T), ncol = 4)
rownames(mat) <- c ("R1", "R2", "R3", "R4", "R5")
colnames(mat) <- c ("I1", "I2", "I3", "I4")
mat
dim(mat)
mat=RCA(mat,binary=T)
mat
c=co.occurrence(t(mat))
c
r=relatedness(c)
r
r[r<1]=0
r[r>1]=1
r
mat
rd=relatedness.density(mat,r)
rd
rd=get.list(rd)
rd
?entry.list
## generate a first region - industry matrix in which cells represent the presence/absence
## of a RCA (period 1)
set.seed(31)
mat1 <- matrix(sample(0:1,20,replace=T), ncol = 4)
rownames(mat1) <- c ("R1", "R2", "R3", "R4", "R5")
colnames(mat1) <- c ("I1", "I2", "I3", "I4")
mat1
## generate a second region - industry matrix in which cells represent the presence/absence
## of a RCA (period 2)
mat2 <- mat1
mat2[3,1] <- 1
mat2
d=entry.list(mat1,mat2)
d
colnames(d)=c("Region","Industry","Entry","Period")
d=merge(d,rd,by=c("Region","Industry"))
d
summary(lm(d$Entry~d$Count))