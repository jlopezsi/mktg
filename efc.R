#European food consumption
#Ilustra el uso de los componentes principales
#Step 1: getting data
url = 'http://www.stat.ucla.edu/~rgould/120bs06/protein.txt'
data.orig <- read.table(url, header=TRUE,
                        sep="\t")
head(data.orig)
# For PCA analysis, keep all the variables
# except the first column with country names: 
data <- data.orig[, -1]

#Step 2a: Perform Preliminary Diagnostics Graphically
par(las=2, cex.axis=0.8,
    mfrow=c(1,2), mai=c(1,0.5,1,0.1) )
boxplot(data,
        main="'Protein' data set.",
        col=topo.colors(ncol(data))
) 
boxplot(data.frame(cor(data)),
          main="cor('Protein') data set.", col=topo.colors(ncol(data)))

#Step 2b: Perform Preliminary Diagnostics (more) Rigorously

bartlett.test(data.frame(cor(data)))

#step 3a:  perform PCA

out.cor <- princomp(data, cor=TRUE)
lambda_perc <- out.cor$sdev^2/sum(out.cor$sdev^2)
V <- out.cor$loadings
Y <-cor(data)%*%V 
# Y=XV
PC <- out.cor$scores 
# PC = (standardized dataset)V 
### Compare with ?prcomp and ?svd


#Step 3b: Check the Math of PCA
#correlation among variables
cor(data)
#correlation matrix, variables with components
V
Y
PC
#Step 4: Determine How Many Components to Keep

par( mai=c(1,1,0.1,0.1) ) 
plot(lambda_perc,
                               type="b",
                               xlab="Component Number",
                               ylab="Percent total variance explained", pch=16
)
text(x=1:ncol(cor(data)), y=lambda_perc,
     labels=round(cumsum(lambda_perc), 2), adj=-0.3
)

#step 5: Interpret the Results

biplot(out.cor, pc.biplot=TRUE,
       xlabs=data.orig$Country, xlim=c(-3, 3)
)
abline(h=0, lty=2,
       col="grey50"
) 
abline(v=0,
         lty=2, col="grey50" 
         )

#Step 5b: Check the Math for Portugal

#Step 6: Analyze Residuals
#Residuals = X - (X V(0) ) V(T,0)
X_hat <- cor(data) -
  (cor(data) %*% V[, 1:2]) %*% t(V[, 1:2])
n_obs <- nrow(X_hat) * ncol(X_hat)
library(lattice)
levelplot(X_hat, 
          col.regions=heat.colors( n_obs ), 
          scales=list( x=list(rot = 90) )
)

#Example 2: Pasadena Meetup.com Groups I
#Data Set Creation

# Step 1: Read the Dataset into R

dist_mat <- read.csv("meetup_dist_mat.csv", row.names=1)
#Step 2: Perform PCA
out.cor <- princomp(dist_mat, cor=TRUE)
lambda_perc <- out.cor$sdev^2/sum(out.cor$sdev^2)
V <- out.cor$loadings
Y <- cor(dist_mat) %*% V 
# Y= XV
PC <- out.cor$scores 
# PC = (standardized dataset)V

#Step 3: Determine How Many Components to Keep
lambda_perc_cumsum <- cumsum(lambda_perc)
par(mfrow=c(1,1))
plot(lambda_perc[2:20], type="b",
     xlab="Component Number (2-20)", ylab="% total variance explained", axes=FALSE)
axis(1, at=1:20,
     lab=2:21,
     las=2) box()
text(x=2:20, y=lambda_perc[2:20],
     labels=round( lambda_perc_cumsum[2:20],
                   digits=2
     ), adj=-0.1,
     pos=3, cex=0.7 )

#Step 4: Interpret the Results
##Look at relationships between weights
V_first4 <- V[, 1:4]
### --- Look at relationships between components 
Y_first4 <- Y[, 1:4]

##Step 4: Interpret the Results
#First four loadings can be interpreted as follows: Component 1 None
#Component 2 social/networking (pos weights) versus fitness/yoga (neg weights)
#Component 3 personal development (pos weights) versus kids and pets (neg weights)
#Component 4 networking/pets (pos weights) versus moms (neg weights)

#Step 5: Alternative Analysis via Co-Clustering

# 'nbcocluster = 4 clusters for rows and 4 for columns 
# 'model' = equal-sized clusters with unequal variance 
library(blockcluster)
out <- cocluster(cor(dist_mat),
                 datatype="continuous", 
                 nbcocluster=c(4,4), 
                 model="pi_rho_sigma2kl" 
                 )
## Co-Clustering successfully terminated! 
par(mfrow=c(1,1))
plot(out)

# Assumptions of PCA
#1 Principal Components are uncorrelated with each other.
#2 Observed variables are combinations of Principal Components).
#  a Observed variables are unconstrained.
# b Observed variables are continuous.
# c Observed variables are linear combinations of Principal Components).
#3 Observed variable means and the variance-covariance matrix
#capture all of the information in the data set.
#4 Observed variables are best represented by an N ⇥ M matrix.
#5 Principal Components are unique up to sign and permutation.

￼￼￼￼￼￼￼￼￼￼￼￼￼￼￼￼￼￼￼￼￼￼￼
