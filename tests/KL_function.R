library(kldest)
# 1D example
mvdnorm(x = 2, mu = 1, Sigma = 2)
dnorm(x = 2, mean = 1, sd = sqrt(2))
# Independent 2D example
mvdnorm(x = c(2,2), mu = c(1,1), Sigma = diag(1:2))
prod(dnorm(x = c(2,2), mean = c(1,1), sd = sqrt(1:2)))
# Correlated 2D example
mvdnorm(x = c(2,2), mu = c(1,1), Sigma = matrix(c(2,1,1,2),nrow=2))
mvdnorm(x = c(2,2,3), mu = c(1,1,1), Sigma = matrix(c(2,1,1,1, 2,1,1,1,2),nrow=3))

X <- c(rep('M',5),rep('F',5))
Y <- c(rep('M',6),rep('F',4))
kld_est_discrete(obs_data, sim_data[sim_data$simulation_nr == 1,-4])

nn = 1000
X1 <- rnorm(nn)
X2 <- rnorm(nn)
X3 <- rnorm(nn)
Y1 <- rnorm(nn)
Y2 <- Y1 + rnorm(nn)
Y3 <- Y2 + rnorm(nn)
X <- cbind(X1,X2,X3)
Y <- cbind(Y1,Y2,Y3)
kld_est_brnn(X, Y)
kld_est_nn(X, Y)


# our example
# nn = 100
nn = 4000
obs_data_s <- sample_n(obs_data,nn,replace = TRUE) %>% as.matrix()
sim_data_s <- sample_n(sim_data[sim_data$simulation_nr == 1,-4], nn, replace = TRUE) %>% as.matrix()
kld_est_nn(obs_data_s, sim_data_s)
kld_est_brnn(obs_data_s, sim_data_s)
