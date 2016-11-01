daphnia <- read.csv('Daphnia_Tidy_Data.csv')
head(daphnia)

daphnia <- daphnia[complete.cases(daphnia),]

# Standardizing variables except Treatment


#daphnia$mm <- (daphnia$mm-mean(daphnia$mm))/sd(daphnia$mm)
#daphnia$Time <- (daphnia$Time-mean(daphnia$Time))/sd(daphnia$Time)

head(daphnia)

m1 <- map(
  alist(
    BeatsMin ~ dnorm(mu, sigma),
    mu <- a + bF*Treatment + 
      bM*mm +
      bT*Time,
    a ~ dnorm(400,100),
    bF ~ dnorm(0,100),
    bM ~ dnorm(0,100),
    bT ~ dnorm(0,100),
    sigma ~ dunif(0,100)
  ),
  data=daphnia)

precis(m1, corr=T)
plot(precis(m1))
?lines

mm.average <- mean(daphnia$mm)
Treatment.average <- mean(daphnia$Treatment)
Time.average <- mean(daphnia$Time)

mm.seq <- seq(from=0, to=2.3, by=0.001)
Treatment.seq <- c(0,1)
Time.seq <- seq(from=0, to=30, by=5)


# First, mean Treatment and Time, changing mm


predicted.data <- data.frame(Treatment=Treatment.average, Time=Time.average, mm=mm.seq)

mu <- link(m1 , data=predicted.data )

mu.mean <- apply( mu , 2 , mean )
mu.HPDI <- apply( mu , 2 , HPDI , prob=0.95 )

par(mfrow=c(1,1))
plot( BeatsMin ~ mm , data=daphnia , type="n" )
mtext( "Treatment and Time = mean" )
lines(mm.seq, mu.mean)
shade(mu.HPDI, mm.seq)

# Now, mean mm and Treatment, looking at Time

predicted.data <- data.frame(Treatment=Treatment.average, Time=Time.seq, mm=mm.average)

mu <- link(m1 , data=predicted.data )

mu.mean <- apply( mu , 2 , mean )
mu.HPDI <- apply( mu , 2 , HPDI , prob=0.95 )

par(mfrow=c(1,1))
plot( BeatsMin ~ Time , data=daphnia , type="n" )
mtext( "Treatment and mm = mean" )
lines(Time.seq, mu.mean)
shade(mu.HPDI, Time.seq)

# Finally, mm and Time average, looking at Treatment

predicted.data <- data.frame(Treatment=Treatment.seq, Time=Time.average, mm=mm.average)

mu <- link(m1 , data=predicted.data )

mu.mean <- apply( mu , 2 , mean )
mu.HPDI <- apply( mu , 2 , HPDI , prob=0.95 )

par(mfrow=c(1,1))
plot( BeatsMin ~ Treatment , data=daphnia, col=col.alpha(rangi2,0.4) )
mtext( "mm and Time = mean" )
lines(Treatment.seq, mu.mean)
shade(mu.HPDI, Treatment.seq)


