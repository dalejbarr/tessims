library(plyr) # of course

# generate data from two independent groups
# and run a t.test
gendata <- function(eff=.6,sdd=1,sample_size=24) {
    dat <- data.frame(Group=rep(c("G1","G2"),each=sample_size),
                      Y=c(rnorm(sample_size,0,sdd),
                          rnorm(sample_size,eff,sdd)))
    estmeans <- aggregate(Y~Group, dat, mean)$Y
    esteff <- estmeans[2]-estmeans[1]
    estsd <- sd(dat$Y)
    ttest <- with(dat, t.test(Y~Group, var.equal=TRUE))
    pow <- power.t.test(sample_size, esteff, estsd)
    return(c(pow=pow$power, p=ttest$p.value,
             sig=ttest$p.value<=.05))
}

# there must be a function for this somewhere in R??
geom_mean <- function(x) {
    tot <- 1
    for (i in 1:length(x)) {
        tot <- tot * x[i]
    }
    return(tot)
}

doAttempts <- function(eff_size=.3, number_per_paper=5, number_of_attempts=1000) {
    allstuds <- raply(number_of_attempts,
                      replicate(number_per_paper, gendata(eff=eff_size)))
    # lix is a logical vector, TRUE if all expts were significant
    lix <- apply(allstuds, 1, function(x) {sum(x["sig",])==ncol(x)})
    # now select out only those studies that have been published
    published <- allstuds[lix,,]
    # calculate and return all TESs for published studies
    apply(published, 1,
          function(x) {geom_mean(x["pow",x["sig",]==1])})
}

eff3 <- doAttempts(.3, 4, 1000) # eff size .3, 4 expts per paper, 1000 attempts
eff6 <- doAttempts(.6, 4, 1000) # eff size .6, 4 expts per paper, 1000 attempts
eff9 <- doAttempts(.9, 4, 1000) # eff size .6, 4 expts per paper, 1000 attempts

threshold <- .1

# calc percent falling below threshold
sum(eff3 < threshold) / length(eff3) # eff size 3
sum(eff6 < threshold) / length(eff6) # eff size 6
sum(eff9 < threshold) / length(eff9) # eff size 6
