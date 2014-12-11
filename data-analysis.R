library(ggplot2)

source('data-analysis-functions.R')

match.wcs.languages <- function() {
  cat("Matching WCS languages with WCS languages (may take a while)...\n")
  wcs.wcs.match.ari <<- match.all.languages(wcs.modes, wcs.modes, metric='ARI', na.imputate=TRUE)
  wcs.wcs.match.ari.rounded <<- round(wcs.wcs.match.ari, digits=2)
  wcs.wcs.match.ari.rounded.no.diag <<- wcs.wcs.match.ari.rounded
  diag(wcs.wcs.match.ari.rounded.no.diag) <<- NA
  wcs.wcs.match.ari.rounded.quantiles <<- quantile(wcs.wcs.match.ari.rounded[upper.tri(wcs.wcs.match.ari.rounded)], probs=0:100/100)
  
}

read.simulations <- function() {
  cat("Reading all simulation results...\n")
  sim.modes <<- data.frame()
  sim.modes <<- rbind(sim.modes, read.simulation("results/Regier3.csv", names.prefix='RKK-3.'))
  sim.modes <<- rbind(sim.modes, read.simulation("results/Regier4.csv", names.prefix='RKK-4.'))
  sim.modes <<- rbind(sim.modes, read.simulation("results/Regier5.csv", names.prefix='RKK-5.'))
  sim.modes <<- rbind(sim.modes, read.simulation("results/Regier6.csv", names.prefix='RKK-6.'))

  sim.modes <<- rbind(sim.modes, read.simulation("results/Regier2014-3.csv", names.prefix='RKeK-3.'))
  sim.modes <<- rbind(sim.modes, read.simulation("results/Regier2014-4.csv", names.prefix='RKeK-4.'))
  sim.modes <<- rbind(sim.modes, read.simulation("results/Regier2014-5.csv", names.prefix='RKeK-5.'))
  sim.modes <<- rbind(sim.modes, read.simulation("results/Regier2014-6.csv", names.prefix='RKeK-6.'))
  
  sim.modes <<- rbind(sim.modes, read.simulation("results/simulations-3.csv", names.prefix='COM-3.'))
  sim.modes <<- rbind(sim.modes, read.simulation("results/simulations-4.csv", names.prefix='COM-4.'))
  sim.modes <<- rbind(sim.modes, read.simulation("results/simulations-5.csv", names.prefix='COM-5.'))
  sim.modes <<- rbind(sim.modes, read.simulation("results/simulations-6.csv", names.prefix='COM-6.'))
  
  sim.modes <<- rbind(sim.modes, read.simulation("results/new-simulations-flat-priors-3.csv", names.prefix='Spec-3.'))
  sim.modes <<- rbind(sim.modes, read.simulation("results/new-simulations-flat-priors-4.csv", names.prefix='Spec-4.'))
  sim.modes <<- rbind(sim.modes, read.simulation("results/new-simulations-flat-priors-5.csv", names.prefix='Spec-5.'))
  sim.modes <<- rbind(sim.modes, read.simulation("results/new-simulations-flat-priors-6.csv", names.prefix='Spec-6.'))
  
#   sim.modes <<- rbind(sim.modes, read.simulation("results/new-simulations-McGill-no-smoothing-3.csv", names.prefix='SpecMcGill-3.'))
#   sim.modes <<- rbind(sim.modes, read.simulation("results/new-simulations-McGill-no-smoothing-4.csv", names.prefix='SpecMcGill-4.'))
#   sim.modes <<- rbind(sim.modes, read.simulation("results/new-simulations-McGill-no-smoothing-5.csv", names.prefix='SpecMcGill-5.'))
#   sim.modes <<- rbind(sim.modes, read.simulation("results/new-simulations-McGill-no-smoothing-6.csv", names.prefix='SpecMcGill-6.'))
  
#   sim.modes <<- rbind(sim.modes, read.simulation("results/new-simulations-McGill-some-smoothing-3.csv", names.prefix='SpecMcGillSmoothed-3.'))
#   sim.modes <<- rbind(sim.modes, read.simulation("results/new-simulations-McGill-some-smoothing-4.csv", names.prefix='SpecMcGillSmoothed-4.'))
#   sim.modes <<- rbind(sim.modes, read.simulation("results/new-simulations-McGill-some-smoothing-5.csv", names.prefix='SpecMcGillSmoothed-5.'))
#   sim.modes <<- rbind(sim.modes, read.simulation("results/new-simulations-McGill-some-smoothing-6.csv", names.prefix='SpecMcGillSmoothed-6.'))

  sim.modes <<- rbind(sim.modes, read.simulation("results/new/Munsell-3.csv", names.prefix='newCOM-3.'))
  sim.modes <<- rbind(sim.modes, read.simulation("results/new/Munsell-4.csv", names.prefix='newCOM-4.'))
  sim.modes <<- rbind(sim.modes, read.simulation("results/new/Munsell-5.csv", names.prefix='newCOM-5.'))
  sim.modes <<- rbind(sim.modes, read.simulation("results/new/Munsell-6.csv", names.prefix='newCOM-6.'))
  
  sim.modes <<- rbind(sim.modes, read.simulation("results/new/spectrum-flat-3.csv", names.prefix='newSpec-3.'))
  sim.modes <<- rbind(sim.modes, read.simulation("results/new/spectrum-flat-4.csv", names.prefix='newSpec-4.'))
  sim.modes <<- rbind(sim.modes, read.simulation("results/new/spectrum-flat-5.csv", names.prefix='newSpec-5.'))
  sim.modes <<- rbind(sim.modes, read.simulation("results/new/spectrum-flat-6.csv", names.prefix='newSpec-6.'))
  
  sim.modes <<- rbind(sim.modes, read.simulation("results/new/spectrum-McGill-smoothing-10-3.csv", names.prefix='newSpecMcGillSmoothed-3.'))
  sim.modes <<- rbind(sim.modes, read.simulation("results/new/spectrum-McGill-smoothing-10-4.csv", names.prefix='newSpecMcGillSmoothed-4.'))
  sim.modes <<- rbind(sim.modes, read.simulation("results/new/spectrum-McGill-smoothing-10-5.csv", names.prefix='newSpecMcGillSmoothed-5.'))
  sim.modes <<- rbind(sim.modes, read.simulation("results/new/spectrum-McGill-smoothing-10-6.csv", names.prefix='newSpecMcGillSmoothed-6.'))

}

process.simulations <- function() {
  cat("Matching simulation results with WCS languages (may take a while)...\n")
  sim.wcs.match.ari <<- match.all.languages(sim.modes, wcs.modes, metric='ARI', na.imputate=TRUE)
  sim.wcs.match.ari.rounded <<- round(sim.wcs.match.ari, digits=2)
}

width <- 8
height <- 5

generate.distribution.plots <- function() {
  rng <- extendrange(sim.wcs.match.ari.rounded)
  ggsave('RKK-results-similarities.png', width=width, height=height, 
         plot.scenario.similarities(sim.wcs.match.ari.rounded, 'RKK', '', additional.prefix='RKK-') + ylim(rng) +
          geom_hline(yintercept=wcs.wcs.match.ari.rounded.quantiles[76], colour='yellow') +
          geom_hline(yintercept=wcs.wcs.match.ari.rounded.quantiles[81], colour='orange') +
          geom_hline(yintercept=wcs.wcs.match.ari.rounded.quantiles[86], colour='red') +
          geom_hline(yintercept=wcs.wcs.match.ari.rounded.quantiles[91], colour='black')
  )
  for (prefix in c('COM-','Spec-','SpecMcGill-','SpecMcGillSmoothed-')) {
    for (t in 3:6) {
      ggsave(paste(sep='', prefix, t, '-results-similarities.png'), width=width, height=height,
             plot.scenario.similarities(sim.wcs.match.ari.rounded, prefix, t, additional.prefix='RKK-') + ylim(rng) +
               geom_hline(yintercept=wcs.wcs.match.ari.rounded.quantiles[76], colour='yellow') +
               geom_hline(yintercept=wcs.wcs.match.ari.rounded.quantiles[81], colour='orange') +
               geom_hline(yintercept=wcs.wcs.match.ari.rounded.quantiles[86], colour='red') +
               geom_hline(yintercept=wcs.wcs.match.ari.rounded.quantiles[91], colour='black')
      )
    }
  }
  #ggplot(rbind(m1, m2[grep('RKK-', m2$Var2),]), aes(Var1,value, color=origin)) + geom_point() + coord_flip()
}

generate.rkk.plots <- function() {
  rng <- extendrange(wcs.wcs.match.ari.rounded.no.diag)
  ggsave('RKK-3-similarity-in-context.png', width=width, height=height, 
         plot.sim.in.context(wcs.wcs.match.ari.rounded.no.diag, c('Wobé','Ejagam','Bauzi','Bété'),
                             sim.wcs.match.ari.rounded, 'RKK-3.1') + ylim(rng)
  )
  ggsave('RKK-4-similarity-in-context.png', width=width, height=height, 
         plot.sim.in.context(wcs.wcs.match.ari.rounded.no.diag, c('Wobé','Colorado','Bauzi','Culina'),
                             sim.wcs.match.ari.rounded, 'RKK-4.1') + ylim(rng)
  )
  ggsave('RKK-5-similarity-in-context.png', width=width, height=height, 
         plot.sim.in.context(wcs.wcs.match.ari.rounded.no.diag, c('Bauzi','Colorado','Múra Pirahá','Iduna','Cayapa'),
                             sim.wcs.match.ari.rounded, 'RKK-5.1') + ylim(rng)
  )
  ggsave('RKK-6-similarity-in-context.png', width=width, height=height, 
         plot.sim.in.context(wcs.wcs.match.ari.rounded.no.diag, c('Bauzi','Colorado','Ocaina','Cofán','Buglere'),
                             sim.wcs.match.ari.rounded, 'RKK-6.1') + ylim(rng)
  )
}