---
title: Bayesian Parameter Estimation with Metropolis MCMC in Haskell
description: A simple linear regression example
tags: Haskell, statistics
extrahead: 
  <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/c3/0.4.8/c3.min.css" type="text/css">
mathjax: on
noToc: valueDoesNotMatter
---

<!-- To compile, set up a cabal sandbox and then, from there, run:
  cabal exec runhaskell ~/path/to/file/2015-01-26-mcmc-metropolis-in-haskell.lhs && cp *.json ~/path/to/posts/doering-site/posts/2015-01-26-mcmc-metropolis-in-haskell/
-->


When you have some measurement values at hand, you will quite often
want to estimate parameters from the data. In principle you now have
the choice between frequentist and Bayesian statistics. I believe that
Bayesian statistics has many advantages, especially with respect to
(%SAR) calibration. This is because parameters are regarded as random
variables, described by a probability distribution. After estimation,
the uncertainty of this estimate is inherently part of the analysis
(in form of the width of the probability distribution), which can then
be used in a subsequent uncertainty analysis to determine the
measurement uncertainty of a measurement result.

I will leave an introduction to Bayesian data analysis to others[^1],
and concentrate here on the implementation of a simple Monte Carlo
Markov chain (MCMC), using the [Metropolis
algorithm](https://en.wikipedia.org/wiki/Metropolis–Hastings_algorithm). The
Metropolis algorithm is maybe the simplest of a range of Markov chain
algorithms, and quite instructive to get your feet wet in the field of
Bayesian simulation.

[^1]: Andrew Gelman, John B. Carlin, Hal S. Stern: *Bayesian Data
Analysis*, 3rd ed. CRC Press 2013.

If you want to get serious with %MCMC, then there are many
full-fledged, efficient %MCMC packages implemented in various
programming languages. I have had good success with [PyMC
2](https://pymc-devs.github.io/pymc/) for Python so far (and I am
really looking forward to the much improved interface of [PyMC
3](https://github.com/pymc-devs/pymc), which is not yet officially
released). The "new kid on the block" seems to be
[STAN](http://mc-stan.org/), whose [Python
interface](https://pystan.readthedocs.org/en/latest/getting_started.html)
is also a pleasure to work with.

But now let's get started, shall we?


Metropolis Monte Carlo Markov Chain in Haskell
----------------------------------------------

Let's look at a simple estimation example: linear regression. The idea
is that we have measured $N$ samples $y_i$, $i = 1 \dots N$, for given
$x_i$. We want to model the data according to
  $$y_i = ax_i + b + e_i,$$
  $$e \sim \mathrm{N}(0, \sigma^2),$$ 
i. e., we want to fit a line through some noisy data. The unknowns
are $a$, $b$, and $\sigma$. Alternatively we could have written 
  $$y \sim \mathrm{N}(ax + b, \sigma^2).$$

The concrete example we are going to look at is inspired by a
[post](https://theoreticalecology.wordpress.com/2010/09/17/metropolis-hastings-mcmc-in-r/)
on an implementation of the Metropolis algorithm in
[R](http://www.r-project.org/). Here I want to implement it using
[Haskell](https://www.haskell.org/), not only to practice the
language, but also to explore how well this compiled language lends
itself to numerical simulations like this. Thanks to [another blog
post](https://idontgetoutmuch.wordpress.com/2014/03/20/bayesian-analysis-a-conjugate-prior-and-markov-chain-monte-carlo/),
it was a surprisingly pleasant experience.

By the way, the [source
file](https://github.com/bdoering/doering-site/blob/master/posts/2015-01-26-mcmc-metropolis-in-haskell.lhs)
of this blog post is a [literate
Haskell](https://www.haskell.org/haskellwiki/Literate_programming)
file which can directly be compiled and executed, should you want to
try it out for yourself!

Let's get started with some imports...

> import Control.Monad
> import Control.Monad.State (evalState)
> import Data.Histogram (asList)
> import Data.Histogram.Fill
> import Data.Histogram.Generic (Histogram)
> import Data.List
> import Data.Random
> import Data.Random.Source.PureMT
> import Statistics.Distribution hiding (mean)
> import Statistics.Distribution.Normal
> import Statistics.Distribution.Uniform
> import Text.JSON
> import qualified Data.Histogram as H
> import qualified Data.Vector.Unboxed as V

Our model relates the known variable $x$ to an output value $y$, as
long as $a$, and $b$ are known:

> predict :: Double -> Double-> Double -> Double
> predict a b x = a * x + b

Having defined this, we can generate some noisy test data now. Later
on we will try to estimate the true values for $a$, $b$, and $\sigma$
from this test data.

> trueA, trueB, trueSd :: Double
> trueA = 5
> trueB = 0
> trueSd = 10
> 
> nSamples :: Int
> nSamples = 31
> 
> xs :: [Double]
> xs = take nSamples $ [-15.0 ..]
>
> arbSeed :: Int
> arbSeed = 4022 -- Arbitrary seed for generating random numbers
> 
> ys :: [Double]
> ys = zipWith (+) noise (map (predict trueA trueB) xs)
>   where noise = evalState (replicateM nSamples (sample (Normal 0.0 trueSd)))
>                 (pureMT $ fromIntegral arbSeed)

In order to include plots in this page, I use the
[C3.js](http://c3js.org/) JavaScript chart library, which can read in
%JSON data. So we export the generated test data as a %JSON file:

> exportTestData :: IO ()
> exportTestData = writeFile "test-data.json" . encode . toJSObject $
>                  [("x", xs), ("y", ys)]

This is what our generated test data looks like, `ys` over `xs`:

<p class="text-center">Generated noisy test data<p>
<div id="chart_testdata"></div>

In order to draw from the posterior, we need to determine the
likelihood function and the prior. Let's start with the
likelihood. The record level likelihood is given as
  $$ p(y_i|a, b, \sigma, x_i) = \frac{1}{\sqrt{2\pi}\sigma} \exp \left[ -\frac{(y_i-(ax_i + b))^2}{2\sigma^2} \right], $$
i. e., the probability of sample $y_i$ under a normal distribution
with mean $a x_i + b$ and variance $\sigma^2$. Each sample $y_i$ is
assumed to be drawn independently, so the model-level likelihood for
the complete sample $y$ is the product of the separate likelihoods, or
$$ p(y|a, b, \sigma, x) = \prod_{i=1}^N \frac{1}{\sqrt{2\pi}\sigma} \exp \left[-\frac{(y_i-(ax_i + b))^2}{2\sigma^2} \right]. $$

In numerical implementations one usually does not compute the
likelihood, but the log-likelihood in order to avoid problems with
small numbers:
  $$  \log [p(y|a, b, \sigma, x)] = \sum_{i=1}^N \frac{1}{2\sigma} \exp \left[ -\frac{(y_i-(ax_i + b))^2}{2\sigma^2} \right]. $$ 
The location of the maximum of the log-likelihood function remains
unchanged.

In Haskell the log-likelihood is:

> loglikelihood :: Double -> Double -> Double -> Double
> loglikelihood a b sd = sum $ zipWith lh xs ys
>   where lh x = logDensity (normalDistr (predict a b x) sd)

As an example, we can plot the log-likelihood $p(y|a, b=0, \sigma=10,
x)$, showing how likely the samples $(x_i, y_i)$, $i=1 \dots N$, are
for varying $a$, a fixed $b=0$, and a fixed $\sigma=10$ (again by
exporting the data to a %JSON file first):

> exportLikelihoodA :: IO ()
> exportLikelihoodA = writeFile "log-likelihood.json" . encode . toJSObject $
>                     (\as -> [ ("x", as),
>                              ("loglikelihood", map (\a -> loglikelihood a trueB trueSd) as )])
>                     [3, 3.05 .. 7::Double]

<p class="text-center">Log-likelihood $p(y|a, b=0, \sigma=10, x)$</p>
<div id="chart_loglikelihood"></div>

We can see that $a=5$ is very close to the maximum. The remaining
offset is a result of the random nature of our test samples $(x_i,
y_i)$.

As for any Bayesian data analysis, we also need a prior. Here we
choose rather uninformative priors which have little influence on the
estimated parameters:

> logprior :: Double -> Double -> Double -> Double
> logprior a b sd = sum [ap, bp, sdp]
>   where
>     ap  = logDensity (uniformDistr 0 10) a
>     bp  = logDensity (normalDistr 0 5) b
>     sdp = logDensity (uniformDistr 0 30) sd

The (unscaled) log-posterior is now given as the sum of the
log-likelihood and the log-prior:

> logposterior :: Double -> Double -> Double -> Double
> logposterior a b sd = loglikelihood a b sd + logprior a b sd

Now we can start to actually implement the Metropolis %MCMC. The
Metropolis algorithm is a random walk algorithm. Each of the

> nIters :: Int
> nIters = 100000

steps in the random walk only depends on the current state (location
in the parameter space) and an acceptance/rejection criterion (which
involves random numbers). To go from the current location $(a, b,
\sigma)$ to a proposed location $(a + \Delta a, b + \Delta b, \sigma +
\Delta\sigma)$, we need to generate random delta values with zero
mean. Let's define a general function for generating a list of random
values from a seed:

> normalisedProposals :: Int -> Double -> Int -> [Double]
> normalisedProposals seed sigma nIters =
>   evalState (replicateM nIters (sample (Normal 0.0 sigma)))
>   (pureMT $ fromIntegral seed)

At each step, the Metropolis algorithm needs to decide if the proposed
jump is accepted, for which random draws from a standard uniform
distribution are required:

> acceptOrRejects :: Int -> Int -> [Double]
> acceptOrRejects seed nIters =
>   evalState (replicateM nIters (sample stdUniform))
>   (pureMT $ fromIntegral seed)

The acceptance probability for accepting the new location over the old
location is then:

> acceptanceProb :: Double -> Double -> Double -- ^ Proposal parameters
>                -> Double -> Double -> Double -- ^ Old parameters 
>                -> Double
> acceptanceProb a' b' sd' a b sd =
>     exp ((logposterior a' b' sd') - (logposterior a b sd))

The core of the Metropolis algorithm is the following. We either
advance to the proposed location or stay at the current location,
depending on the acceptance probability:

> oneStep :: (Double, Double, Double, Int)    -- ^ Current location
>         -> (Double, Double, Double, Double) -- ^ Delta location and acceptance probability
>         -> (Double, Double, Double, Int)    -- ^ New state
> oneStep (a, b, sd, nAccs) (da, db, dsd, acceptOrReject) =
>   if acceptOrReject < acceptanceProb (a + da) (b + db) (sd + dsd) a b sd
>   then (a + da, b + db, sd + dsd, nAccs + 1)
>   else (a, b, sd, nAccs)

The complete Markov `chain` can be generated from a starting location
and a succession of single steps:

> startA, startB, startSd :: Double
> startA = 4  -- ^ All start values are a little off
> startB = 1  --   to show that the algorithm converges
> startSd = 9 --   to the true posterior after burn in
>
> burnIn :: Int
> burnIn = 1000
>
> chain :: Int        -- ^ Seed for generation of random numbers
>       -> [ ( Double -- ^ a
>            , Double -- ^ b
>            , Double -- ^ sigma
>            , Int    -- ^ number of accepted samples
>            ) ]
> chain seed =
>   drop burnIn $
>   scanl oneStep (startA, startB, startSd, 0) $
>   zip4 (normalisedProposals seed 0.3 nIters)    
>     (normalisedProposals (seed+999) 1.5 nIters) 
>     (normalisedProposals (seed+1) 2.0 nIters )  
>     (acceptOrRejects (seed+2) nIters)

The promise of the trace data is that it was drawn from the posterior
distribution. So by looking at the trace of each parameter, we can
directly derive quantities of interest (like an estimate of the
expected value, and an estimate of the variance of this expected
value). Here I want to generate a histogram from the trace data and
export it as a %JSON file for plotting. Additionally I want to export
the last 1000 samples of the trace data to get a visual clue if
consecutive samples are sufficiently uncorrelated.

> hb :: Double -> Double
>    -> HBuilder Double (Data.Histogram.Generic.Histogram V.Vector H.BinD Double)
> hb lower upper = forceDouble -<< mkSimple (H.binD lower numBins upper)
>     where numBins = 121
> 
> -- | Export trace data to JSON file (histogram and last 1000 samples)
> exportTrace :: V.Vector Double -> String -> Double -> Double -> IO ()
> exportTrace trace name lower upper = do
>   let hist = fillBuilderVec (hb lower upper) trace
>   write "histogram" $ [ ("x", V.toList $ H.binsCenters (H.bins hist))
>                       , ("y", V.toList (H.histData hist)) ]
>   write "trace"     $ [("data", V.toList . V.drop (V.length trace - 1000) $ trace)]
>       where write x = writeFile (concat [ x, name, ".json"]) . encode . toJSObject
>
> consumeResults :: ( [Double], [Double], [Double], [Int] ) -> IO ()
> consumeResults (traceA, traceB, traceSd, lAcc) = do
>   exportTrace (V.fromList traceA)  "A" 3 7
>   exportTrace (V.fromList traceB)  "B" (-6) 6
>   exportTrace (V.fromList traceSd) "Sd" 0 20
>   let nAccepts = last lAcc
>   putStrLn $ concat [ "nIter: "
>                     , show nIters
>                     , "  accepts: "
>                     , show nAccepts
>                     , "  ratio: "
>                     , show (fromIntegral nAccepts / fromIntegral nIters)
>                     ] 
> main = do
>   exportTestData
>   exportLikelihoodA
>   consumeResults . unzip4 . chain $ arbSeed
>   return ()        

And here is a histogram and trace of $a$:

<div class="row">
<div class="col-sm-6">
<p class="text-center">Histogram of $a$</p>
<div id="chart_histogramA"></div>
</div>
<div class="col-sm-6">
<p class="text-center">Last 1000 values of trace of $a$</p>  
<div id="chart_traceA"></div>               
</div>
</div>

The histogram (an approximation of the probability density function of
the parameter $a$) nicely summarizes the estimation result. As a
measure of uncertainty, one often considers the 95 % highest
probability density interval (HPDI), but for this demo the analysis
shall stop here.

If you look at the trace, you can see that sometimes it remains at the
same location for a few draws, and sometimes a new location is
accepted right away. This acceptance rate is an important parameter
when adjusting the variance of the jumps (the second parameter of our
`normalisedProposals` function). If the variance is too small, then
the acceptance ratio will be close to 1.0, but the location will
change very slowly so that many random draws are required until the
trace had the chance to sufficiently explore the parameter space. On
the other hand, if the variance is too large then hardly any new
location will be accepted and the trace appears to be stuck. It is
difficult to get the variance of the jump proposals right in the
general case (and this the reason why so many libraries exist which
implement %MCMCs). Here I just tuned the values manually until the
acceptance ratio was around 0.4.

For completeness, here are the histograms and parts of the traces for
the parameters $b$ and $\sigma$:

<div class="row">
<div class="col-sm-6">
<p class="text-center">Histogram of $b$</p>
<div id="chart_histogramB"></div>
</div>
<div class="col-sm-6">
<p class="text-center">Last 1000 values of trace of $b$</p>  
<div id="chart_traceB"></div>               
</div>
</div>

<div class="row">
<div class="col-sm-6">
<p class="text-center">Histogram of $\sigma$</p>
<div id="chart_histogramSd"></div>
</div>
<div class="col-sm-6">
<p class="text-center">Last 1000 values of trace of $\sigma$</p>  
<div id="chart_traceSd"></div>               
</div>
</div>


Conclusion
==========

Implementing the Metropolis %MCMC algorithm in Haskell was a good
learning experience, both from the point of view of Bayesian data
analysis and for programming in Haskell. 

An interesting next step would be to look at more complex statistical
models, considering hierarchical or pooled data. This is an area where
Bayesian data analysis should excel over many Frequentist approaches
because Bayesian models are easier to adapt to more complex
situations. Solving these models in practice is then relatively easy
thanks to the availability of random walk algorithms like the
Metropolis sampler, especially when existing %MCMC libraries are used.


<script src="https://cdnjs.cloudflare.com/ajax/libs/d3/3.5.3/d3.min.js" charset="utf-8"></script>
<script src="https://cdnjs.cloudflare.com/ajax/libs/c3/0.4.9/c3.min.js"></script>

<script>
var chart_testdata = c3.generate({
    bindto: '#chart_testdata',
    data: {
        x: 'x',
        url: '2015-01-26-mcmc-metropolis-in-haskell/test-data.json',
        mimeType: 'json',
        type: 'scatter',
        colors: {
            'y': '#428BCA',
        },
    },
    legend: {
        show: false
    },
    point: {
        r: 5
    },
    axis: {
        x: {label: 'x'},
        y: {label: 'y'},
    }
});

var chart_loglikelihood = c3.generate({
    bindto: '#chart_loglikelihood',
    data: {
        x: 'x',
        url: '2015-01-26-mcmc-metropolis-in-haskell/log-likelihood.json',
        mimeType: 'json',
        colors: {
            'y': '#428BCA',
        },
    },
    legend: {
        show: false
    },
    point: {
        show: false
    },
    axis: {
        x: {
            label: 'a',
            tick: {
                values: [3,4,5,6,7],
            },
        },
        y: {label: 'log(likelihood)'},
    },
    grid: {
        x: {
            lines: [{value: 5, text: 'true A'}]
        },
    },
});

function trace(name, trueVal) {
var chart_traceA = c3.generate({
    bindto: '#chart_trace' + name,
    size: { height: 250 },
    data: {
        url: '2015-01-26-mcmc-metropolis-in-haskell/trace' + name + '.json',
        mimeType: 'json',
        colors: {
            'data': '#428BCA',
        },
    },
    legend: {
        show: false
    },
    point: {
        show: false
    },
    axis: {
        x: {
            label: 'Iteration',
            tick: {
                values: [0, 500, 1000],
            },
        },
        y: {label: 'Trace of ' + name},
    },
    grid: {
        y: {
            lines: [{value: trueVal, text: 'true ' + name}]
        },
    },
});
};

function histogram(name, trueVal, xticks) {
var chart_histogram = c3.generate({
    bindto: '#chart_histogram' + name,
    size: { height: 250 },
    data: {
        x: 'x',
        url: '2015-01-26-mcmc-metropolis-in-haskell/histogram' + name + '.json',
        mimeType: 'json',
        type: 'bar',
        colors: {
            'data': '#428BCA',
        },
    },
    legend: {
        show: false
    },
    axis: {
        x: {
            label: 'x',
            tick: {
                values: xticks,
            },
        },
        y: {label: 'Count'},
    },
    grid: {
        x: {
            lines: [{value: trueVal, text: 'true ' + name}]
        },
    },
    bar: {  
        width: {
            ratio: 1.1
        },
    },
});
};

histogram('A', 5, [3,4,5,6,7]);
histogram('B', 0, [-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6]);
histogram('Sd', 10, [0,5,10,15,20]);
trace('A', 5);
trace('B', 0);
trace('Sd', 10);

</script>


         
