---
title: DIY Bayesian Parameter Estimation with the Metropolis Algorithm in Haskell
description: A small example of generating SVG files with random dots using Haskell
tags: Haskell, statistics
extrahead: 
  <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/c3/0.4.8/c3.min.css" type="text/css">
mathjax: on
---
Alright, let's get started!

So, here we have a plot:

<div id="chart_testdata"></div>

And here is a plot about the log-likelihood:

<div id="chart_loglikelihood"></div>

Let's plot trace A:


And here is a histogram and trace of $a$:


<div class="row">
<div class="col-sm-6">
<p class="text-center">Histogram of $a$</p>
<div id="chart_histogramA"></div>
</div>
<div class="col-sm-6">
<p class="text-center">Trace of $a$</p>  
<div id="chart_traceA"></div>               
</div>
</div>

<div class="row">
<div class="col-sm-6">
<p class="text-center">Histogram of $b$</p>
<div id="chart_histogramB"></div>
</div>
<div class="col-sm-6">
<p class="text-center">Trace of $b$</p>  
<div id="chart_traceB"></div>               
</div>
</div>

<div class="row">
<div class="col-sm-6">
<p class="text-center">Histogram of $\sigma$</p>
<div id="chart_histogramSd"></div>
</div>
<div class="col-sm-6">
<p class="text-center">Trace of $\sigma$</p>  
<div id="chart_traceSd"></div>               
</div>
</div>

        



And that's it!


<script src="https://cdnjs.cloudflare.com/ajax/libs/d3/3.5.3/d3.min.js" charset="utf-8"></script>
<script src="https://cdnjs.cloudflare.com/ajax/libs/c3/0.4.8/c3.min.js"></script>

<script>
var chart_testdata = c3.generate({
    bindto: '#chart_testdata',
    data: {
        x: 'x',
        url: '2015-01-17-mcmc-metropolis-in-haskell/test-data.json',
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
        url: '2015-01-17-mcmc-metropolis-in-haskell/log-likelihood.json',
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
            label: 'x',
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
        url: '2015-01-17-mcmc-metropolis-in-haskell/trace' + name + '.json',
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
                values: [0, 1000, 2000, 3000, 4000, 5000, 6000]
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
        url: '2015-01-17-mcmc-metropolis-in-haskell/histogram' + name + '.json',
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
histogram('B', 0, [-6,-5-4,-3,-2, -1, 0,1,2,3,4,5,6]);
histogram('Sd', 10, [0,5,10,15,20]);
trace('A', 5);
trace('B', 0);
trace('Sd', 10);

</script>





> import Control.Monad
> import Control.Monad.State
> import Data.Histogram (asList)
> import Data.Histogram.Fill
> import Data.Histogram.Generic (Histogram)
> import Data.List
> import Data.Random
> import Data.Random.Source.PureMT
> import Graphics.Gnuplot.Simple
> import Statistics.Distribution hiding (mean)
> import Statistics.Distribution.Normal
> import Statistics.Distribution.Uniform
> import Statistics.Sample (mean)
> import Text.JSON
> import qualified Data.Histogram as H
> import qualified Data.Vector.Unboxed as V
>   
> trueA, trueB, trueSd :: Double
> trueA = 5
> trueB = 0
> trueSd = 10
> 
> -- | Predict y from x and parameters
> predict :: Double -> Double-> Double -> Double
> predict a b x = a * x + b
> 
> xs :: [Double]
> xs = take nSamples $ [-15.0 ..]
> 
> -- | Noisy data
> ys :: [Double]
> ys = zipWith (+) noise (map (predict trueA trueB) xs)
>   where noise = evalState (replicateM nSamples (sample (Normal 0.0 trueSd)))
>                 (pureMT $ fromIntegral arbSeed)
> 
> -- | Random proposal values (mean=0)
> normalisedProposals :: Int -> Double -> Int -> [Double]
> normalisedProposals seed sigma nIters =
>   evalState (replicateM nIters (sample (Normal 0.0 sigma)))
>   (pureMT $ fromIntegral seed)
> 
> -- | Sampling from uniform distribution
> acceptOrRejects :: Int -> Int -> [Double]
> acceptOrRejects seed nIters =
>   evalState (replicateM nIters (sample stdUniform))
>   (pureMT $ fromIntegral seed)
> 
> -- | Compute joint log-prior probability density depending on state 
> logprior :: Double -> Double -> Double -> Double
> logprior a b sd = sum . map log $ [ap, bp, sdp]
>   where
>     ap  = density (uniformDistr 0 10) a
>     bp  = density (normalDistr 0 5) b
>     sdp = density (uniformDistr 0 30) sd
> 
> -- | Log-likelihood
> loglikelihood :: Double -> Double -> Double -> Double
> loglikelihood a b sd = sum $ zipWith lh xs ys
>   where lh x = log . density (normalDistr (predict a b x) sd)
> 
> -- | Add log-likelihood and log-prior to get log-posterior
> logposterior :: Double -> Double -> Double -> Double
> logposterior a b sd = loglikelihood a b sd + logprior a b sd
> 
> acceptanceProb :: Double -> Double -> Double -- ^ Proposal parameters
>                -> Double -> Double -> Double -- ^ Old parameters 
>                -> Double
> acceptanceProb a' b' sd' a b sd =
>     exp ((logposterior a' b' sd') - (logposterior a b sd))
> 
> oneStep :: (Double, Double, Double, Int)    -- ^ Current state
>         -> (Double, Double, Double, Double) -- ^ Delta state and acceptance prob.
>         -> (Double, Double, Double, Int)    -- ^ New state
> oneStep (a, b, sd, nAccs) (da, db, dsd, acceptOrReject) =
>   if acceptOrReject < acceptanceProb (a + da) (b + db) (sd + dsd) a b sd
>   then (a + da, b + db, sd + dsd, nAccs + 1)
>   else (a, b, sd, nAccs)
> 
> test :: Int -> [(Double, Double, Double, Int)]
> test seed =
>   drop burnIn $
>   scanl oneStep (startA, startB, startSd, 0) $
>   zip4 (normalisedProposals seed 0.3 nIters)    
>     (normalisedProposals (seed+999) 1.5 nIters) 
>     (normalisedProposals (seed+1) 2.0 nIters )  
>     (acceptOrRejects (seed+2) nIters)
> 
> -- | Arbitrary seed for random number generator
> arbSeed :: Int
> arbSeed = 4022
> 
> nIters :: Int
> nIters = 10000
> 
> burnIn :: Int
> burnIn = 5000
> 
> nSamples :: Int
> nSamples = 31
> 
> startA, startB, startSd :: Double
> startA = 4.0
> startB = 0.0
> startSd = 10.0
> 
> get0 (x,_,_,_) = x
> get1 (_,x,_,_) = x
> get2 (_,_,x,_) = x
> get3 (_,_,_,x) = x
> 
> numBins = 121 :: Int
> 
> hb :: Double -> Double -> HBuilder Double (Data.Histogram.Generic.Histogram V.Vector H.BinD Double)
> hb lower upper = forceDouble -<< mkSimple (H.binD lower numBins upper)
> 
> reportResult :: V.Vector Double -> String -> Double -> Double -> IO ()
> reportResult trace name lower upper = do
>   let hist = fillBuilderVec (hb lower upper) trace 
>   putStrLn $ "Trace mean: " ++ show (mean trace)
>   mapM_ print (V.toList $ V.slice (V.length trace - 10) 8 trace)
>   let fn1 = concat ["histogram_", name, ".png" ]
> 
>   plotPathStyle [ Title ( "Histogram for " ++ name )
>                 , PNG fn1 ]
>                 PlotStyle { plotType=Boxes
>                           , lineSpec=DefaultStyle 1 }
>                 (asList hist)
>   putStrLn ("Created plot " ++ fn1)
> 
>   let fn2 = concat ["trace_", name, ".png" ]
>   plotList [ Title ( "Trace for " ++ name )
>            , PNG fn2 ]
>            (V.toList trace)
>
> exportTrace :: V.Vector Double -> String -> Double -> Double -> IO ()
> exportTrace trace name lower upper = do
>   let hist = fillBuilderVec (hb lower upper) trace
>   write "histogram" $ [("x", V.toList $ H.binsCenters (H.bins hist)), ("y", V.toList (H.histData hist))]
>   write "trace"     $ [("data", V.toList trace)]
>       where write x = writeFile (concat [ x, name, ".json"]) . encode . toJSObject
>            
> main = do
>   -- Export test data to JSON
>   writeFile "test-data.json" . encode . toJSObject $ [("x", xs), ("y", ys)]
>   writeFile "log-likelihood.json" . encode . toJSObject $
>                 (\x -> [ ("x", x),
>                          ("loglikelihood", map (\a -> loglikelihood a trueB trueSd) x )])
>                 [3, 3.05 .. 7::Double]
>                 
>   -- Plot test data
>   plotPath [ Title "Test Data"
>            , LineStyle 1 [ LineWidth 3 ]
>            , PNG "test-data.png" ]
>            (zip xs ys)
>   plotPathStyle [ Title "Test Data with style"
>                 , PNG "test-data_style.png" ]
>                 PlotStyle { plotType=Points
>                           , lineSpec=DefaultStyle 1 }
>                 (zip xs ys)
>   -- Plot likelihoods
>   plotFunc [ Title "Likelihood A"
>            , PNG "likelihood_A.png"
>            , Grid (Just [""]) ]
>            (linearScale 250 (3, 7)) (\x -> loglikelihood x trueB trueSd)                
>   plotFunc [ Title "Likelihood B"
>            , PNG "likelihood_B.png"
>            , Grid (Just [""]) ]
>            (linearScale 250 (0, 4)) (\x -> loglikelihood trueA x trueSd)               
>   -- plotFunc [ Title "Likelihood Sd"
>   --          , PNG "likelihood_Sd.png"
>   --          , Grid (Just [""]) ]
>   --          (linearScale 250 (5, 15)) (\x -> likelihood trueA trueB x)
> 
>   -- Run MCMC simulation
>   let traceData = unzip4 $ test arbSeed
>       -- Unpack to vectors
>       [traceA, traceB, traceSd] = map V.fromList [get0 traceData, get1 traceData, get2 traceData]
>   reportResult traceA "A" 3 7
>   reportResult traceB "B" (-2) 2
>   reportResult traceSd "Sd" 5 15
>
>   exportTrace traceA "A" 3 7
>   exportTrace traceB "B" (-6) 6
>   exportTrace traceSd "Sd" 0 20
>                
>   let nAccepts = last (get3 traceData)
>   putStrLn $ concat [ "nIter: "
>                     , show nIters
>                     , "  accepts: "
>                     , show nAccepts
>                     , "  ratio: "
>                     , show (fromIntegral nAccepts / fromIntegral nIters)
>                     ] 
         
