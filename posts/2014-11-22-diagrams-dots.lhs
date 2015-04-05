---
title: Drawing Random Dots with Haskell
description: A small example of generating SVG files with random dots using Haskell
tags: Haskell
extrahead: 
  <link rel="stylesheet" href="//netdna.bootstrapcdn.com/bootstrap/3.0.0/css/bootstrap.min.css">
  <link rel="stylesheet" href="//blueimp.github.io/Gallery/css/blueimp-gallery.min.css">
  <link rel="stylesheet" href="/assets/css/bootstrap-image-gallery.min.css">
noToc: valueDoesNotMatter
---

My wife and I are in the process of creating a
digital photo album, and to keep true to our wedding theme, we need
dots, many colorful random dots as a background. Each page should look
similar, but not identical, and dots should not be overlapping.

I guess I have been thinking too much about random numbers and
distributions lately … "If the only tool you have is a
hammer, you tend to see every problem as a nail."

Well, random dots. I could do this by hand (which is not a very
enlightening task, especially if we want to do about 100 photo pages),
or find a little algorithm that does it for me. So below you can find
my approach at generating random dots, where bigger dots are
concentrated in the center.

I used [Haskell](https://haskell.org) and the fascinating
[Diagrams](http://projects.haskell.org/diagrams/) package, also as a
way to explore a library that can directly generate %SVG files. In
fact, this page is actually [literate
Haskell](https://www.haskell.org/haskellwiki/Literate_programming), so
you might copy/pase the text for instance to `dots.lhs` and generate
your own dots with

```bash
runhaskell dots.lhs -o dots.svg -w 800
```

Each time you run the code, a different output is generated. Here are
some examples:

<!-- Image gallery: https://github.com/blueimp/Bootstrap-image-Gallery -->
<div id="links">
<a href="2014-11-22-diagrams-dots/dots1.png" title="Dots 1" data-gallery>
<img src="2014-11-22-diagrams-dots/thumbs/dots1.png" alt="Dots 1">
</a>
<a href="2014-11-22-diagrams-dots/dots2.png" title="Dots 2" data-gallery>
<img src="2014-11-22-diagrams-dots/thumbs/dots2.png" alt="Dots 2">
</a>
<a href="2014-11-22-diagrams-dots/dots3.png" title="Dots 3" data-gallery>
<img src="2014-11-22-diagrams-dots/thumbs/dots3.png" alt="Dots 3">
</a>
<a href="2014-11-22-diagrams-dots/dots4.png" title="Dots 4" data-gallery>
<img src="2014-11-22-diagrams-dots/thumbs/dots4.png" alt="Dots 4">
</a>
<a href="2014-11-22-diagrams-dots/dots5.png" title="Dots 5" data-gallery>
<img src="2014-11-22-diagrams-dots/thumbs/dots5.png" alt="Dots 5">
</a>
<a href="2014-11-22-diagrams-dots/dots6.png" title="Dots 6" data-gallery>
<img src="2014-11-22-diagrams-dots/thumbs/dots6.png" alt="Dots 6">
</a>
<a href="2014-11-22-diagrams-dots/dots7.png" title="Dots 7" data-gallery>
<img src="2014-11-22-diagrams-dots/thumbs/dots7.png" alt="Dots 7">
</a>
<a href="2014-11-22-diagrams-dots/dots8.png" title="Dots 8" data-gallery>
<img src="2014-11-22-diagrams-dots/thumbs/dots8.png" alt="Dots 8">
</a>
<a href="2014-11-22-diagrams-dots/dots9.png" title="Dots 9" data-gallery>
<img src="2014-11-22-diagrams-dots/thumbs/dots9.png" alt="Dots 9">
</a>
<a href="2014-11-22-diagrams-dots/dots10.png" title="Dots 10" data-gallery>
<img src="2014-11-22-diagrams-dots/thumbs/dots10.png" alt="Dots 10">
</a>
</div>


The Actual Code!
----------------

> module Main (main) where
>
> import Data.Colour.SRGB
> import Data.Random.Source
> import Diagrams.Backend.SVG.CmdLine
> import Diagrams.Prelude
> import qualified Data.Random as R

Each dot is defined by a coordinate, a size, and a color.

> data Dot = Dot { _dotCenter :: R2
>                , _radius :: Double
>                , _color :: Colour Double 
>                } deriving Show

Let's allow only certain colors from which we can choose.
                
> colors :: [Colour Double]
> colors = map sRGB24read [
>           "bf3131",
>           "f5b456",
>           "a89178",
>           "615b5b",
>           "add274",
>           "b9a1b9",
>           "f0a2bc",
>           "eb565c",
>           "d15f69",
>           "48bdbe",
>           "f1ede2"]

`randomDot` generates a single dot with a random location, radius, and
color. The location and the radius are drawn from a normal
distribution whose standard distribution is influenced by the `x`
argument. A large `x` results in, on average, small radii and a small
location spread. Therefore, large dots are more likely at the origin
(0,0).

> randomDot :: Double -> R.RVar Dot
> randomDot x = do
>   let mu_rad = 15 * exp (-4 * x)
>       sigmaSq_rad = 0.3 * mu_rad
>       sigmaSq_loc = 8 * exp (2.5*x)
>   locX <- R.sample (R.normal 0 sigmaSq_loc)
>   locY <- R.sample (R.normal 0 sigmaSq_loc)
>   radius <- abs <$> R.sample (R.normal mu_rad sigmaSq_rad)
>   color <- R.sample (R.randomElement colors)
>   return $ Dot (r2 (locX, locY)) radius color

To get non-overlapping dots I do some trial and error: Generate a dot
and compare with all previously generated dots. Only keep the new dot
if it is not too close to any previously generated dots. This approach
is not very efficient, but for 100 dots its just fine.

> randomDots :: [Dot] -> [Double] -> IO [Dot]
> randomDots dots [] = return dots
> randomDots dots (x:xs) = do
>   dot <- R.sample $ randomDot x
>   if any (tooClose dot) dots
>   then randomDots dots (x:xs)
>   else randomDots (dot:dots) xs

The dots should not overlap. Actually, they should be separated a
little, therefore the factor `1.1`.

> tooClose :: Dot -> Dot -> Bool
> tooClose x y = dist < 1.1 * radiusSum
>     where
>       dist = magnitude $ _dotCenter x ^-^ _dotCenter y
>       radiusSum = _radius x + _radius y

Now the dots need to be converted to a diagram.

> fromDot :: Dot -> Diagram B R2
> fromDot c = circle (_radius c) # fc (_color c)
>                                # lw none
>                                # translate (_dotCenter c)
> 
> dotsToDiagram :: [Dot] -> Diagram B R2
> dotsToDiagram = mconcat . map fromDot

Command-line arguments are already implemented by the `Diagrams` package. Nice!

> main :: IO ()
> main = mainWith . dotsToDiagram =<< randomDots [] [0.01, 0.02..1.0]

And we are done …

<!-- The Bootstrap Image Gallery lightbox, should be a child element of the document body -->
<div id="blueimp-gallery" class="blueimp-gallery" data-use-bootstrap-modal="false">
<!-- The container for the modal slides -->
<div class="slides"></div>
<!-- Controls for the borderless lightbox -->
<h3 class="title"></h3>
<a class="prev">‹</a>
<a class="next">›</a>
<a class="close">×</a>
<a class="play-pause"></a>
<ol class="indicator"></ol>
<!-- The modal dialog, which will be used to wrap the lightbox content -->
<div class="modal fade">
<div class="modal-dialog">
<div class="modal-content">
<div class="modal-header">
<button type="button" class="close" aria-hidden="true">&times;</button>
<h4 class="modal-title"></h4>
</div>
<div class="modal-body next"></div>
<div class="modal-footer">
<button type="button" class="btn btn-default pull-left prev">
<i class="glyphicon glyphicon-chevron-left"></i>
Previous
</button>
<button type="button" class="btn btn-primary next">
Next
<i class="glyphicon glyphicon-chevron-right"></i>
</button>
</div>
</div>
</div>
</div>
</div>

<script src="//ajax.googleapis.com/ajax/libs/jquery/1.11.1/jquery.min.js"></script>
<script src="//blueimp.github.io/Gallery/js/jquery.blueimp-gallery.min.js"></script>
<script src="/assets/js/bootstrap-image-gallery.min.js"></script>
