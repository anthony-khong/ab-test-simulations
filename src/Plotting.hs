module Plotting where

import           Graphics.Rendering.Chart.Backend.Cairo
import           Graphics.Rendering.Chart.Easy
import qualified Numeric.LinearAlgebra.Data             as D
import qualified Statistics.Distribution                as Dist
import qualified Statistics.Distribution.Normal         as Norm

normGrid :: [Double]
normGrid = D.toList $ D.linspace 100 (-3, 3)

normDensityGrid :: [Double]
normDensityGrid = fmap (Dist.density Norm.standard) normGrid

fillFromZero :: [(Double, Double)] -> EC m2 (PlotFillBetween Double Double)
fillFromZero points = liftEC $ do
  plot_fillbetween_style .= solidFillStyle (grey `withOpacity` 0.25)
  plot_fillbetween_values .= fmap (\(x, y) -> (x, (0, y))) points

histogram :: FilePath -> [(Double, Double)] -> IO ()
histogram filename points = toFile def filename $ do
    setColors [opaque black]
    color <- takeColor
    plot (line "Density" [points])
    plot (fillFromZero points)

go = histogram "figures/test.png" (zip normGrid normDensityGrid)
