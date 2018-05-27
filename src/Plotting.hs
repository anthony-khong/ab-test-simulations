module Plotting where

import qualified Data.Random.Distribution.Normal        as DRDN
import qualified Data.Vector                            as V
import           Graphics.Rendering.Chart.Backend.Cairo
import           Graphics.Rendering.Chart.Easy
import qualified Numeric.LinearAlgebra.Data             as LAD
import qualified Sample                                 as S
import qualified Statistics.Distribution                as SD
import qualified Statistics.Distribution.Normal         as SDN
import qualified Statistics.Sample.Histogram            as SSH

normGrid :: [Double]
normGrid = LAD.toList $ LAD.linspace 100 (-3, 3)

normDensityGrid :: [Double]
normDensityGrid = fmap (SD.density SDN.standard) normGrid

fillFromZero :: [(Double, Double)] -> EC m2 (PlotFillBetween Double Double)
fillFromZero points = liftEC $ do
  plot_fillbetween_style .= solidFillStyle (grey `withOpacity` 0.25)
  plot_fillbetween_values .= fmap (\(x, y) -> (x, (0, y))) points

histPlot :: FilePath -> [(Double, Double)] -> IO ()
histPlot filename points = toFile def filename $ do
    setColors [opaque black]
    color <- takeColor
    plot (line "Density" [points])
    plot (fillFromZero points)

pHistPlot :: FilePath -> [Double] -> IO ()
pHistPlot = undefined

histogram :: Int -> [Double] -> [(Double, Double)]
histogram nBins xs = zip (V.toList grid) (V.toList densities)
  where
      (grid, counts) = SSH.histogram nBins (V.fromList xs)
      densities = V.map (\x -> x / fromIntegral (length xs)) counts

xs :: [Double]
xs = take 1000 (S.sample 123 DRDN.StdNormal)

go = histogram 25 xs
