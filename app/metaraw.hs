module Main where

import qualified Image.Raw.LibRaw as LR
import Control.Monad
import Foreign.C
import System.Environment
import Text.Printf

main :: IO ()
main = do
  r <- LR.init
  case r of
    Nothing -> putStrLn "See you"
    Just handle -> do
      files <- getArgs
      (flip mapM_) files $ \f -> do
        errCode <- LR.openFile handle f
        ip <- LR.getIParams handle
        io <- LR.getImgOther handle
        lens <- LR.getLensInfo handle
        -- printf "file,make,model,focallen,aperture,shutter,iso\n"
        printf "%s,%s,%s,%d,%f,%f,%d\n"
          f
          (LR.make ip)
          (LR.model ip)
          (round' (LR.focalLen io))
          (LR.aperture io)
          (LR.shutter io)
          (round' (LR.isoSpeed io))
        LR.recycle handle
  where round' :: Float -> Integer
        round' = round
