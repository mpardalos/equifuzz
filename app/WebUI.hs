{-# LANGUAGE QuasiQuotes #-}

module WebUI (runWebUI) where

import Control.Concurrent (Chan)
import Data.String.Interpolate (i, __i)
import Experiments (ExperimentProgress)
import Web.Scotty (get, html, scotty)

runWebUI :: Chan ExperimentProgress -> IO ()
runWebUI progressChan = scotty 8888 $ do
  get "/" $ do
    html
      [__i|
          <html>
          <head>
              <title>Equifuzz</title>
          </head>
          <body>
              Lots of crazy bugs found here
          </body>
          </html>
          |]
