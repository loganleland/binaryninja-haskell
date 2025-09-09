module Main where

import Binja.BinaryView
import Binja.Function
import Binja.FFI (shutdown)

main = do
  let filename = "/Users/leland/projects/binaryninja-haskell/FaceTime"
  let options = "{\"analysis.mode\": \"intermediate\", \"analysis.limits.maxFunctionSize\": 0}"
  view <- load filename options
  -- Get functions by name
  funcsByName <- Binja.BinaryView.functionsByName view "-[PhoneViewController _prepareForLoadView]"
  mapM_ Binja.Function.print funcsByName
  -- Get function by address
  funcsByAddr <- Binja.BinaryView.functionsAt view 4294992020
  mapM_ Binja.Function.print funcsByAddr
  -- Get functions containing address
  funcsByContain <- Binja.BinaryView.functionsContaining view 4294992020
  mapM_ Binja.Function.print funcsByName
  shutdown
