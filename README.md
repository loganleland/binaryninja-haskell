# binaryninja-haskell
Haskell bindings for binary ninja with support for the medium level SSA intermediate language.

## Build Instructions
Symlink binary ninja core shared objects to package root.

For example on MacOS:

- ```ln -s /Applications/Binary\ Ninja.app/Contents/MacOS/libbinaryninjacore.dylib libbinaryninjacore.dylib```
- ```ln -s /Applications/Binary\ Ninja.app/Contents/MacOS/libbinaryninjacore.1.dylib libbinaryninjacore.1.dylib```

Or pass stack the directory where libbinaryninjacore is located.

Then build and exec the demo (after changing the path to your license and binary/bndb of interest) with stack.
- stack init
- stack build --extra-lib-dirs "$(pwd)"
- stack run example --extra-lib-dirs "$(pwd)"

## Branches
This repo tracks supported binja version via branches starting with v5.1.8005-Stable.
Tested and implemented against an ultimate license install.

The main branch tracks the current work-in-progress.

## Format
All haskell files are formatted with https://github.com/tweag/ormolu

## Cookbook

### Getting all functions in a binary
```haskell
module Main where

import Binja.BinaryView
import Binja.Function
import Binja.FFI (shutdown)

main :: IO ()
main = do
  let filename = "/Users/leland/projects/binaryninja-haskell/FaceTime"
  let options = "{\"analysis.mode\": \"intermediate\", \"analysis.limits.maxFunctionSize\": 0}"
  view <- load filename options
  funcs <- Binja.BinaryView.functions view
  mapM_ Binja.Function.print funcs
  Prelude.print $ "Found " ++ show (length funcs) ++ " functions"
  shutdown
```

### Getting all medium level SSA IL instructions
```haskell
module Main where

import Binja.BinaryView
import Binja.Mlil
import Binja.FFI (shutdown)

main :: IO ()
main = do
  let filename = "/Users/leland/projects/binaryninja-haskell/FaceTime"
  let options = "{\"analysis.mode\": \"intermediate\", \"analysis.limits.maxFunctionSize\": 0}"
  view <- load filename options
  mlilSSAs <- Binja.Mlil.instructions view
  Prelude.print $ "Found " ++ show (length mlilSSAs) ++ " mlil ssa instructions"
  shutdown
```

### Getting a specific function
```haskell
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
  singleFunc <- Binja.BinaryView.functionAt view 4294992020
  Binja.Function.print singleFunc
  -- Get functions containing address
  funcsByContain <- Binja.BinaryView.functionsContaining view 4294992020
  mapM_ Binja.Function.print funcsByName
  shutdown
```

### Getting all code refs of address and associated medium level SSA IL instructions
```haskell
module Main where

import Binja.BinaryView
import Binja.ReferenceSource
import Binja.Mlil
import Binja.FFI (shutdown)

main :: IO ()
main = do
  let filename = "/Users/leland/projects/binaryninja-haskell/FaceTime"
  let options = "{\"analysis.mode\": \"intermediate\", \"analysis.limits.maxFunctionSize\": 0}"
  view <- load filename options
  codeRefs' <- Binja.ReferenceSource.codeRefs view 4295938392
  Prelude.print $ "Found " ++ show (length codeRefs') ++ " codeRefs of address 4295938392"
  mlils <- mapM Binja.Mlil.fromRef codeRefs'
  mapM_ Prelude.print mlils
  shutdown
```
