# binaryninja-haskell
Haskell bindings for binary ninja with (work-in-progress) support for the medium level SSA intermediate language.

## Build Instructions
Symlink the local install of binary ninja to binaryninja-haskell/lib.

For example on MacOS:
```ln -s /Applications/Binary\ Ninja.app/Contents/MacOS lib```

Then build and exec the demo (after changing the path to your license and binary/bndb of interest) with stack.
- stack init
- stack build
- stack exec binaryninja-demo

The demo will be moved into a proper test prior to 1.0 release.


## Version
Currently targeting Binary Ninja Ultimate 5.1.8005-Stable.
Once a 1.0 version is finished this repo will contain tags
for each supported version of binary ninja.

## Format
All haskell files are formatted with https://github.com/tweag/ormolu .
One way to accomplish this is executing: ```ormolu --mode inplace $(find . -name '*.hs')``` in the top level directory.
