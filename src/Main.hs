-- Test program (To be moved to testing prior to first release)

module Main where

import BinaryView
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import FFI
  ( getLicensedUserEmail,
    getProduct,
    getProductType,
    getSerialNumberString,
    getSettingsFileName,
    getUniqueIdentifierString,
    getVersionString,
    setLicense,
    shutdown,
  )
import qualified Function
import Llil
import Mlil
import Plugin
import ReferenceSource
import Symbol
import Types

main :: IO ()
main = do
  licenseData <- BS.readFile "/Users/leland/Downloads/license.txt"
  setLicense $ BS8.unpack licenseData
  productStr <- getProduct
  putStrLn $ "Product: " ++ productStr
  productTypeStr <- getProductType
  putStrLn ("Product Type: " ++ productTypeStr)
  email <- getLicensedUserEmail
  putStrLn ("Email: " ++ email)
  serialNumberStr <- getSerialNumberString
  putStrLn ("Serial Number: " ++ serialNumberStr)
  versionStr <- getVersionString
  putStrLn ("Version: " ++ versionStr)
  uniqID <- getUniqueIdentifierString
  putStrLn ("uniq ID: " ++ uniqID)
  userDir <- getUserDirectory
  putStrLn ("user directory: " ++ userDir)
  settingsFilename <- getSettingsFileName
  putStrLn ("getSettingsFileName: " ++ settingsFilename)
  installDirectory <- getInstallDirectory
  putStrLn ("getInstallDirectory : " ++ installDirectory)
  -- Test BNLoadFileName
  let filename = "/Users/leland/projects/binaryninja-haskell/FaceTime"
  -- let filename = "TEST.bndb" -- Modify with a valid binary path
  let options = "{}" -- Example JSON options
  view <- load filename options
  if view == nullPtr
    then putStrLn "Failed to load binary view."
    else putStrLn "Binary view loaded successfully."
  resultTEST <- save view "./TEST_file.bndb"
  if resultTEST then putStrLn "[*] SAVED" else putStrLn "[*] Not Saved"
  existFunction <- hasFunctions view
  if existFunction then putStrLn "[*] Has functions." else putStrLn "[*] Has no functions."
  if hasSymbols view then putStrLn "[*] Has symbols." else putStrLn "[*] Has no symbols."

  -- Get functions, mlil ssa version then all mlil ssa instructions in all functions
  -- funcs <- functions view
  -- mlilSSAFuncs <- mapM Function.mlilSSA funcs
  -- mlilSSAInstructions <- mapM Mlil.instructions mlilSSAFuncs
  -- Prelude.print mlilSSAInstructions

  -- Get code refs for address 4462920
  -- codeRef <- ReferenceSource.codeRefs view 4462920
  -- mlils <- mapM Mlil.fromRef codeRef
  -- mapM_ Prelude.print mlils
  --
  --
  allInstructions <- Mlil.instructions view
  mapM_ Prelude.print allInstructions
  shutdown
  putStrLn "Shutting Down...."
