-- Test program (To be moved to testing prior to first release)


module Main where

import FFI (getProduct, getProductType, getLicensedUserEmail,
            getSerialNumberString, getVersionString, setLicense,
            shutdown, getUniqueIdentifierString, getSettingsFileName)
import BinaryView
import Plugin
import Symbol
import Types
import ReferenceSource
import Llil
import Mlil
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8

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
  --let filename = "/usr/bin/file" -- Modify with a valid binary path
  let filename = "TEST.bndb" -- Modify with a valid binary path
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
  --funcs <- functions view
  --mapM_ Function.print funcs
  symbolList <- BinaryView.symbols view
  --mapM_ Symbol.print symbolList
  stringList <- strings view
  --mapM_ Prelude.print stringList
  symCodeRefs <- mapM (Symbol.codeRefs view) symbolList 
  --Prelude.print symCodeRefs
  codeRef <- ReferenceSource.codeRefs view 4462920
  --llils <- mapM Llil.fromRef codeRef
  mlils <- mapM Mlil.fromRef codeRef
  mapM_ Prelude.print $ zip codeRef mlils
  shutdown
  putStrLn "Shutting Down...."

