{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Main (main) where

import Control.Monad
import Data.Aeson (encodeFile)
import GHC.IO.Encoding (setLocaleEncoding)
import System.FilePath
import System.Exit
import System.IO

import Eventlog.Args (args, Args(..))
import Eventlog.HtmlTemplate
import Eventlog.Data
import Eventlog.Types
import Eventlog.Ticky

main :: IO ()
main = do
  -- This fixes a problem for Windows users: https://serokell.io/blog/haskell-with-utf8
  setLocaleEncoding utf8
  a <- args
  when (null (files a)) exitSuccess
  argsToOutput a


argsToOutput :: Args -> IO ()
argsToOutput a@Args{files = files', outputFile = Nothing} =
  if | json a    -> forM_ files' $ \file -> doOneJson a file (file <.> "json")
     | otherwise -> forM_ files' $ \file -> doOneHtml a file (file <.> "html")
argsToOutput a@Args{files = [fin], outputFile = Just fout} =
  if | json a    -> doOneJson a fin fout
     | otherwise -> doOneHtml a fin fout
argsToOutput _ =
  die "When the -o option is specified, exactly one eventlog file has to be passed."

doOneJson :: Args -> FilePath -> FilePath -> IO ()
doOneJson a fin fout = do
  HeapProfile (_, val, _, _) <- generateJson fin a
  encodeFile fout val

doOneHtml :: Args -> FilePath -> FilePath -> IO ()
doOneHtml a fin fout = do
  prof_type <- generateJsonValidate checkTraces fin a
  let html = case prof_type of
                HeapProfile (header, data_json, descs, closure_descs) ->
                 templateString header data_json descs closure_descs a
                TickyProfile (header, tallocs, ticked_per, dat) ->
                  tickyTemplateString header tallocs ticked_per dat a
  writeFile fout html
  where
    checkTraces :: ProfData -> IO ()
    checkTraces dat =
      if length (profTraces dat) > 1000
        then hPutStrLn stderr
              "More than 1000 traces, consider reducing using -i or -x"
        else return ()

