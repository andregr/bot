module Bot.Test.TestUtil (
  testGroupGenerator,
  testCase,
  (@?=),
  
  captureStdOut 
  ) where

import Bot.Util                       ( Text )
import Control.Exception              ( bracket )
import Data.Text.Lazy.IO              ( hGetContents )
import GHC.IO.Handle                  ( hDuplicate, hDuplicateTo)
import System.Directory               ( removeFile )
import System.IO                      ( Handle, IOMode(ReadWriteMode), withFile, 
                                        stdout, hFlush,
                                        hSeek, SeekMode(AbsoluteSeek) ) 
import Test.Framework.TH              ( testGroupGenerator )
import Test.Framework.Providers.HUnit ( testCase ) -- Used by TH
import Test.HUnit                     ( (@?=) )


captureStdOut :: IO a -> ((a, Text) -> IO b) -> IO b
captureStdOut action processor = do
  withTempFile $ \file -> do
    a <- redirect stdout file action
    hSeek file AbsoluteSeek 0
    t <- hGetContents file
    processor (a, t)

withTempFile :: (Handle -> IO a) -> IO a
withTempFile action = do
  let tempFilename = "redirected_output"
  a <- withFile tempFilename ReadWriteMode action
  removeFile tempFilename
  return a

redirect :: Handle -> Handle -> IO a -> IO a
redirect source destination action = bracket before after (const action)
  where
    before :: IO Handle
    before = do
      hFlush source
      sourceDup <- hDuplicate source
      hDuplicateTo destination source
      return sourceDup

    after :: Handle -> IO ()
    after sourceDup = do
      hFlush destination
      hDuplicateTo sourceDup source