{-# LANGUAGE OverloadedStrings #-}
module Proc00001 where

import           Control.Applicative      ((*>))
import           Control.Concurrent.Async (Concurrently (..))
import           Data.Conduit             (await, yield, ($$), (=$))
import qualified Data.Conduit.Binary      as CB
import qualified Data.Conduit.List        as CL
import           Data.Conduit.Process     (ClosedStream (..), streamingProcess,
                                           proc, waitForStreamingProcess)
import           System.IO                (stdin)

main :: IO ()
main = do
    putStrLn "Enter lines of data. I'll base64-encode it."
    putStrLn "Enter \"quit\" to exit."

    ((toProcess, close), fromProcess, ClosedStream, cph) <-
        streamingProcess (proc "base64" [])

    let input = CB.sourceHandle stdin
             $$ CB.lines
             =$ inputLoop
             =$ toProcess

        inputLoop = do
            mbs <- await
            case mbs of
                Nothing -> close
                Just "quit" -> close
                Just bs -> do
                    yield bs
                    inputLoop

        output = fromProcess $$ CL.mapM_
            (\bs -> putStrLn $ "from process: " ++ show bs)

    ec <- runConcurrently $
        Concurrently input *>
        Concurrently output *>
        Concurrently (waitForStreamingProcess cph)

    putStrLn $ "Process exit code: " ++ show ec
