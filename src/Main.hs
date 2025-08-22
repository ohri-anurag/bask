module Main where

import Control.Monad (foldM)
import Control.Monad.Except (throwError)
import Data.ByteString.Char8 qualified as B
import Data.Text.IO qualified as TIO
import Parser qualified as Parser
import Relude
import System.Exit (ExitCode (..))
import System.IO (hClose, hSetBinaryMode)
import System.Process (CreateProcess (..), StdStream (..), createPipe, createProcess, shell, waitForProcess)
import Text.Megaparsec (parse)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fileName] -> do
      bytes <- decodeUtf8 <$> readFileBS fileName
      case parse Parser.pipeline "example" bytes of
        Left b -> print b
        Right xs -> do
          -- print xs
          ee <- runExceptT $ executePipeline xs B.empty
          whenLeft_ ee print
    [] -> putStrLn "Error: No arguments provided.\nbask requires a file path as an argument. This file should contain a bask script.\nUsage:\n\tbask <filepath>"
    _ -> putStrLn "Error: Too many arguments provided.\nbask requires a single file path as an argument. This file should contain a bask script.\nUsage:\n\tbask <filepath>"

type MyMonad = ExceptT Text IO

executePipeline :: Parser.Pipeline -> ByteString -> MyMonad [ByteString]
executePipeline (Parser.Pipeline steps) stream = foldM (flip executeStep) (repeat stream) steps

executeStep :: Parser.Step -> [ByteString] -> MyMonad [ByteString]
executeStep (Parser.Step s1) s2 = helper2 (toList s1) s2
  where
    -- helper (section :| sectionRest) streams@(stream :| streamRest) =
    --   case section of
    --     Parser.SingleInput pipe -> NonEmpty.appendList (executePipe pipe stream) $ helper2 sectionRest streamRest
    --     Parser.Merge mergeInputs ->
    --       let inputCount = Parser.countInputs mergeInputs
    --           (forMerge, rest) = NonEmpty.splitAt inputCount streams
    --        in helper3 mergeInputs forMerge :| helper2 sectionRest rest
    --     Parser.ReOrder (Parser.AtLeastTwo a rest) ->
    --       let newPositions = a NonEmpty.<| rest
    --        in -- executedStreams = (\s -> SC.mwrap $! fmap (\c -> SC.fromStrict $! seq (rnf c) c) $! SC.toStrict_ $! seq s s) <$> streams
    --           -- executedStreams = seq (traverse (fmap rnf . SC.toStrict_) streams) streams
    --           if length newPositions > length streams
    --             then error "reorder command doesn't have enough streams to re-order!"
    --             else do
    --               bytestrings <- lift $ traverse SC.toLazy_ streams
    --
    --               NonEmpty.appendList
    --                 (fmap (SC.fromLazy . snd) $ NonEmpty.sortBy (comparing fst) $ NonEmpty.zip newPositions bytestrings)
    --                 (helper2 sectionRest $ NonEmpty.drop (length newPositions) streams)

    helper2 :: [Parser.Section] -> [ByteString] -> MyMonad [ByteString]
    helper2 [] _ = pure []
    helper2 sections [] = helper2 sections $ repeat B.empty
    helper2 (section : sectionRest) streams@(stream : streamRest) =
      case section of
        Parser.SingleInput pipe -> (<>) <$> executePipe pipe stream <*> helper2 sectionRest streamRest
        Parser.Merge mergeInputs ->
          let inputCount = Parser.countInputs mergeInputs
              (forMerge, rest) = splitAt inputCount streams
           in do
                resultRest <- helper2 sectionRest rest
                pure $ helper3 mergeInputs forMerge : resultRest
        Parser.ReOrder (Parser.AtLeastTwo a (b :| rest)) ->
          let newPositions = a : b : rest
           in if length newPositions > length streams
                then error "reorder command doesn't have enough streams to re-order!"
                else do
                  resultRest <- helper2 sectionRest (drop (length newPositions) streams)
                  pure
                    $ fmap snd (sortBy (comparing fst) $ zip newPositions streams)
                    <> resultRest
        Parser.Concat n ->
          if n > length streams
            then error $ "concat command required " <> show n <> " inputs, but only " <> show (length streams) <> " provided!"
            else
              let (toConcat, rest) = splitAt n streams
               in fmap (mconcat toConcat :) $ helper2 sectionRest rest

    helper3 mergeInputs@(Parser.AtLeastTwo a (b :| rest)) streams =
      let streamCount = length streams
          inputCount = Parser.countInputs mergeInputs
       in if streamCount < inputCount
            then error $ toText $ "merge command is asking for more inputs than available from previous step!" ++ show mergeInputs
            else mconcat $ helper4 (a : b : rest) streams

    helper4 :: [Parser.MergeInput] -> [ByteString] -> [ByteString]
    helper4 [] [] = []
    helper4 (Parser.JustText text : rest) streams = (encodeUtf8 text) : helper4 rest streams
    helper4 (Parser.InputPipe : rest) (stream : srest) = stream : helper4 rest srest
    helper4 _ _ = error "Panic!"

executePipe :: Parser.Pipe -> ByteString -> MyMonad [ByteString]
executePipe (Parser.SinglePipe pipeCommand) stream = executePipeCommand pipeCommand stream
executePipe (Parser.SharedPipe (Parser.AtLeastTwo x xs)) stream = concat <$> traverse (flip executePipeCommand stream) (x : toList xs)

executePipeCommand :: Parser.PipeCommand -> ByteString -> MyMonad [ByteString]
executePipeCommand (Parser.PipelineCommand pipeline) stream = executePipeline pipeline stream
executePipeCommand (Parser.SingleCommand command) stream = one <$> executeCommand command stream

executeCommand :: Parser.Command -> ByteString -> MyMonad ByteString
executeCommand (Parser.ReadLine prompt) _ = lift (for_ prompt (TIO.putStr . flip (<>) " ")) >> hFlush stdout >> helper
  where
    helper = do
      bytes <- lift B.getLine
      if B.null bytes
        then helper
        else pure bytes
executeCommand Parser.PassThru s = pure s
-- executeCommand (Parser.Lines cmd) stream = SC.unlines $ S.maps (executeExternalCommand cmd) $ SC.denull $ SC.lines stream
executeCommand (Parser.WriteLine text) stream =
  lift (TIO.putStrLn text) $> stream
executeCommand (Parser.WriteFile path) stream = lift (writeFileBS (toString path) stream) $> B.empty
executeCommand (Parser.AppendFile path) stream = lift (appendFileBS (toString path) stream) $> B.empty
executeCommand (Parser.Command cmd) stream = executeExternalCommand cmd stream

executeExternalCommand :: Parser.ExternalCommand -> ByteString -> MyMonad ByteString
executeExternalCommand (Parser.ExternalCommand cmd) stream = do
  (mStdin, mStdout, mStderr, procHandle) <- lift $ do
    (readEnd, writeEnd) <- createPipe
    hSetBinaryMode writeEnd True
    B.hPut writeEnd stream
    hClose writeEnd

    createProcess (shell $ toString cmd) {std_in = UseHandle readEnd, std_out = CreatePipe, std_err = CreatePipe}
  case (mStdin, mStdout, mStderr) of
    (Nothing, Just out, Just err) -> do
      lift $ do
        hSetBinaryMode out True
        hSetBinaryMode err True

      errBytes <- lift $ B.hGetContents err
      outBytes <- lift $ B.hGetContents out
      exitCode <- lift $ waitForProcess procHandle
      case exitCode of
        ExitSuccess -> pure outBytes
        ExitFailure _ -> throwError . decodeUtf8 $ errBytes <> outBytes
    _ -> throwError "Failed to create output handle"
