module Main where

import Control.Monad (foldM)
import Control.Monad.Except (throwError)
import Data.ByteString.Char8 qualified as SB
import Data.ByteString.Lazy.Char8 qualified as B
import Data.Text qualified as Text
import Data.Text.IO qualified as TIO
import Data.Traversable (for)
import Parser qualified as Parser
import Relude
import Relude.Extra.Foldable1 (maximum1)
import System.Exit (ExitCode (..))
import System.IO (hClose, hSetBinaryMode)
import System.Process (CreateProcess (..), StdStream (..), createPipe, createProcess, shell, waitForProcess)

main :: IO ()
main = do
  args <- getArgs
  case args of
    fileName : _ -> do
      bytes <- decodeUtf8 <$> readFileBS fileName
      case Parser.parse Parser.script bytes of
        Left b -> print b
        Right xs -> do
          ee <- runExceptT $ executeScript xs
          case ee of
            Left err -> TIO.putStrLn err *> exitFailure
            Right out -> traverse_ B.putStrLn out
    [] -> putStrLn "Error: No arguments provided.\nbask requires a file path as an argument. This file should contain a bask script.\nUsage:\n\tbask <filepath>"

type MyMonad = ExceptT Text IO

executeScript :: Parser.Script -> MyMonad [LByteString]
executeScript (Parser.Script pipelines) = fmap concat $ for pipelines $ \p -> executePipeline p mempty

executePipeline :: Parser.Pipeline -> LByteString -> MyMonad [LByteString]
executePipeline (Parser.Pipeline steps) stream = foldM (flip executeStep) (repeat stream) steps

executeStep :: Parser.Step -> [LByteString] -> MyMonad [LByteString]
executeStep (Parser.Step s1) s2 = helper2 (toList s1) s2
  where
    helper2 :: [Parser.Section] -> [LByteString] -> MyMonad [LByteString]
    helper2 [] _ = pure []
    helper2 sections [] = helper2 sections $ repeat mempty
    helper2 (section : sectionRest) streams@(stream : streamRest) =
      case section of
        Parser.SingleInput pipe -> (<>) <$> executePipe pipe stream <*> helper2 sectionRest streamRest
        Parser.Merge (Parser.AtLeastTwo a (b :| rest)) -> do
          let inputs = a :| b : rest
              maxInput =
                maximum1 $ inputs <&> \c -> case c of
                  Parser.JustText _ -> 0
                  Parser.Argument i -> i
          mergeResult <-
            fmap (B.concat . toList) $ for inputs $ \case
              Parser.JustText txt -> pure $ encodeUtf8 txt
              Parser.Argument i -> maybeToExceptT ("Couldn't find input from previous step for merge command for number: " <> show i) $ hoistMaybe $ streams !!? pred i
          (mergeResult :) <$> helper2 sectionRest (drop maxInput streams)
        Parser.Concat n ->
          if n > length streams
            then error $ "concat command required " <> show n <> " inputs, but only " <> show (length streams) <> " provided!"
            else
              let (toConcat, rest) = splitAt n streams
               in fmap (mconcat toConcat :) $ helper2 sectionRest rest

executePipe :: Parser.Pipe -> LByteString -> MyMonad [LByteString]
executePipe (Parser.SinglePipe pipeCommand) stream = executePipeCommand pipeCommand stream
executePipe (Parser.SharedPipe (Parser.AtLeastTwo x xs)) stream = concat <$> traverse (flip executePipeCommand stream) (x : toList xs)

executePipeCommand :: Parser.PipeCommand -> LByteString -> MyMonad [LByteString]
executePipeCommand (Parser.PipelineCommand pipeline) stream = executePipeline pipeline stream
executePipeCommand (Parser.SingleCommand command) stream = one <$> executeCommand command stream

executeCommand :: Parser.Command -> LByteString -> MyMonad LByteString
executeCommand (Parser.ReadLine prompt) _ = lift (TIO.putStr $ prompt <> " ") >> hFlush stdout >> helper
  where
    helper = do
      bytes <- B.fromStrict <$> lift SB.getLine
      if B.null bytes
        then helper
        else pure bytes
executeCommand Parser.PassThru s = pure s
executeCommand (Parser.WriteLine text) stream =
  lift (TIO.putStrLn text) $> stream
executeCommand (Parser.WriteFile path) stream = lift (writeFileLBS (toString path) stream) $> mempty
executeCommand (Parser.AppendFile path) stream = lift (appendFileLBS (toString path) stream) $> mempty
executeCommand (Parser.ShowOutput cmd) stream = executeExternalCommand cmd stream True
executeCommand (Parser.Command cmd) stream = executeExternalCommand cmd stream False

executeExternalCommand :: Parser.ExternalCommand -> LByteString -> Bool -> MyMonad LByteString
executeExternalCommand (Parser.ExternalCommand splits) stream isShow = do
  arguments <- toText <<$>> lift getArgs
  cmd <- fmap (Text.concat . toList) $ for splits $ \case
    Parser.JustText t -> pure t
    Parser.Argument i -> maybeToExceptT ("Couldn't find external argument for number: " <> show i) $ hoistMaybe $ arguments !!? i
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
      outBytes <-
        if isShow
          then do
            lift $ B.hGetContents out >>= B.putStr
            pure mempty
          else lift $ B.hGetContents out

      exitCode <- lift $ waitForProcess procHandle
      case exitCode of
        ExitSuccess -> pure outBytes
        ExitFailure _ -> throwError . decodeUtf8 $ errBytes <> outBytes
    _ -> throwError "Failed to create output handle"
