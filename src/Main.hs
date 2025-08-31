module Main where

import Control.Monad (foldM)
import Control.Monad.Except (throwError)
import Data.ByteString.Char8 qualified as SB
import Data.ByteString.Lazy.Char8 qualified as B
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text.IO qualified as TIO
import Data.Traversable (for)
import Parser qualified as Parser
import Relude
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

zipWithMN :: (Applicative m) => (a -> b -> m c) -> NonEmpty a -> NonEmpty b -> m (NonEmpty c)
zipWithMN f (a :| as) (b :| bs) = (:|) <$> f a b <*> zipWithM f as bs

type MyMonad = ExceptT Text IO

type Input = LByteString

type Arguments = NonEmpty LByteString

type Output = LByteString

executeScript :: Parser.Script -> MyMonad Arguments
executeScript (Parser.Script pipelines) = sconcat <$> traverse executePipeline pipelines

executePipeline :: Parser.Pipeline -> MyMonad Arguments
executePipeline (Parser.Pipeline steps) = foldM (flip executeStep) (NonEmpty.repeat mempty) steps

executeStep :: Parser.Step -> Arguments -> MyMonad Arguments
executeStep (Parser.Step sections) args =
  let args' = args <> NonEmpty.repeat mempty
   in sconcat <$> zipWithMN (\s a -> executeSection s a args') sections args'

executeSection :: Parser.Section -> Input -> Arguments -> MyMonad Arguments
executeSection (Parser.Section commands) input args = for commands $ \c -> executeCommand c input args

executeCommand :: Parser.Command -> Input -> Arguments -> MyMonad Output
executeCommand (Parser.ReadLine prompt) _ _ = lift (TIO.putStr $ prompt <> " ") >> hFlush stdout >> helper
  where
    helper = do
      bytes <- B.fromStrict <$> lift SB.getLine
      if B.null bytes
        then helper
        else pure bytes
executeCommand Parser.PassThru input _ = pure input
executeCommand (Parser.WriteLine text) input _ =
  lift (TIO.putStrLn text) $> input
executeCommand (Parser.WriteFile path) input _ = lift (writeFileLBS (toString path) input) $> mempty
executeCommand (Parser.AppendFile path) input _ = lift (appendFileLBS (toString path) input) $> mempty
executeCommand (Parser.ShowOutput cmd) input args = executeExternalCommand cmd input args True
executeCommand (Parser.Command cmd) input args = executeExternalCommand cmd input args False
executeCommand (Parser.Concat (Parser.AtLeastTwo a (b :| rest))) _ args = createCmd (a :| b : rest) args

executeExternalCommand :: Parser.ExternalCommand -> Input -> Arguments -> Bool -> MyMonad LByteString
executeExternalCommand (Parser.ExternalCommand commandTexts) input internal isShow = do
  cmd <- createCmd commandTexts internal
  (mStdin, mStdout, mStderr, procHandle) <- lift $ do
    (readEnd, writeEnd) <- createPipe
    hSetBinaryMode writeEnd True
    B.hPut writeEnd input
    hClose writeEnd

    createProcess (shell $ decodeUtf8 cmd) {std_in = UseHandle readEnd, std_out = CreatePipe, std_err = CreatePipe}
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

createCmd :: NonEmpty Parser.CommandText -> Arguments -> MyMonad LByteString
createCmd commandTexts internal = do
  arguments <- lift getArgs
  flip foldMapM commandTexts $ \case
    Parser.JustText t -> pure $ encodeUtf8 t
    Parser.ExternalArgument i -> maybeToExceptT ("Couldn't find external argument for number: " <> show i) $ hoistMaybe $ fmap encodeUtf8 $ arguments !!? i
    Parser.PipeArgument i -> maybeToExceptT ("Couldn't find internal argument for number: " <> show i) $ hoistMaybe $ toList internal !!? pred i
