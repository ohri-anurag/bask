module Main where

import Control.Monad (foldM)
import Control.Monad.Except (throwError)
import Data.ByteString.Char8 qualified as SB
import Data.ByteString.Lazy.Char8 qualified as B
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as Text
import Data.Text.IO qualified as TIO
import Data.Traversable (for)
import Parser qualified as Parser
import Relude
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
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
            Right out -> traverse_ (B.putStrLn . fst) out
    [] -> putStrLn "Error: No arguments provided.\nbask requires a file path as an argument. This file should contain a bask script.\nUsage:\n\tbask <filepath>"

zipWithMN :: (Applicative m) => (a -> b -> m c) -> NonEmpty a -> NonEmpty b -> m (NonEmpty c)
zipWithMN f (a :| as) (b :| bs) = (:|) <$> f a b <*> zipWithM f as bs

type MyMonad = ExceptT Text IO

type Input = (LByteString, Maybe Text)

type Arguments = NonEmpty Input

type Output = Input

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
executeCommand (Parser.ReadLine prompt) (_, dir) _ = lift (TIO.putStr $ prompt <> " ") >> hFlush stdout >> helper
  where
    helper = do
      bytes <- B.fromStrict <$> lift SB.getLine
      if B.null bytes
        then helper
        else pure (bytes, dir)
executeCommand Parser.PassThru input _ = pure input
executeCommand (Parser.WriteLine text) input _ =
  lift (TIO.putStrLn text) $> input
executeCommand (Parser.WriteFile path) (input, dir) _ = lift (writeFileLBS (foldMap toString dir </> toString path) input) $> (mempty, dir)
executeCommand (Parser.AppendFile path) (input, dir) _ = lift (appendFileLBS (foldMap toString dir </> toString path) input) $> (mempty, dir)
executeCommand (Parser.ShowOutput cmd) input args = executeExternalCommand cmd input args True
executeCommand (Parser.Command cmd) input args = executeExternalCommand cmd input args False
executeCommand (Parser.Concat (Parser.AtLeastTwo a (b :| rest))) (_, dir) args = createCmd (a :| b : rest) args <&> \o -> (o, dir)
executeCommand (Parser.ChangeDir pathInput) (input, _) args = do
  path <- createCmd (pathInput :| []) args
  pure (input, Just . Text.strip $ decodeUtf8 path)
executeCommand (Parser.If cond th el) (_, dir) args = case cond of
  Parser.Equals a b -> do
    a' <- createCmd (a :| []) args
    b' <- createCmd (b :| []) args
    res <- if a' == b' then createCmd th args else createCmd el args
    pure (res, dir)
  Parser.Exists a -> do
    branch <-
      lift
        ( runExceptT (createCmd (a :| []) args) >>= \case
            Right _ -> pure th
            Left _ -> pure el
        )
    res <- createCmd branch args
    pure (res, dir)

executeExternalCommand :: Parser.ExternalCommand -> Input -> Arguments -> Bool -> MyMonad Output
executeExternalCommand (Parser.ExternalCommand commandTexts) (input, dir) internal isShow = do
  cmd <- createCmd commandTexts internal
  (mStdin, mStdout, mStderr, procHandle) <- lift $ do
    (readEnd, writeEnd) <- createPipe
    hSetBinaryMode writeEnd True
    B.hPut writeEnd input
    hClose writeEnd

    createProcess
      (shell $ decodeUtf8 cmd)
        { std_in = UseHandle readEnd,
          std_out = if isShow then UseHandle stdout else CreatePipe,
          std_err = CreatePipe,
          cwd = toString <$> dir
        }
  case (mStdin, mStdout, mStderr) of
    (Nothing, Just out, Just err) -> do
      lift $ do
        hSetBinaryMode out True
        hSetBinaryMode err True

      errBytes <- lift $ B.hGetContents err
      outBytes <- lift $ B.hGetContents out

      exitCode <- lift $ waitForProcess procHandle
      case exitCode of
        ExitSuccess -> pure (outBytes, dir)
        ExitFailure _ -> throwError . decodeUtf8 $ errBytes <> outBytes
    (Nothing, Nothing, Just err) -> do
      lift $ hSetBinaryMode err True

      errBytes <- lift $ B.hGetContents err

      exitCode <- lift $ waitForProcess procHandle
      case exitCode of
        ExitSuccess -> pure (mempty, dir)
        ExitFailure _ -> throwError . decodeUtf8 $ errBytes
    _ -> throwError "Failed to create output handle"

createCmd :: NonEmpty Parser.CommandText -> Arguments -> MyMonad LByteString
createCmd commandTexts internal = do
  let internalArgs = fst <$> internal
  arguments <- lift getArgs
  flip foldMapM commandTexts $ \case
    Parser.JustText t -> pure $ encodeUtf8 t
    Parser.ExternalArgument i -> maybeToExceptT ("Couldn't find external argument for number: " <> show i) $ hoistMaybe $ fmap encodeUtf8 $ arguments !!? i
    Parser.PipeArgument i -> maybeToExceptT ("Couldn't find internal argument for number: " <> show i) $ hoistMaybe $ toList internalArgs !!? pred i
