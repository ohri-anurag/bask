module Parser where

import Control.Monad.Combinators.NonEmpty (sepBy1)
import Data.Char (isDigit, isSpace)
import Data.Text qualified as Text
import Relude hiding (some)
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as M

data AtLeastTwo a = AtLeastTwo a (NonEmpty a)
  deriving (Show, Eq)

newtype Pipeline = Pipeline (NonEmpty Step)
  deriving (Show, Eq)

newtype Step = Step (NonEmpty Section)
  deriving (Show, Eq)

data Section
  = SingleInput Pipe
  | Merge (AtLeastTwo MergeInput)
  | ReOrder (AtLeastTwo Int)
  | Concat Int
  deriving (Show, Eq)

data Pipe
  = SinglePipe PipeCommand
  | SharedPipe (AtLeastTwo PipeCommand)
  deriving (Show, Eq)

data MergeInput
  = JustText Text
  | InputPipe
  deriving (Show, Eq)

data PipeCommand
  = PipelineCommand Pipeline
  | SingleCommand Command
  deriving (Show, Eq)

data Command
  = Command ExternalCommand
  | ReadLine (Maybe Text)
  | WriteLine Text
  | WriteFile Text
  | AppendFile Text
  | PassThru
  deriving
    ( -- | Lines ExternalCommand
      Show,
      Eq
    )

newtype ExternalCommand = ExternalCommand Text
  deriving (Show, Eq)

type Parser = M.Parsec () Text

pipeline :: Parser Pipeline
pipeline =
  Pipeline <$> sepBy1 step (M.char '|' *> M.hspace)

step :: Parser Step
step = Step <$> sepBy1 section (M.hspace *> M.string "||" *> M.hspace) <* M.space

section :: Parser Section
section =
  ( do
      M.string "merge" *> M.hspace
      Merge <$> mergeInput
  )
    <|> ( do
            M.string "reorder" *> M.hspace
            texts <- Text.words . Text.strip <$> M.takeWhile1P (Just "command/writeline") (\c -> c /= '|' && c /= '=')
            case mapMaybe (readMaybe . toString) texts of
              a : b : rest -> pure . ReOrder $ AtLeastTwo a (b :| rest)
              _ -> error "reorder must have at least two arguments!"
        )
    <|> ( do
            M.string "concat" *> M.hspace
            istr <- M.takeWhile1P (Just "command/concat") isDigit
            case readMaybe $ toString istr of
              Just i -> pure $ Concat i
              Nothing -> error . toText $ "concat command requires an integer specifying the number of inputs to concatenate!\nGiven: " <> istr
        )
    <|> (SingleInput <$> pipe)

mergeInput :: Parser (AtLeastTwo MergeInput)
mergeInput =
  M.takeWhile1P (Just "string") (\c -> c /= '|' && c /= '=') >>= \s ->
    case intersperse InputPipe $ fmap JustText $ Text.splitOn "pipe" s of
      a : b : rest -> pure $ AtLeastTwo a (b :| rest)
      _ -> fail "merge command needs at least two inputs!"

pipe :: Parser Pipe
pipe = do
  a :| rest <- sepBy1 pipeCommand (M.string "==" *> M.hspace)
  pure $ case rest of
    b : bs -> SharedPipe $ AtLeastTwo a (b :| bs)
    [] -> SinglePipe a

pipeCommand :: Parser PipeCommand
pipeCommand = SingleCommand <$> command

command :: Parser Command
command = do
  M.takeWhile1P (Just "command/singlecommand") (not . isSpace) >>= \s ->
    case s of
      "writefile" -> M.hspace *> (WriteFile . Text.strip <$> M.takeWhile1P (Just "command/writefile") (\c -> c /= '|' && c /= '='))
      "appendfile" -> M.hspace *> (AppendFile . Text.strip <$> M.takeWhile1P (Just "command/appendfile") (\c -> c /= '|' && c /= '='))
      "writeline" -> M.hspace *> (WriteLine . Text.strip <$> M.takeWhile1P (Just "command/writeline") (\c -> c /= '|' && c /= '='))
      "readline" -> M.hspace *> fmap ReadLine (Text.strip <<$>> M.optional (M.takeWhile1P (Just "command/readline") (\c -> c /= '|' && c /= '=')))
      "passthru" -> pure PassThru
      -- "lines" -> M.hspace *> (Lines . ExternalCommand . Text.strip <$> M.takeWhile1P (Just "command/lines") (\c -> c /= '|' && c /= '='))
      _ -> Command . ExternalCommand . Text.strip . (<>) s <$> M.takeWhile1P (Just "command/external") (\c -> c /= '|' && c /= '=')

countInputs :: AtLeastTwo MergeInput -> Int
countInputs (AtLeastTwo a (b :| rest)) =
  sum
    $ mapMaybe
      ( \x -> case x of
          InputPipe -> Just 1
          _ -> Nothing
      )
      (a : b : rest)
