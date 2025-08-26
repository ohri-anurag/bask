module Parser where

import Control.Monad.Except (throwError)
import Data.Char (isDigit)
import Data.Text qualified as Text
import Relude hiding (some)

data AtLeastTwo a = AtLeastTwo a (NonEmpty a)
  deriving (Show, Eq)

newtype Script = Script (NonEmpty Pipeline)
  deriving (Show, Eq)

newtype Pipeline = Pipeline (NonEmpty Step)
  deriving (Show, Eq)

newtype Step = Step (NonEmpty Section)
  deriving (Show, Eq)

data Section
  = SingleInput Pipe
  | Merge (AtLeastTwo Text)
  | ReOrder (AtLeastTwo Int)
  | Concat Int
  deriving (Show, Eq)

data Pipe
  = SinglePipe PipeCommand
  | SharedPipe (AtLeastTwo PipeCommand)
  deriving (Show, Eq)

data PipeCommand
  = PipelineCommand Pipeline
  | SingleCommand Command
  deriving (Show, Eq)

data Command
  = Command ExternalCommand
  | ReadLine Text
  | WriteLine Text
  | WriteFile Text
  | AppendFile Text
  | ShowOutput ExternalCommand
  | PassThru
  deriving
    ( -- | Lines ExternalCommand
      Show,
      Eq
    )

newtype ExternalCommand = ExternalCommand (NonEmpty CommandText)
  deriving (Show, Eq)

data CommandText
  = JustText Text
  | Argument Int
  deriving (Show, Eq)

data ParserState = ParserState
  { line :: Int,
    column :: Int,
    input :: Text
  }
  deriving (Show)

data Error = Error Text ParserState
  deriving (Show)

type Parser = ExceptT Error (State ParserState)

parse :: Parser a -> Text -> Either Error a
parse parser input = evalState (runExceptT parser) $ ParserState 1 1 input

peek :: Int -> Parser Text
peek i = do
  ParserState {input} <- get
  pure $ Text.take i input

consume :: Char -> Parser ()
consume c = do
  st@ParserState {..} <- get
  case Text.uncons input of
    Nothing -> throwError $ Error ("Expected: " <> show c <> ", but reached End of Input!") st
    Just (i, rest) ->
      if i == c
        then put st {input = rest, column = if c == '\n' then 0 else succ column, line = if c == '\n' then succ line else line}
        else throwError $ Error ("Expected: " <> show c <> ", but received: " <> show i) st

space :: Parser ()
space = consume ' '

string :: Text -> Parser ()
string = traverse_ consume . Text.unpack

word :: Parser Text
word = consumeWhile (\c -> not (c == ' ' || c == '\n'))

consumeWhile :: (Char -> Bool) -> Parser Text
consumeWhile cond = do
  ParserState {input} <- get
  let (l, _) = Text.span cond input
  string l $> l

isSeparator :: Char -> Bool
isSeparator c = c == '|' || c == '=' || c == '\n'

textBlocks :: Bool -> Parser Text
textBlocks isExternal = Text.pack . reverse <$> helper []
  where
    helper acc = do
      peek 2 >>= \case
        "||" -> pure acc
        "==" -> pure acc
        "\\$" -> string "\\$" *> helper ('$' : acc)
        s -> case Text.uncons s of
          Nothing -> pure acc
          Just (c, _) ->
            case c of
              '\n' -> pure acc
              '$' ->
                if isExternal
                  then pure acc
                  else consume '$' *> helper ('$' : acc)
              _ -> consume c *> helper (c : acc)

textBlock :: Parser Text
textBlock = textBlocks False

command :: Parser Command
command =
  word >>= \case
    "readline" -> space *> fmap (ReadLine . Text.strip) textBlock
    "writeline" -> space *> fmap (WriteLine . Text.strip) textBlock
    "writefile" -> space *> fmap (WriteFile . Text.strip) textBlock
    "appendfile" -> space *> fmap (AppendFile . Text.strip) textBlock
    "show" -> space *> fmap ShowOutput externalCommand
    "passthru" -> pure PassThru
    w -> do
      modify $ \st@(ParserState {input}) -> st {input = w <> input}
      Command <$> externalCommand
  where
    externalCommand =
      ExternalCommand <$> do
        a <- JustText <$> textBlocks True
        (a :|) <$> helper
    helper = do
      peek 1 >>= \case
        "$" -> do
          consume '$'
          st <- get
          nstr <- consumeWhile isDigit
          n <- hoistEither . bimap (\e -> Error e st) Argument . readEither $ toString nstr
          when (n == Argument 0) $ throwError $ Error "Arguments passed from bash must be numbered starting from 1!" st
          b <- JustText <$> textBlocks True
          helper <&> \x -> n : b : x
        _ -> pure []

pipeCommand :: Parser PipeCommand
pipeCommand = SingleCommand <$> command

pipe :: Parser Pipe
pipe = do
  a <- pipeCommand
  helper >>= \case
    [] -> pure $ SinglePipe a
    b : rest -> pure $ SharedPipe (AtLeastTwo a (b :| rest))
  where
    helper = do
      peek 2 >>= \case
        "==" -> do
          string "==" *> space
          a <- pipeCommand
          (a :) <$> helper
        _ -> pure []

section :: Parser Section
section =
  word >>= \case
    "merge" ->
      space *> do
        Text.splitOn "pipe" <$> textBlock >>= \case
          a : b : rest -> pure . Merge . AtLeastTwo a $ b :| rest
          _ -> get >>= throwError . Error ("merge command requires at least two strings to merge, separated by the literal string `pipe`!")
    "reorder" ->
      space *> do
        fmap (mapMaybe (readMaybe . toString) . Text.words) textBlock >>= \case
          a : b : rest -> pure . ReOrder . AtLeastTwo a $ b :| rest
          _ -> get >>= throwError . Error ("reorder command requires at least two integers!")
    "concat" ->
      space *> do
        fmap (readMaybe . toString) textBlock >>= \case
          Just i -> pure $ Concat i
          Nothing -> get >>= throwError . Error ("concat command requires at least one integer!")
    w -> do
      st@ParserState {input} <- get
      put st {input = w <> input}
      SingleInput <$> pipe

step :: Parser Step
step = do
  a <- section
  (Step . (:|) a) <$> helper
  where
    helper = do
      peek 2 >>= \case
        "||" -> do
          string "||" *> space
          b <- section
          (b :) <$> helper
        _ -> pure []

pipeline :: Parser Pipeline
pipeline = do
  a <- step
  (Pipeline . (:|) a) <$> helper
  where
    helper = do
      peek 2 >>= \case
        "\n|" -> do
          string "\n|" *> space

          b <- step
          (b :) <$> helper
        _ -> pure []

script :: Parser Script
script = do
  a <- pipeline
  (Script . (:|) a) <$> helper
  where
    helper = do
      peek 2 >>= \case
        "\n\n" -> do
          string "\n\n"
          b <- pipeline
          (b :) <$> helper
        _ -> pure []
