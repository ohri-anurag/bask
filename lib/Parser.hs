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

newtype Section = Section (NonEmpty Command)
  deriving (Show, Eq)

data Command
  = Command ExternalCommand
  | ReadLine Text
  | WriteLine Text
  | WriteFile Text
  | AppendFile Text
  | ShowOutput ExternalCommand
  | PassThru
  | Concat (AtLeastTwo CommandText)
  | ChangeDir CommandText
  | If Condition (NonEmpty CommandText) (NonEmpty CommandText)
  deriving (Show, Eq)

data Condition
  = Exists CommandText
  | Equals CommandText CommandText
  deriving (Show, Eq)

newtype ExternalCommand = ExternalCommand (NonEmpty CommandText)
  deriving (Show, Eq)

data CommandText
  = JustText Text
  | ExternalArgument Int
  | PipeArgument Int
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

textBlock :: Parser Text
textBlock = Text.pack . reverse <$> helper []
  where
    helper acc = do
      peek 2 >>= \case
        "||" -> pure acc
        "==" -> pure acc
        s -> case Text.uncons s of
          Nothing -> pure acc
          Just (c, _) ->
            case c of
              '\n' -> pure acc
              '$' -> consume '$' *> helper ('$' : acc)
              _ -> consume c *> helper (c : acc)

singleCommandText :: Parser CommandText
singleCommandText =
  peek 1 >>= \case
    "$" -> do
      consume '$'
      st <- get
      nstr <- consumeWhile isDigit
      n <- hoistEither . bimap (\e -> Error e st) ExternalArgument . readEither $ toString nstr
      when (n == ExternalArgument 0) $ throwError $ Error "External Arguments must be numbered starting from 1!" st
      pure n
    "#" -> do
      consume '#'
      st <- get
      nstr <- consumeWhile isDigit
      n <- hoistEither . bimap (\e -> Error e st) PipeArgument . readEither $ toString nstr
      when (n == PipeArgument 0) $ throwError $ Error "Internal Arguments must be numbered starting from 1!" st
      pure n
    _ -> helper []
  where
    toJust = pure . JustText . Text.pack . reverse
    helper acc =
      peek 2 >>= \case
        "||" -> toJust acc
        "==" -> toJust acc
        "\\$" -> string "\\$" *> helper ('$' : acc)
        "\\#" -> string "\\#" *> helper ('#' : acc)
        "\\n" -> string "\\n" *> helper ('\n' : acc)
        s -> case Text.uncons s of
          Nothing -> toJust acc
          Just (c, _) ->
            case c of
              '$' -> pure . JustText . Text.pack $ reverse acc
              '#' -> pure . JustText . Text.pack $ reverse acc
              '\n' -> toJust acc
              _ -> consume c *> helper (c : acc)

commandText :: Parser [CommandText]
commandText = do
  a <- singleCommandText
  rest <-
    peek 2 >>= \case
      "==" -> pure []
      "||" -> pure []
      _ ->
        peek 1 >>= \case
          "\n" -> pure []
          "" -> pure []
          _ -> commandText
  pure $ a : rest

externalCommand :: Parser ExternalCommand
externalCommand =
  commandText >>= \case
    [] -> get >>= throwError . Error "Expected at least one word for this section/step!"
    a : as -> pure $ ExternalCommand $ a :| as

condition :: Parser Condition
condition = do
  a <- singleCommandText
  peek 1 >>= \case
    "=" -> do
      consume '='
      space
      Equals a <$> singleCommandText
    _ -> pure $ Exists a

thenBranch :: Parser (NonEmpty CommandText)
thenBranch = do
  a <- singleCommandText
  (a :|) <$> helper
  where
    helper =
      peek 5 >>= \case
        " else" -> pure []
        _ -> do
          b <- singleCommandText
          (b :) <$> helper

command :: Parser Command
command =
  word >>= \case
    "readline" -> space *> fmap (ReadLine . Text.strip) textBlock
    "writeline" -> space *> fmap (WriteLine . Text.strip) textBlock
    "writefile" -> space *> fmap (WriteFile . Text.strip) textBlock
    "appendfile" -> space *> fmap (AppendFile . Text.strip) textBlock
    "show" -> space *> fmap ShowOutput externalCommand
    "passthru" -> do
      peek 1 >>= \case
        " " -> consume ' ' $> PassThru
        _ -> pure PassThru
    "concat" -> do
      space
      commandText >>= \case
        a : b : rest -> pure . Concat . AtLeastTwo a $ b :| rest
        _ -> get >>= throwError . Error "Expected at least two arguments for concat command!"
    "cd" -> do
      space
      commandText >>= \case
        [x] -> pure $ ChangeDir x
        _ -> get >>= throwError . Error "Expected one argument for cd command!"
    "if" -> do
      space
      c <- condition
      space
      string "then "
      a <- thenBranch
      string " else "
      commandText >>= \case
        (b : rest) -> pure $ If c a (b :| rest)
        _ -> get >>= throwError . Error "Expected at least one input for the else branch!"
    w -> do
      modify $ \st@(ParserState {input}) -> st {input = w <> input}
      Command <$> externalCommand

sepBy :: Parser a -> Text -> Parser (NonEmpty a)
sepBy p s = (:|) <$> p <*> helper
  where
    helper =
      peek (Text.length s) >>= \str ->
        if str == s
          then do
            string s
            (:) <$> p <*> helper
          else pure []

section :: Parser Section
section = Section <$> sepBy command "== "

step :: Parser Step
step = Step <$> sepBy section "|| "

pipeline :: Parser Pipeline
pipeline = Pipeline <$> sepBy step "\n| "

script :: Parser Script
script = Script <$> sepBy pipeline "\n\n"
