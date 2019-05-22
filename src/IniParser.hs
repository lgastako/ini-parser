{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module IniParser where

import           Control.Monad               ( void )
import           Data.Attoparsec.Text        ( Parser
                                             , char
                                             , endOfInput
                                             , endOfLine
                                             , many'
                                             , many1
                                             , notInClass
                                             , parseOnly
                                             , satisfy
                                             , space
                                             )
import           Data.Map.Strict             ( Map )
import qualified Data.Map.Strict      as Map
import           Data.Text                   ( Text
                                             , pack
                                             )
import           System.Environment          ( getArgs )

type Ini = Map Text Section

data Section = Section
  { name       :: Text
  , properties :: Map Text Text
  } deriving (Show)

main :: IO ()
main = do
  [path] <- getArgs
  parseIniFile path >>= \case
    Right ini -> putStrLn $ "Parsed INI: " ++ show ini
    Left err  -> putStrLn $ "ERROR parsing ini: " ++ err

parseIniFile :: FilePath -> IO (Either String Ini)
parseIniFile iniFilePath = parseIni . pack <$> readFile iniFilePath

parseIni :: Text -> Either String Ini
parseIni = parseOnly ini

ini :: Parser Ini
ini = do
  defaultSection <- lexeme (Section "" <$> (Map.fromList <$> many' property))
  namedSections  <- lexeme (many' section)
  void endOfInput
  let allSections | null (properties defaultSection) = namedSections
                  | otherwise = defaultSection:namedSections
  pure . Map.fromList . map (\section -> (name section, section))
    $ allSections

section :: Parser Section
section = Section <$> sectionName <*> (Map.fromList <$> many' (lexeme property))

sectionName :: Parser Text
sectionName = char '[' *> sectionNameChars <* char ']' <* endOfLine

sectionNameChars :: Parser Text
sectionNameChars = pack <$> many' (satisfy $ notInClass "]\r\n")

property :: Parser (Text, Text)
property = (,) <$> propertyName <*> (lexeme (char '=') *> propertyValue)

propertyName :: Parser Text
propertyName = pack <$> many' (satisfy $ notInClass "=\r\n\t ")

propertyValue :: Parser Text
propertyValue = pack <$> many' (satisfy $ notInClass "\r\n")

lexeme :: Parser a -> Parser a
lexeme p = whitespace *> p <* whitespace

whitespace :: Parser String
whitespace = many' space
