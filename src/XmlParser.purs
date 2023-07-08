module XmlParser
  ( Element(..)
  , XmlAttribute(..)
  , XmlNode(..)
  , parseXmlNode
  , parseXmlNodes
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Lazy (defer)
import Data.Array as Array
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.List as List
import Data.Show.Generic (genericShow)
import Data.String.CodeUnits (fromCharArray)
import StringParser (anyChar, regex, skipSpaces, string, whiteSpace, Parser, ParseError, runParser, try)
import StringParser.Combinators (many, manyTill, option, optional, sepEndBy)

data XmlNode
  = XmlElement Element
  | XmlText String
  | XmlComment String

derive instance Generic XmlNode _
instance Show XmlNode where
  show = defer \_ -> genericShow

type Element =
  { name :: String
  , attributes :: List XmlAttribute
  , children :: List XmlNode
  }

data XmlAttribute = XmlAttribute String String

derive instance Generic XmlAttribute _
instance Show XmlAttribute where
  show = genericShow

charListToString :: List Char -> String
charListToString = fromCharArray <<< Array.fromFoldable

attributeParser :: Parser XmlAttribute
attributeParser = do
  k :: String <- regex "[^=>/]+"
  v :: String <- option "" (string "=\"" *> regex "[^\"]*" <* string "\"")
  pure $ XmlAttribute k v

openingParser :: Parser Element
openingParser = do
  _ <- string "<"

  name :: String <- regex "[^/> ]+"

  attributes :: List XmlAttribute <-
    whiteSpace *> sepEndBy attributeParser whiteSpace

  pure
    { name
    , attributes
    , children: List.Nil
    }

closingOrChildrenParser :: Element -> Parser Element
closingOrChildrenParser element = defer \_ ->
  try childrenParser <|> try closingParser
  where
  closingParser :: Parser Element
  closingParser = whiteSpace *> optional (string "/") *> string ">" *> pure element

  childrenParser :: Parser Element
  childrenParser = do
    _ <- whiteSpace *> string ">"
    children :: List XmlNode <- manyTill nodeParser
      (string ("</" <> element.name <> ">"))
    pure $
      element { children = children }

elementParser :: Parser XmlNode
elementParser = defer \_ -> do
  skipSpaces
  openingParser
    >>= closingOrChildrenParser
    >>=
      pure <<< XmlElement

textParser :: Parser XmlNode
textParser = XmlText <$> regex "[^<]+"

commentParser :: Parser XmlNode
commentParser = do
  skipSpaces
  comment :: List Char <- string "<!--" *> manyTill anyChar (string "-->")
  pure $ XmlComment $ charListToString comment

nodeParser :: Parser XmlNode
nodeParser = defer \_ ->
  try textParser
    <|> try commentParser
    <|>
      elementParser

parseXmlNodes :: String -> Either ParseError (List XmlNode)
parseXmlNodes input =
  runParser (many nodeParser) input

parseXmlNode :: String -> Either ParseError XmlNode
parseXmlNode input =
  runParser nodeParser input