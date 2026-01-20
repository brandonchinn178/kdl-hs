{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Implement the v2 parser specified at: https://kdl.dev/spec/#name-full-grammar
-}
module KDL.Parser.Internal (
  -- * (1) Compatibility
  p_bom,
  p_version,

  -- * (3.1) Document
  p_document,
  p_nodes,
  p_line_space,
  p_node_space,

  -- * (3.2) Node
  p_node,
  p_final_node,
  p_base_node,
  p_node_prop_or_arg,
  p_node_terminator,

  -- * (3.3) Line Continuation
  p_escline,

  -- * (3.4) Property
  p_prop,

  -- * (3.5) Argument
  p_value'Entry,

  -- * (3.6) Children Block
  p_node_children,

  -- * (3.7) Value
  p_value,
  p_keyword,

  -- * (3.8) Type Annotation
  p_type,

  -- * (3.9) String
  p_string'Identifier,
  p_string,

  -- * (3.10) Identifier String
  p_identifier_string,
  isValidUnquotedString,
  p_unambiguous_ident,
  p_signed_ident,
  disallowed_keyword_identifiers,
  p_dotted_ident,
  p_identifier_char,

  -- * (3.11) Quoted String
  p_quoted_string,
  p_single_line_string_body,
  p_string_character,
  p_hex_unicode,
  p_ws_escape,

  -- * (3.12) Multi-line String
  p_multi_line_string_body,

  -- * (3.13) Raw String
  p_raw_string,
  p_raw_string_quotes,
  p_single_line_raw_string_body,
  p_single_line_raw_string_char,
  p_multi_line_raw_string_body,

  -- * (3.14) Number
  p_number,
  p_hex,
  p_hex_digit,
  p_octal,
  p_binary,
  p_decimal,
  p_integer,
  p_digits,
  p_exponent,
  p_sign,
  p_keyword_number,

  -- * (3.15) Boolean
  p_boolean,

  -- * (3.17) Whitespace
  p_ws,
  p_unicode_space,
  p_single_line_comment,
  p_multi_line_comment,
  p_slashdash,

  -- * (3.18) Newline
  p_newline,

  -- * (3.19) Disallowed Literal Code Points
  is_disallowed_literal_code_points,

  -- * Unicode
  p_unicode,
  is_unicode_scalar_value,
) where

import Control.Monad (guard, void)
import Data.Bifunctor (first)
import Data.Bits (toIntegralSized)
import Data.Char (
  chr,
  digitToInt,
  isDigit,
  isHexDigit,
  isOctDigit,
  isSpace,
  ord,
 )
import Data.Either (isRight)
import Data.Foldable (traverse_)
import Data.Foldable qualified as Seq (toList)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (catMaybes, fromMaybe)
import Data.Scientific (Scientific)
import Data.Scientific qualified as Scientific
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import KDL.Types (
  Ann (..),
  AnnFormat (..),
  Document,
  Entry (..),
  EntryFormat (..),
  Identifier (..),
  IdentifierFormat (..),
  Node (..),
  NodeFormat (..),
  NodeList (..),
  NodeListFormat (..),
  Value (..),
  ValueData (..),
  ValueFormat (..),
 )
import KDL.Types qualified as AnnFormat (AnnFormat (..))
import KDL.Types qualified as EntryFormat (EntryFormat (..))
import KDL.Types qualified as NodeFormat (NodeFormat (..))
import KDL.Types qualified as NodeListFormat (NodeListFormat (..))
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void Text

{----- (1) Compatibility -----}

-- | ref: (1)
-- bom := '\u{FEFF}'
p_bom :: Parser Text
p_bom = label "BOM" $ do
  string "\xFEFF"

-- | ref: (1)
-- version :=
--     '/-' unicode-space* 'kdl-version' unicode-space+ ('1' | '2')
--     unicode-space* newline
p_version :: Parser Text
p_version = label "Version" $ do
  withSource_ $ do
    _ <- string "/-"
    _ <- many p_unicode_space
    _ <- string "kdl-version"
    _ <- some p_unicode_space
    _ <- oneOf ['1', '2']
    _ <- many p_unicode_space
    _ <- p_newline
    pure ()

{----- (3.1) Document -----}

-- | ref: (3.1)
-- document := bom? version? nodes
p_document :: Parser Document
p_document = label "Document" $ do
  bom <- option "" p_bom
  version <- option "" $ try p_version
  nodes <- p_nodes
  eof
  pure . prependLeading bom . prependLeading version $ nodes

-- | ref: (3.1)
-- nodes := (line-space* node)* line-space*
p_nodes :: Parser NodeList
p_nodes = label "Nodes" $ do
  -- Collect a list of (Either Text Node), where Left is whitespace for the next node
  (nodes, extraWS') <-
    manyWithSlashdash . try $ do
      leading <- withSource_ $ many p_line_space
      node <- p_node
      pure $ prependLeading leading <$> node

  -- Assign whitespace to the next node
  extraWS <- Text.append extraWS' <$> withSource_ (many p_line_space)

  -- If there are no nodes, then all the whitespace goes to the NodeList's
  -- leading whitespace.
  -- If there are nodes, then each Node already parsed their own leading whitespace
  -- and the NodeList will only get trailing whitespace.
  let (leading, trailing) = if null nodes then (extraWS, "") else ("", extraWS)

  pure NodeList{nodes, format = Just NodeListFormat{..}}

-- | ref: (3.1) + (3.17)
-- // Whitespace where newlines are allowed.
-- line-space := node-space | newline | single-line-comment
p_line_space :: Parser ()
p_line_space = label "Line space" $ do
  void . choice $
    [ p_node_space
    , void p_newline
    , void p_single_line_comment
    ]

-- | ref: (3.1) + (3.17)
-- // Whitespace within nodes,
-- // where newline-ish things must be esclined.
-- node-space := ws* escline ws* | ws+
p_node_space :: Parser ()
p_node_space = label "Node space" $ do
  void . choice . map try $
    [ many p_ws *> p_escline *> many p_ws
    , some p_ws
    ]

{----- (3.2) Node -----}

-- | ref: (3.2)
-- node := base-node node-terminator
p_node :: Parser (SlashdashResult Node)
p_node = label "Node" $ do
  node <- p_base_node
  terminator <- p_node_terminator
  pure $ mapFormat (setNodeTerminator terminator) <$> node

setNodeTerminator :: Text -> NodeFormat -> NodeFormat
setNodeTerminator terminator format =
  format
    { NodeFormat.before_terminator = format.trailing
    , NodeFormat.terminator = terminator
    , NodeFormat.trailing = ""
    }

-- | ref: (3.2)
-- final-node := base-node node-terminator?
p_final_node :: Parser (SlashdashResult Node)
p_final_node = label "Node" $ do
  node <- p_base_node
  terminator <- optional p_node_terminator
  pure $ mapFormat (maybe id setNodeTerminator terminator) <$> node

-- | ref: (3.2)
-- base-node := slashdash? type? node-space* string
--     (node-space* (node-space | slashdash) node-prop-or-arg)*
--     // slashdashed node-children must always be after props and args.
--     (node-space* slashdash node-children)*
--     (node-space* node-children)?
--     (node-space* slashdash node-children)*
--     node-space*
p_base_node :: Parser (SlashdashResult Node)
p_base_node = label "Base Node" $ do
  parseOptSlashDash $ do
    -- node ann
    ann0 <- optional p_type
    annTrailing <- withSource_ $ many p_node_space
    let (ann, leading) =
          case ann0 of
            Just a -> (Just $ appendTrailing annTrailing a, "")
            Nothing -> (Nothing, annTrailing)

    -- node name
    name <- p_string'Identifier

    -- node-prop-or-arg
    (entries, entriesTrailing) <- manyWithSlashdash . try $ do
      entryLeading <- withSource_ $ many p_node_space
      entry <- parseOptSlashDash p_node_prop_or_arg
      pure $ prependLeading entryLeading <$> entry

    -- slashdashed node-children #1
    slashdashedChildren1 <- p_slashdashed_children

    -- node-children
    (childrenLeading, children) <- optional2 . try $ (,) <$> withSource_ (many p_node_space) <*> p_node_children
    let before_children = entriesTrailing <> slashdashedChildren1 <> fromMaybe "" childrenLeading

    -- slashdashed node-children #2
    slashdashedChildren2 <- p_slashdashed_children

    -- trailing space
    nodeTrailing <- withSource_ $ many p_node_space
    let trailing = slashdashedChildren2 <> nodeTrailing

    -- set by caller
    let before_terminator = ""
        terminator = ""

    pure Node{format = Just NodeFormat{..}, ..}
 where
  p_slashdashed_children =
    withSource_ . many . try $ do
      _ <- many p_node_space
      p_slashdash
      _ <- p_node_children
      pure ()

-- | ref: (3.2)
-- node-prop-or-arg := prop | value
p_node_prop_or_arg :: Parser Entry
p_node_prop_or_arg = label "Node entry" $ do
  try p_prop <|> p_value'Entry

-- | ref: (3.2)
-- node-terminator := single-line-comment | newline | ';' | eof
p_node_terminator :: Parser Text
p_node_terminator = label "Node terminator" $ do
  choice
    [ p_single_line_comment
    , p_newline
    , string ";"
    , p_eof
    ]

{----- (3.3) Line Continuation -----}

-- | ref: (3.3)
-- escline := '\\' ws* (single-line-comment | newline | eof)
p_escline :: Parser ()
p_escline = label "escline" $ do
  _ <- string "\\"
  _ <- many p_ws
  _ <- p_single_line_comment <|> p_newline <|> p_eof
  pure ()

{----- (3.4) Property -----}

-- | ref: (3.4)
-- prop := string node-space* '=' node-space* value
p_prop :: Parser Entry
p_prop = label "Property" $ do
  name <- p_string'Identifier
  after_key <- withSource_ $ many p_node_space
  _ <- char '='
  after_eq <- withSource_ $ many p_node_space
  (value, leading) <- p_value
  let format =
        EntryFormat
          { leading
          , after_key
          , after_eq
          , trailing = ""
          }
  pure Entry{name = Just name, value, format = Just format}

{----- (3.5) Argument -----}

-- | ref: (3.5)
p_value'Entry :: Parser Entry
p_value'Entry = label "Argument" $ do
  (value, leading) <- p_value
  let format =
        EntryFormat
          { leading
          , after_key = ""
          , after_eq = ""
          , trailing = ""
          }
  pure Entry{name = Nothing, value, format = Just format}

{----- (3.6) Children Block -----}

-- | ref: (3.6)
-- node-children := '{' nodes final-node? '}'
p_node_children :: Parser NodeList
p_node_children = label "Children" $ do
  between (char '{') (char '}') $ do
    nodes <- p_nodes
    finalNodeResult <- optional $ withSource p_final_node
    pure $
      case finalNodeResult of
        Nothing -> nodes
        Just (SlashDashed, src) -> appendTrailing src nodes
        Just (NoSlashDash finalNode, _) ->
          (mapTrailing (const "") nodes)
            { nodes = nodes.nodes <> [appendTrailing (maybe "" (.trailing) nodes.format) finalNode]
            }

{----- (3.7) Value -----}

-- | ref: (3.7)
-- value := type? node-space* (string | number | keyword)
p_value :: Parser (Value, Text)
p_value = label "Value" $ do
  ann0 <- optional p_type
  annTrailing <- withSource_ $ many p_node_space
  let (ann, leading) =
        case ann0 of
          Just a -> (Just $ appendTrailing annTrailing a, "")
          Nothing -> (Nothing, annTrailing)

  (data_, repr) <-
    withSource . choice . map try $
      [ Text <$> p_string
      , p_number
      , p_keyword
      ]

  pure (Value{ann, data_, format = Just ValueFormat{..}}, leading)

-- | ref: (3.7)
p_keyword :: Parser ValueData
p_keyword = label "Value keyword" $ do
  choice . map try $
    [ Bool <$> p_boolean
    , Null <$ string "#null"
    ]

{----- (3.8) Type Annotation -----}

-- | ref: (3.8)
-- type := '(' node-space* string node-space* ')'
p_type :: Parser Ann
p_type = label "Type annotation" $ do
  between (char '(') (char ')') $ do
    before_id <- withSource_ $ many p_node_space
    identifier <- p_string'Identifier
    after_id <- withSource_ $ many p_node_space
    let format = Just AnnFormat{leading = "", trailing = "", ..}
    pure Ann{..}

{----- (3.9) String -----}

-- | ref: (3.9)
p_string'Identifier :: Parser Identifier
p_string'Identifier = do
  (value, repr) <- withSource p_string
  pure Identifier{value, format = Just IdentifierFormat{repr}}

-- | ref: (3.9)
-- string := identifier-string | quoted-string | raw-string ¶
p_string :: Parser Text
p_string = label "String" $ do
  choice
    [ p_identifier_string
    , p_quoted_string
    , p_raw_string
    ]

{----- (3.10) Identifier String -----}

-- | ref: (3.10)
-- identifier-string := unambiguous-ident | signed-ident | dotted-ident
p_identifier_string :: Parser Text
p_identifier_string = label "Unquoted string" $ do
  choice . map try $
    [ p_unambiguous_ident
    , p_signed_ident
    , p_dotted_ident
    ]

isValidUnquotedString :: Text -> Bool
isValidUnquotedString = isRight . parse (p_identifier_string <* eof) ""

-- | ref: (3.10)
-- unambiguous-ident :=
--     ((identifier-char - digit - sign - '.') identifier-char*)
--     - disallowed-keyword-identifiers
p_unambiguous_ident :: Parser Text
p_unambiguous_ident = label "Unambiguous identifier" . try $ do
  c <- p_identifier_char
  guard $ not $ isDigit c || c `elem` ['-', '+', '.']
  cs <- many p_identifier_char
  let s = Text.pack (c : cs)
  guard $ s `Set.notMember` disallowed_keyword_identifiers
  pure s

-- | ref: (3.10)
-- signed-ident :=
--     sign ((identifier-char - digit - '.') identifier-char*)?
p_signed_ident :: Parser Text
p_signed_ident = label "Unambiguous identifier signed char" $ do
  c0 <- p_sign
  cs <- option "" $ do
    c1 <- p_identifier_char
    guard $ not $ isDigit c1 || c1 == '.'
    cs <- many p_identifier_char
    pure (c1 : cs)
  pure $ Text.pack (c0 : cs)

-- | ref: (3.10)
-- disallowed-keyword-identifiers :=
--     'true' | 'false' | 'null' | 'inf' | '-inf' | 'nan'
disallowed_keyword_identifiers :: Set Text
disallowed_keyword_identifiers =
  Set.fromList
    [ "true"
    , "false"
    , "null"
    , "inf"
    , "-inf"
    , "nan"
    ]

-- | ref: (3.10)
-- dotted-ident :=
--     sign? '.' ((identifier-char - digit) identifier-char*)?
p_dotted_ident :: Parser Text
p_dotted_ident = label "Unambiguous identifier dotted ident" $ do
  c0 <- option "" $ Text.singleton <$> p_sign
  c1 <- single '.'
  cs <- option "" $ do
    c2 <- p_identifier_char
    guard $ not $ isDigit c2
    cs <- many p_identifier_char
    pure (c2 : cs)
  pure $ c0 <> Text.pack (c1 : cs)

-- | ref: (3.10.2)
-- identifier-char :=
--     unicode - unicode-space - newline - [\\/(){};\[\]"#=]
--     - disallowed-literal-code-points
p_identifier_char :: Parser Char
p_identifier_char = label "Unambiguous identifier char" $ do
  satisfy $ \c -> Text.singleton c `Set.notMember` invalid && (not . is_disallowed_literal_code_points) c
 where
  invalid =
    Set.unions
      [ Set.map Text.singleton chars_unicode_space
      , chars_newline
      , Set.map Text.singleton $ Set.fromList "\\/(){};[]\"#="
      ]

{----- (3.11) Quoted String -----}

-- | ref: (3.11) + (3.12)
-- quoted-string :=
--     '"' single-line-string-body '"' |
--     '"""' newline
--     (multi-line-string-body newline)?
--     (unicode-space | ws-escape)* '"""'
p_quoted_string :: Parser Text
p_quoted_string = label "Quoted string" $ do
  choice
    [ between (string quotes3) (string quotes3) . fmap resolveMultilineString . withSource_ $ do
        _ <- p_newline
        _ <- optional . try $ do
          p_multi_line_string_body (p_newline *> parseMultilineTrailing *> string quotes3)
          p_newline
        parseMultilineTrailing
    , between (string quotes1) (string quotes1) . withSource_ $ do
        p_single_line_string_body
    ]
 where
  parseMultilineTrailing =
    void . many . choice $
      [ p_unicode_space
      , p_ws_escape
      ]

-- | ref: (3.11)
-- single-line-string-body := (string-character - newline)*
p_single_line_string_body :: Parser ()
p_single_line_string_body = label "Quoted string line" $ do
  void . many $ p_string_character (Just p_newline)

-- | ref: (3.11)
-- string-character :=
--     '\\' (["\\bfnrts] |
--     'u{' hex-unicode '}') |
--     ws-escape |
--     [^\\"] - disallowed-literal-code-points
p_string_character :: Maybe (Parser invalid) -> Parser Text
p_string_character invalid = label "String character" $ do
  choice . map try $
    [ do
        _ <- char '\\'
        fmap Text.singleton . choice $
          [ char '"'
          , char '\\'
          , char 'b' *> pure '\b'
          , char 'f' *> pure '\f'
          , char 'n' *> pure '\n'
          , char 'r' *> pure '\r'
          , char 't' *> pure '\t'
          , char 's' *> pure ' '
          , between (string "u{") (string "}") $ do
              chr <$> p_hex_unicode
          ]
    , "" <$ p_ws_escape
    , do
        traverse_ notFollowedBy invalid
        c <- satisfy (`notElem` ['\\', '"'])
        guard $ not $ is_disallowed_literal_code_points c
        pure $ Text.singleton c
    ]

-- | ref: (3.11.1)
-- hex-unicode := hex-digit{1, 6} - surrogate - above-max-scalar
-- surrogate := [0]{0, 2} [dD] [8-9a-fA-F] hex-digit{2}
-- //  U+D800-DFFF:         D   8          00
-- //                       D   F          FF
-- above-max-scalar = [2-9a-fA-F] hex-digit{5} |
--     [1] [1-9a-fA-F] hex-digit{4}
p_hex_unicode :: Parser Int
p_hex_unicode = label "Hex unicode" $ do
  digits <- countBetween 1 6 p_hex_digit
  guard $ (not . null) digits
  let x = undigits 16 digits
  guard $ is_unicode_scalar_value x
  pure x

-- | ref: (3.11.1.1)
-- ws-escape := '\\' (unicode-space | newline)+
p_ws_escape :: Parser ()
p_ws_escape = label "Escaped Whitespace" $ do
  char '\\' *> void (some $ p_unicode_space <|> void p_newline)

{----- (3.12) Multi-line String -----}

-- | ref: (3.12)
-- multi-line-string-body := (('"' | '""')? string-character)*
--
-- Requires some changes to the grammar: https://github.com/kdl-org/kdl/pull/552
p_multi_line_string_body :: Parser end -> Parser ()
p_multi_line_string_body end = label "Quoted multiline string body" $ do
  void $ manyTill validChar (try $ lookAhead end)
 where
  validChar = string "\"\"" <|> string "\"" <|> p_string_character Nothing

resolveMultilineString :: Text -> Text
resolveMultilineString =
  Text.intercalate "\n"
    . rmFirstAndLastNewlines
    . collapseWsOnlyLines
    . rmWsPrefix
    . Text.splitOn "\n"
 where
  -- First and last lines should be empty at this point; dropping those lines
  -- is equivalent to removing the first and last newline.
  rmFirstAndLastNewlines = dropEnd 1 . drop 1

  collapseWsOnlyLines =
    map $ \line -> if Text.all isSpace line then "" else line

  rmWsPrefix =
    modifyNE $ \lines_ ->
      let prefix = NonEmpty.last lines_
       in NonEmpty.map (rmPrefix prefix) lines_

  dropEnd n xs = take (length xs - n) xs

  rmPrefix pre s = fromMaybe s $ Text.stripPrefix pre s

  modifyNE :: (NonEmpty a -> NonEmpty a) -> [a] -> [a]
  modifyNE f xs =
    case NonEmpty.nonEmpty xs of
      Nothing -> []
      Just xs' -> NonEmpty.toList $ f xs'

{----- (3.13) Raw String -----}

-- | ref: (3.13)
-- raw-string := '#' raw-string-quotes '#' | '#' raw-string '#'
p_raw_string :: Parser Text
p_raw_string = label "Raw string" $ do
  -- For efficiency, we'll implement this slightly differently than the grammar
  -- verbatim. Find all the hashes up front and use that to pass the closing
  -- delimiter to the inner parsers.
  delim <- withSource_ . some $ string "#"
  s <- p_raw_string_quotes (string delim)
  _ <- string delim
  pure s

-- | ref: (3.13)
-- raw-string-quotes :=
--     '"' single-line-raw-string-body '"' |
--     '"""' newline
--     (multi-line-raw-string-body newline)?
--     unicode-space* '"""'
p_raw_string_quotes :: Parser end -> Parser Text
p_raw_string_quotes end = label "Raw string quotes" $ do
  choice
    [ between (string quotes3) (string quotes3) . fmap resolveMultilineString . withSource_ $ do
        _ <- p_newline
        _ <- optional . try $ do
          p_multi_line_raw_string_body (p_newline *> parseMultilineTrailing *> string quotes3)
          p_newline
        _ <- parseMultilineTrailing
        pure ()
    , between (string quotes1) (string quotes1) . withSource_ $ do
        p_single_line_raw_string_body (string quotes1 *> end)
    ]
 where
  parseMultilineTrailing = many p_unicode_space

-- | ref: (3.13)
-- single-line-raw-string-body :=
--     '' |
--     (single-line-raw-string-char - '"')
--         single-line-raw-string-char*? |
--     '"' (single-line-raw-string-char - '"')
--         single-line-raw-string-char*?
p_single_line_raw_string_body :: Parser end -> Parser ()
p_single_line_raw_string_body end = label "Raw string single line" $ do
  void . optional $ do
    _ <- optional $ char '"'
    c <- p_single_line_raw_string_char
    guard $ c /= '"'
    manyTill p_single_line_raw_string_char (try $ lookAhead end)

-- | ref: (3.13)
-- single-line-raw-string-char :=
--     unicode - newline - disallowed-literal-code-points
p_single_line_raw_string_char :: Parser Char
p_single_line_raw_string_char = label "Raw string character" $ do
  c <- p_unicode
  guard $ Text.singleton c `Set.notMember` chars_newline
  guard $ not $ is_disallowed_literal_code_points c
  pure c

-- | ref: (3.13)
-- multi-line-raw-string-body :=
--     (unicode - disallowed-literal-code-points)*?
p_multi_line_raw_string_body :: Parser end -> Parser ()
p_multi_line_raw_string_body end = label "Raw string multiline body" $ do
  void $ manyTill validChar (try $ lookAhead end)
 where
  validChar = do
    c <- p_unicode
    guard $ not $ is_disallowed_literal_code_points c
    pure $ Text.singleton c

{----- (3.14) Number -----}

-- | ref: (3.14)
-- number := keyword-number | hex | octal | binary | decimal
p_number :: Parser ValueData
p_number = label "Number" $ do
  choice . map try $
    [ p_keyword_number
    , Number <$> p_hex
    , Number <$> p_octal
    , Number <$> p_binary
    , Number <$> p_decimal
    ]

-- | ref: (3.14)
-- hex := sign? '0x' hex-digit (hex-digit | '_')*
p_hex :: Parser Scientific
p_hex = label "Hex" $ parseNumWith "0x" 16 isHexDigit

-- | ref: (3.14)
-- hex-digit := [0-9a-fA-F]
p_hex_digit :: Parser Int
p_hex_digit = label "Hex digit" $ do
  digitToInt <$> satisfy isHexDigit

-- | ref: (3.14)
-- octal := sign? '0o' [0-7] [0-7_]*
p_octal :: Parser Scientific
p_octal = label "Octal" $ parseNumWith "0o" 8 isOctDigit

-- | ref: (3.14)
-- binary := sign? '0b' ('0' | '1') ('0' | '1' | '_')*
p_binary :: Parser Scientific
p_binary = label "Binary" $ parseNumWith "0b" 2 isBinDigit
 where
  isBinDigit c = c == '0' || c == '1'

parseNumWith :: Text -> Integer -> (Char -> Bool) -> Parser Scientific
parseNumWith prefix base isValid = do
  signed <- parseSigned
  _ <- string prefix
  x <- p_digits base isValid
  pure . signed . fromInteger $ x

-- | ref: (3.14)
-- decimal := sign? integer ('.' integer)? exponent?
p_decimal :: Parser Scientific
p_decimal = label "Decimal" $ do
  signed <- parseSigned
  i <- p_integer
  (f, fdigits) <- option (0, 0) $ char '.' *> p_integer_withNumDigits
  e <- option 0 p_exponent
  pure . signed $ Scientific.scientific (i * (10 ^ fdigits) + f) (e - fdigits)
 where
  p_integer_withNumDigits = fmap (Text.length . Text.filter isDigit) <$> withSource p_integer

-- | ref: (3.14)
-- integer := digit (digit | '_')*
p_integer :: Parser Integer
p_integer = label "Integer" $ p_digits 10 isDigit

p_digits :: Integer -> (Char -> Bool) -> Parser Integer
p_digits base isValid = do
  d <- p_digit
  ds <-
    fmap catMaybes . many . choice $
      [ Just <$> p_digit
      , Nothing <$ char '_'
      ]
  pure . undigits base . map toInteger $ (d : ds)
 where
  p_digit = digitToInt <$> satisfy isValid

-- | ref: (3.14)
-- exponent := ('e' | 'E') sign? integer
p_exponent :: Parser Int
p_exponent = label "Exponent" $ do
  _ <- oneOf ['e', 'E']
  signed <- parseSigned
  x <- p_integer
  case toIntegralSized x of
    Just x' -> pure $ signed x'
    Nothing -> fail $ "Exponent is too large: " <> show x

-- | ref: (3.14)
-- sign := '+' | '-'
p_sign :: Parser Char
p_sign = label "Sign" $ do
  oneOf ['-', '+']

parseSigned :: (Num a) => Parser (a -> a)
parseSigned = toSign <$> optional p_sign
 where
  toSign = \case
    Just '-' -> negate
    _ -> id

-- | ref: (3.14.1)
-- keyword-number := '#inf' | '#-inf' | '#nan'
p_keyword_number :: Parser ValueData
p_keyword_number = label "Number keyword" $ do
  choice
    [ Inf <$ string "#inf"
    , NegInf <$ string "#-inf"
    , NaN <$ string "#nan"
    ]

{----- (3.15) Boolean -----}

-- | ref: (3.15)
-- boolean := '#true' | '#false'
p_boolean :: Parser Bool
p_boolean = label "Boolean" $ do
  choice
    [ True <$ string "#true"
    , False <$ string "#false"
    ]

{----- (3.17) Whitespace -----}

-- | ref: (3.17)
-- ws := unicode-space | multi-line-comment
p_ws :: Parser ()
p_ws = label "Whitespace" $ do
  p_unicode_space <|> p_multi_line_comment

-- | ref: (3.17)
-- unicode-space := See Table
--     (All White_Space unicode characters which are not `newline`)
p_unicode_space :: Parser ()
p_unicode_space = label "Unicode Space" $ do
  choice
    [ void (char c) <?> l
    | (c, l) <- chars_unicode_space'
    ]

-- | ref: (3.17)
chars_unicode_space :: Set Char
chars_unicode_space = Set.fromList $ map fst chars_unicode_space'

-- | ref: (3.17)
chars_unicode_space' :: [(Char, String)]
chars_unicode_space' =
  [ ('\x0009', "character tabulation")
  , ('\x0020', "space")
  , ('\x00A0', "no-break space")
  , ('\x1680', "ogham space mark")
  , ('\x2000', "en quad")
  , ('\x2001', "em quad")
  , ('\x2002', "en space")
  , ('\x2003', "em space")
  , ('\x2004', "three-per-em space")
  , ('\x2005', "four-per-em space")
  , ('\x2006', "six-per-em space")
  , ('\x2007', "figure space")
  , ('\x2008', "punctuation space")
  , ('\x2009', "thin space")
  , ('\x200A', "hair space")
  , ('\x202F', "narrow no-break space")
  , ('\x205F', "medium mathmatical space")
  , ('\x3000', "ideographic space")
  ]

-- | ref: (3.17.1)
-- single-line-comment := '//' ^newline* (newline | eof)
p_single_line_comment :: Parser Text
p_single_line_comment = label "Single-line comment" $ do
  withSource_ $ do
    _ <- string "//"
    _ <- takeWhileP Nothing (/= '\n')
    _ <- p_newline <|> p_eof
    pure ()

-- | ref: (3.17.2)
-- multi-line-comment := '/*' commented-block
p_multi_line_comment :: Parser ()
p_multi_line_comment = label "Multi-line comment" $ do
  string "/*" *> p_commented_block

-- | ref: (3.17.2)
-- commented-block :=
--     '*/' | (multi-line-comment | '*' | '/' | [^*/]+) commented-block
p_commented_block :: Parser ()
p_commented_block = label "Commented block" $ do
  choice
    [ void $ string "*/"
    , do
        choice
          [ p_multi_line_comment
          , void $ char '*'
          , void $ char '/'
          , void $ takeWhileP Nothing (`notElem` ['*', '/'])
          ]
        p_commented_block
    ]

data SlashdashResult a = SlashDashed | NoSlashDash a

instance Functor SlashdashResult where
  fmap f = \case
    SlashDashed -> SlashDashed
    NoSlashDash a -> NoSlashDash (f a)

-- | Run the given parser on multiple possibly-slashdashed components,
-- merge slashdashed components into the next component's leading whitespace,
-- and return any leftovers (e.g. slashdashed components at the end of the list).
manyWithSlashdash ::
  (HasWsFormat a) =>
  Parser (SlashdashResult a) ->
  Parser ([a], Text)
manyWithSlashdash = fmap merge . many . withSource
 where
  merge = first Seq.toList . foldl go (Seq.empty, "")
  go (nodes, buf) = \case
    -- If node is slashdashed, add all of its source code to the buffer
    (SlashDashed, src) -> (nodes, buf <> src)
    -- Otherwise, prepend the buffer to the node's leading whitespace
    (NoSlashDash node, _) -> (nodes Seq.|> prependLeading buf node, "")

-- | ref: (3.17.3)
-- slashdash := '/-' line-space*
p_slashdash :: Parser ()
p_slashdash = label "Slashdash comment" $ do
  string "/-" *> many p_line_space *> pure ()

parseOptSlashDash :: Parser a -> Parser (SlashdashResult a)
parseOptSlashDash m = do
  slashdash <- optional p_slashdash
  a <- m
  pure $ maybe (NoSlashDash a) (const SlashDashed) slashdash

{----- (3.18) Newline -----}

-- | ref: (3.18)
-- newline := See Table (All Newline White_Space)
p_newline :: Parser Text
p_newline = label "Newline" $ do
  choice [(string s) <?> l | (s, l) <- chars_newline']

-- | ref: (3.18)
chars_newline :: Set Text
chars_newline = Set.fromList $ map fst chars_newline'

-- | ref: (3.18)
chars_newline' :: [(Text, String)]
chars_newline' =
  [ ("\x000D\x000A", "CRLF")
  , ("\x000D", "CR")
  , ("\x000A", "LF")
  , ("\x0085", "NEL")
  , ("\x000B", "VT")
  , ("\x000C", "FF")
  , ("\x2028", "LS")
  , ("\x2029", "PS")
  ]

{----- (3.19) Disallowed Literal Code Points -----}

-- | ref: (3.19)
-- disallowed-literal-code-points :=
--     See Table (Disallowed Literal Code Points)
is_disallowed_literal_code_points :: Char -> Bool
is_disallowed_literal_code_points c =
  or
    [ -- The codepoints U+0000-0008 or the codepoints U+000E-001F (various control characters).
      or
        [ '\x0000' <= c && c <= '\x0008'
        , '\x000E' <= c && c <= '\x001F'
        ]
    , c == '\x007F' -- U+007F (the Delete control character).
    , (not . is_unicode_scalar_value . ord) c -- Any codepoint that is not a Unicode Scalar Value (U+D800-DFFF).
    , -- U+200E-200F, U+202A-202E, and U+2066-2069, the unicode "direction control" characters
      or
        [ '\x200E' <= c && c <= '\x200F'
        , '\x202A' <= c && c <= '\x202E'
        , '\x2066' <= c && c <= '\x2069'
        ]
    , -- U+FEFF, aka Zero-width Non-breaking Space (ZWNBSP)/Byte Order Mark (BOM), except as the first code point in a document.
      c == '\xFEFF'
    ]

{----- Unicode -----}

-- | unicode := Any Unicode Scalar Value
p_unicode :: Parser Char
p_unicode = label "Unicode scalar value" $ do
  satisfy (is_unicode_scalar_value . ord)

-- | https://unicode.org/glossary/#unicode_scalar_value
is_unicode_scalar_value :: Int -> Bool
is_unicode_scalar_value x =
  (0 <= x && x <= 0xD7FF) || (0xE000 <= x && x <= 0x10FFFF)

{----- Utilities -----}

withSource :: Parser a -> Parser (a, Text)
withSource p = do
  s <- getParserState
  a <- p
  s' <- getParserState
  let n = stateOffset s' - stateOffset s
  pure (a, Text.take n (stateInput s))

withSource_ :: Parser a -> Parser Text
withSource_ = fmap snd . withSource

optional2 :: Parser (a, b) -> Parser (Maybe a, Maybe b)
optional2 m = split <$> optional m
 where
  split = \case
    Nothing -> (Nothing, Nothing)
    Just (a, b) -> (Just a, Just b)

quotes1 :: Text
quotes1 = "\""

quotes3 :: Text
quotes3 = "\"\"\""

-- | Return a list whose length is in the range [lo, hi], inclusive.
countBetween :: Int -> Int -> Parser a -> Parser [a]
countBetween lo hi m = go 0
 where
  go n
    | n < lo = (:) <$> m <*> go (n + 1)
    | n < hi = ((:) <$> m <*> go (n + 1)) <|> pure []
    | otherwise = pure []

p_eof :: Parser Text
p_eof = "" <$ eof

undigits :: (Num a) => a -> [a] -> a
undigits base = foldl' (\acc x -> acc * base + x) 0

class HasFormat a where
  type KdlFormat a
  mapFormat :: (KdlFormat a -> KdlFormat a) -> a -> a
instance HasFormat NodeList where
  type KdlFormat NodeList = NodeListFormat
  mapFormat f NodeList{..} = NodeList{format = f <$> format, ..}
instance HasFormat Node where
  type KdlFormat Node = NodeFormat
  mapFormat f Node{..} = Node{format = f <$> format, ..}
instance HasFormat Ann where
  type KdlFormat Ann = AnnFormat
  mapFormat f Ann{..} = Ann{format = f <$> format, ..}
instance HasFormat Entry where
  type KdlFormat Entry = EntryFormat
  mapFormat f Entry{..} = Entry{format = f <$> format, ..}

class (HasFormat a) => HasWsFormat a where
  mapLeading :: (Text -> Text) -> a -> a
  mapTrailing :: (Text -> Text) -> a -> a
  prependLeading :: Text -> a -> a
  prependLeading s = mapLeading (s <>)
  appendTrailing :: Text -> a -> a
  appendTrailing s = mapTrailing (<> s)
instance HasWsFormat NodeList where
  mapLeading f = mapFormat $ \format -> format{NodeListFormat.leading = f format.leading}
  mapTrailing f = mapFormat $ \format -> format{NodeListFormat.trailing = f format.trailing}
instance HasWsFormat Node where
  mapLeading f = mapFormat $ \format -> format{NodeFormat.leading = f format.leading}
  mapTrailing f = mapFormat $ \format -> format{NodeFormat.trailing = f format.trailing}
instance HasWsFormat Ann where
  mapLeading f = mapFormat $ \format -> format{AnnFormat.leading = f format.leading}
  mapTrailing f = mapFormat $ \format -> format{AnnFormat.trailing = f format.trailing}
instance HasWsFormat Entry where
  mapLeading f = mapFormat $ \format -> format{EntryFormat.leading = f format.leading}
  mapTrailing f = mapFormat $ \format -> format{EntryFormat.trailing = f format.trailing}
