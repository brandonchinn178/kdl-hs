{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoFieldSelectors #-}

{-|
Implement the v2 parser specified at: https://kdl.dev/spec/#name-full-grammar
-}
module KDL.Parser.Internal (
  Parser,

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

import Control.Monad (guard, void, (>=>))
import Control.Monad.Trans.State.Strict (StateT, evalStateT)
import Control.Monad.Trans.State.Strict qualified as State
import Data.Bifunctor (bimap)
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
import Data.Foldable (foldlM, traverse_)
import Data.Foldable qualified as Seq (toList)
import Data.Maybe (catMaybes, fromMaybe, isJust)
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
  AnnExtension (..),
  AnnFormat (..),
  Document,
  Entry (..),
  EntryExtension (..),
  EntryFormat (..),
  Identifier (..),
  IdentifierExtension (..),
  IdentifierFormat (..),
  Node (..),
  NodeExtension (..),
  NodeFormat (..),
  NodeList (..),
  NodeListExtension (..),
  NodeListFormat (..),
  Value (..),
  ValueData (..),
  ValueExtension (..),
  ValueFormat (..),
 )
import KDL.Types qualified as AnnExtension (AnnExtension (..))
import KDL.Types qualified as AnnFormat (AnnFormat (..))
import KDL.Types qualified as EntryExtension (EntryExtension (..))
import KDL.Types qualified as EntryFormat (EntryFormat (..))
import KDL.Types qualified as NodeExtension (NodeExtension (..))
import KDL.Types qualified as NodeFormat (NodeFormat (..))
import KDL.Types qualified as NodeListExtension (NodeListExtension (..))
import KDL.Types qualified as NodeListFormat (NodeListFormat (..))
import Text.Megaparsec
import Text.Megaparsec.Char

#if !MIN_VERSION_base(4,20,0)
import Data.Foldable (foldl')
#endif

type Parser = Parsec Void Text

{----- (1) Compatibility -----}

-- | ref: (1)
-- bom := '\u{FEFF}'
p_bom :: Parser ()
p_bom = label "BOM" $ do
  void $ string "\xFEFF"

-- | ref: (1)
-- version :=
--     '/-' unicode-space* 'kdl-version' unicode-space+ ('1' | '2')
--     unicode-space* newline
p_version :: Parser ()
p_version = label "version" $ do
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
p_document = do
  bom <- hidden . option "" . withSource_ $ p_bom
  version <- hidden . option "" . withSource_ $ try p_version
  nodes <- p_nodes
  hidden eof
  pure . prependLeading bom . prependLeading version $ nodes

-- | ref: (3.1)
-- nodes := (line-space* node)* line-space*
p_nodes :: Parser NodeList
p_nodes = label "nodes" $ do
  -- The grammar is left-associative, but we do right-associative to get
  -- correct backtracking semantics + good parse errors
  initialWS <- withSource_ $ many p_line_space
  results <- fmap concat . manyWhile $ do
    (sdash, parseNode) <- p_node
    (result, continue) <-
      resolveSlashdash sdash parseNode >>= \case
        Left src -> pure (Left src, True)
        Right (node, hasTerminator) -> pure (Right node, hasTerminator)
    trailing <- withSource_ $ many p_line_space
    pure ([result, Left trailing], continue)
  let (nodes, leftoverWS) = mergeLeadingWS initialWS results
  -- If there are no nodes, all the whitespace should be considered leading
  -- whitespace. Otherwise, the leftover whitespace (e.g. slashdashed nodes
  -- at the end) are trailing whitespace.
  let (leading, trailing) =
        if null nodes
          then (leftoverWS, "")
          else ("", leftoverWS)
      ext =
        NodeListExtension
          { format = Just NodeListFormat{..}
          }
  pure NodeList{..}
 where
  manyWhile :: Parser (a, Bool) -> Parser [a]
  manyWhile p =
    optional p >>= \case
      Nothing -> pure []
      Just (a, continue) -> fmap (a :) . option [] $ if continue then manyWhile p else empty

-- | ref: (3.1) + (3.17)
-- // Whitespace where newlines are allowed.
-- line-space := node-space | newline | single-line-comment
p_line_space :: Parser ()
p_line_space = hidden $ do
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
p_node_space = hidden $ do
  void . choice . map try $
    [ many p_ws *> p_escline *> many p_ws
    , some p_ws
    ]

{----- (3.2) Node -----}

-- | ref: (3.2)
-- node := base-node node-terminator
-- final-node := base-node node-terminator?
p_node :: Parser (SlashdashResult, Parser (Node, Bool))
p_node = label "node" $ do
  (sdash, parseNode) <- p_base_node
  pure . (sdash,) $ do
    node <- parseNode
    mTerminator <- optional p_node_terminator
    pure (mapFormat (maybe id setTerminator mTerminator) node, isJust mTerminator)
 where
  setTerminator terminator format =
    format
      { NodeFormat.beforeTerminator = format.trailing
      , NodeFormat.terminator = terminator
      , NodeFormat.trailing = ""
      }

-- | ref: (3.2)
-- base-node := slashdash? type? node-space* string
--     (node-space* (node-space | slashdash) node-prop-or-arg)*
--     // slashdashed node-children must always be after props and args.
--     (node-space* slashdash node-children)*
--     (node-space* node-children)?
--     (node-space* slashdash node-children)*
--     node-space*
p_base_node :: Parser (SlashdashResult, Parser Node)
p_base_node = label "base node" $ do
  sdash <- option NoSlashdash p_slashdash
  pure . (sdash,) $ do
    -- node ann
    initialAnn <- optional p_type
    postAnnWS <- withSource_ $ many p_node_space
    let (ann, leading) =
          case initialAnn of
            Just a -> (Just $ appendTrailing postAnnWS a, "")
            Nothing -> (Nothing, postAnnWS)

    -- node name
    name <- p_string'Identifier

    -- node entries
    (entries, postEntriesWS) <- mergeLeadingWS "" <$> p_entries

    -- slashdashed node-children #1
    slashdashedChildren1 <- p_slashdashed_children

    -- node children
    let unzipMaybe = maybe (Nothing, Nothing) (\(a, b) -> (Just a, Just b))
    (preChildrenWS, children) <- fmap unzipMaybe . optional $ do
      -- Make sure a children block is coming up before we commit to consuming
      -- the whitespace as pre-children whitespace
      _ <- lookAhead $ try $ many p_node_space *> label "children block" (char '{')
      (,)
        <$> withSource_ (many p_node_space)
        <*> p_node_children
    let beforeChildren = postEntriesWS <> slashdashedChildren1 <> fromMaybe "" preChildrenWS

    -- slashdashed node-children #2
    slashdashedChildren2 <- p_slashdashed_children

    -- trailing space
    postChildrenWS <- withSource_ $ many p_node_space
    let trailing = slashdashedChildren2 <> postChildrenWS

    -- set by caller
    let beforeTerminator = ""
        terminator = ""

    let ext =
          NodeExtension
            { format = Just NodeFormat{..}
            }

    pure Node{..}
 where
  -- (node-space* (node-space | slashdash) node-prop-or-arg)*
  p_entries = fmap concat . many . label "node prop or arg" $ do
    -- make sure this is not the pre-children whitespace or post-node whitespace
    -- so that we can commit to this being node-prop-or-arg whitespace
    notFollowedBy $ do
      _ <- many p_node_space
      _ <- optional p_slashdash
      choice
        [ void (char '{')
        , void (char '}')
        , void p_node_terminator
        ]
    leading <- withSource_ $ many p_node_space
    -- if there was no node-space, it _must_ be a slashdash
    sdash <- (if Text.null leading then id else option NoSlashdash) p_slashdash
    entry <- resolveSlashdash sdash p_node_prop_or_arg
    pure [Left leading, entry]

  -- (node-space* slashdash node-children)*
  p_slashdashed_children =
    withSource_ . many . try $ do
      _ <- many p_node_space
      _ <- p_slashdash
      _ <- p_node_children
      pure ()

-- | ref: (3.2)
-- node-prop-or-arg := prop | value
p_node_prop_or_arg :: Parser Entry
p_node_prop_or_arg = label "node entry" $ do
  try p_prop <|> p_value'Entry

-- | ref: (3.2)
-- node-terminator := single-line-comment | newline | ';' | eof
p_node_terminator :: Parser Text
p_node_terminator = label "end of node" $ do
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
p_prop = label "property" $ do
  name <- p_string'Identifier
  afterKey <- withSource_ $ many p_node_space
  _ <- char '='
  afterEq <- withSource_ $ many p_node_space
  (value, leading) <- p_value
  let format =
        EntryFormat
          { leading
          , afterKey
          , afterEq
          , trailing = ""
          }
      ext = EntryExtension{format = Just format}
  pure Entry{name = Just name, ..}

{----- (3.5) Argument -----}

-- | ref: (3.5)
p_value'Entry :: Parser Entry
p_value'Entry = label "argument" $ do
  (value, leading) <- p_value
  let format =
        EntryFormat
          { leading
          , afterKey = ""
          , afterEq = ""
          , trailing = ""
          }
      ext =
        EntryExtension
          { format = Just format
          }
  pure Entry{name = Nothing, ..}

{----- (3.6) Children Block -----}

-- | ref: (3.6)
-- node-children := '{' nodes final-node? '}'
p_node_children :: Parser NodeList
p_node_children = label "children block" $ do
  between (char '{') (char '}') $ do
    p_nodes -- 'final-node?' logic is in p_nodes / p_node

{----- (3.7) Value -----}

-- | ref: (3.7)
-- value := type? node-space* (string | number | keyword)
p_value :: Parser (Value, Text)
p_value = label "value" $ do
  initialAnn <- optional p_type
  postAnnWS <- withSource_ $ many p_node_space
  let (ann, leading) =
        case initialAnn of
          Just a -> (Just $ appendTrailing postAnnWS a, "")
          Nothing -> (Nothing, postAnnWS)

  (data_, repr_) <-
    withSource . choice . map try $
      [ String <$> p_string
      , p_number
      , p_keyword
      ]
  let repr = Just repr_

  let ext =
        ValueExtension
          { format = Just ValueFormat{..}
          }
  pure (Value{..}, leading)

-- | ref: (3.7)
p_keyword :: Parser ValueData
p_keyword = label "value keyword" $ do
  choice . map try $
    [ Bool <$> p_boolean
    , Null <$ string "#null"
    ]

{----- (3.8) Type Annotation -----}

-- | ref: (3.8)
-- type := '(' node-space* string node-space* ')'
p_type :: Parser Ann
p_type = label "type annotation" $ do
  between (char '(') (char ')') $ do
    beforeId <- withSource_ $ many p_node_space
    identifier <- p_string'Identifier
    afterId <- withSource_ $ many p_node_space
    let format = Just AnnFormat{leading = "", trailing = "", ..}
        ext = AnnExtension{format}
    pure Ann{..}

{----- (3.9) String -----}

-- | ref: (3.9)
p_string'Identifier :: Parser Identifier
p_string'Identifier = do
  (value, repr_) <- withSource p_string
  let repr = Just repr_
  let ext = IdentifierExtension{format = Just IdentifierFormat{..}}
  pure Identifier{..}

-- | ref: (3.9)
-- string := identifier-string | quoted-string | raw-string ¶
p_string :: Parser Text
p_string = label "string" $ do
  choice
    [ p_identifier_string
    , p_quoted_string
    , p_raw_string
    ]

{----- (3.10) Identifier String -----}

-- | ref: (3.10)
-- identifier-string := unambiguous-ident | signed-ident | dotted-ident
p_identifier_string :: Parser Text
p_identifier_string = label "unquoted string" $ do
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
p_unambiguous_ident = label "unquoted string" $ do
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
p_signed_ident = do
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
p_dotted_ident = do
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
p_identifier_char = do
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
p_quoted_string = label "quoted string" $ do
  choice
    [ between (string quotes3) (string quotes3) $ do
        parseMultilineString
          p_multi_line_string_body
          ( repeat0 . choice $
              [ Text.singleton <$> p_unicode_space
              , "" <$ p_ws_escape
              ]
          )
    , between (string quotes1) (string quotes1) $ do
        p_single_line_string_body
    ]

-- | ref: (3.11)
-- single-line-string-body := (string-character - newline)*
p_single_line_string_body :: Parser Text
p_single_line_string_body = label "quoted string line" $ do
  repeat0 $ p_string_character (Just p_newline)

-- | ref: (3.11)
-- string-character :=
--     '\\' (["\\bfnrts] |
--     'u{' hex-unicode '}') |
--     ws-escape |
--     [^\\"] - disallowed-literal-code-points
p_string_character :: Maybe (Parser invalid) -> Parser Text
p_string_character invalid = label "string character" $ do
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
p_hex_unicode = label "hex unicode" $ do
  digits <- countBetween 1 6 p_hex_digit
  guard $ (not . null) digits
  let x = undigits 16 digits
  guard $ is_unicode_scalar_value x
  pure x

-- | ref: (3.11.1.1)
-- ws-escape := '\\' (unicode-space | newline)+
p_ws_escape :: Parser ()
p_ws_escape = label "escaped Whitespace" $ do
  char '\\' *> void (some $ void p_unicode_space <|> void p_newline)

{----- (3.12) Multi-line String -----}

-- | ref: (3.12)
-- multi-line-string-body := (('"' | '""')? string-character)*
--
-- Requires some changes to the grammar: https://github.com/kdl-org/kdl/pull/552
p_multi_line_string_body :: Parser end -> Parser MultilineChars
p_multi_line_string_body end = label "quoted multiline string body" $ do
  manyTill (withSource validChar) (try . lookAhead $ end)
 where
  validChar = do
    choice
      [ string "\"\"" <* notFollowedBy (char '"')
      , string "\"" <* notFollowedBy (char '"')
      , p_string_character Nothing
      ]

-- | Characters in a multiline string.
--
-- In simple cases, the input is effectively [(Char, Text)], containing
-- each character in the multiline string and its raw representation. This
-- distinguishes what the user actually wrote vs its semantic value; e.g.
-- '\s' is represented as (" ", "\s").
--
-- It needs to be (Text, Text) instead of (Char, Text) because some character
-- sequences are semantically an empty string (e.g. p_ws_escape) and a few can
-- return multiple characters (e.g. p_newline).
type MultilineChars = [(Text, Text)]

type MultilineProcessorM a = StateT MultilineProcessorState Parser a
data MultilineProcessorState = MultilineProcessorState
  { wsPrefix :: Text
  , lineStartOffset :: Int
  }

-- | Parse a multiline string with the grammar:
--
--     '"""' newline
--     (<body> newline)?
--     <endSpace>* '"""'
parseMultilineString ::
  (forall end. Parser end -> Parser MultilineChars) ->
  Parser Text ->
  Parser Text
parseMultilineString parseBody parseEndSpace = do
  _ <- p_newline -- Drop the first newline
  State{stateOffset = startOffset} <- getParserState
  mBody <- optional . try $ do
    body <- parseBody (p_newline *> parseEndSpace *> string quotes3)
    _ <- p_newline -- Drop the last newline
    pure body
  end <- parseEndSpace
  case mBody of
    Nothing -> pure ""
    Just body -> do
      let state =
            MultilineProcessorState
              { wsPrefix = end
              , lineStartOffset = startOffset
              }
      body' <- flip evalStateT state $ mapLinesM processLine body
      pure $ foldMap fst body'
 where
  processLine = rmPrefix . collapseWsOnlyLines

  collapseWsOnlyLines line =
    let srcs = foldMap snd line
     in if Text.all isSpace srcs
          then [("", srcs)]
          else line

  rmPrefix line0 = do
    let go pre = \case
          -- Consumed the full prefix, return the final line
          line | Text.null pre -> pure line
          -- The prefix starts with the source text; consume and continue matching the rest of the prefix
          (_, src) : rest | Just pre' <- Text.stripPrefix src pre -> go pre' rest
          -- Prefix did not match, return the initial line unchanged
          _ -> do
            offset <- State.gets (.lineStartOffset)
            parseError . FancyError offset . Set.singleton $
              ErrorFail "Line does not have the correct indentation"
    pre0 <- State.gets (.wsPrefix)
    -- If the line is completely empty (e.g. after collapseWsOnlyLines),
    -- there's no prefix to strip.
    if all (Text.null . fst) line0
      then pure line0
      else go pre0 line0

  mapLinesM ::
    (MultilineChars -> MultilineProcessorM MultilineChars) ->
    MultilineChars ->
    MultilineProcessorM MultilineChars
  mapLinesM f =
    let resolveLine (buf, acc) = (acc <>) . Seq.fromList <$> f (Seq.toList buf)
        go (buf, acc) (c, src)
          -- If we're at a newline, `buf` contains the chars before the newline.
          -- Apply the function and reset the buffer. The newline should be added
          -- directly to the accumulator, since the newline isn't part of the line.
          -- Per (3.12.1), newline characters are normalized to LF.
          | c `Set.member` chars_newline = do
              acc' <- resolveLine (buf, acc)
              State.modify $ \s ->
                let lineLen = sum . fmap Text.length $ fmap snd buf Seq.|> src
                 in s{lineStartOffset = s.lineStartOffset + lineLen}
              pure (Seq.empty, acc' Seq.|> ("\n", src))
          -- Otherwise, append to the buffer and continue
          | otherwise = do
              pure (buf Seq.|> (c, src), acc)
     in foldlM go (Seq.empty, Seq.empty)
          -- Resolve line one last time to apply the function on the last line
          >=> resolveLine
          >=> (pure . Seq.toList)

{----- (3.13) Raw String -----}

-- | ref: (3.13)
-- raw-string := '#' raw-string-quotes '#' | '#' raw-string '#'
p_raw_string :: Parser Text
p_raw_string = label "raw string" $ do
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
p_raw_string_quotes end = label "raw string quotes" $ do
  choice
    [ between (string quotes3) (string quotes3) $ do
        parseMultilineString
          p_multi_line_raw_string_body
          (repeat0 $ Text.singleton <$> p_unicode_space)
    , between (string quotes1) (string quotes1) . withSource_ $ do
        p_single_line_raw_string_body (string quotes1 *> end)
    ]

-- | ref: (3.13)
-- single-line-raw-string-body :=
--     '' |
--     (single-line-raw-string-char - '"')
--         single-line-raw-string-char*? |
--     '"' (single-line-raw-string-char - '"')
--         single-line-raw-string-char*?
p_single_line_raw_string_body :: Parser end -> Parser ()
p_single_line_raw_string_body end = label "raw string single line" $ do
  void . optional $ do
    _ <- optional $ char '"'
    c <- p_single_line_raw_string_char
    guard $ c /= '"'
    manyTill p_single_line_raw_string_char (try $ lookAhead end)

-- | ref: (3.13)
-- single-line-raw-string-char :=
--     unicode - newline - disallowed-literal-code-points
p_single_line_raw_string_char :: Parser Char
p_single_line_raw_string_char = label "raw string character" $ do
  c <- p_unicode
  guard $ Text.singleton c `Set.notMember` chars_newline
  guard $ not $ is_disallowed_literal_code_points c
  pure c

-- | ref: (3.13)
-- multi-line-raw-string-body :=
--     (unicode - disallowed-literal-code-points)*?
p_multi_line_raw_string_body :: Parser end -> Parser MultilineChars
p_multi_line_raw_string_body end = label "raw string multiline body" $ do
  manyTill validChar (try . lookAhead $ end)
 where
  validChar = do
    c <- p_unicode
    guard $ not $ is_disallowed_literal_code_points c
    let s = Text.singleton c
    pure (s, s)

{----- (3.14) Number -----}

-- | ref: (3.14)
-- number := keyword-number | hex | octal | binary | decimal
p_number :: Parser ValueData
p_number = label "number" $ do
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
p_hex = label "hex" $ parseNumWith "0x" 16 isHexDigit

-- | ref: (3.14)
-- hex-digit := [0-9a-fA-F]
p_hex_digit :: Parser Int
p_hex_digit = label "hex digit" $ do
  digitToInt <$> satisfy isHexDigit

-- | ref: (3.14)
-- octal := sign? '0o' [0-7] [0-7_]*
p_octal :: Parser Scientific
p_octal = label "octal" $ parseNumWith "0o" 8 isOctDigit

-- | ref: (3.14)
-- binary := sign? '0b' ('0' | '1') ('0' | '1' | '_')*
p_binary :: Parser Scientific
p_binary = label "binary" $ parseNumWith "0b" 2 isBinDigit
 where
  isBinDigit c = c == '0' || c == '1'

parseNumWith :: Text -> Integer -> (Char -> Bool) -> Parser Scientific
parseNumWith prefix base isValid = do
  signed <- parseSigned
  _ <- string prefix
  (x, _) <- p_digits base isValid
  pure . signed . fromInteger $ x

-- | ref: (3.14)
-- decimal := sign? integer ('.' integer)? exponent?
p_decimal :: Parser Scientific
p_decimal = label "decimal number" $ do
  signed <- parseSigned
  (i, _) <- p_integer
  (f, fdigits) <- option (0, 0) $ label "decimal point" (char '.') *> p_integer
  e <- option 0 p_exponent
  pure . signed $ Scientific.scientific (i * (10 ^ fdigits) + f) (e - fdigits)

-- | ref: (3.14)
-- integer := digit (digit | '_')*
p_integer :: Parser (Integer, Int)
p_integer = label "integer" $ p_digits 10 isDigit

p_digits :: Integer -> (Char -> Bool) -> Parser (Integer, Int)
p_digits base isValid = do
  d <- p_digit
  ds <-
    fmap catMaybes . many . choice $
      [ Just <$> p_digit
      , Nothing <$ hidden (char '_')
      ]
  let digits = map toInteger (d : ds)
  pure (undigits base digits, length digits)
 where
  p_digit = digitToInt <$> satisfy isValid

-- | ref: (3.14)
-- exponent := ('e' | 'E') sign? integer
p_exponent :: Parser Int
p_exponent = label "exponent" $ do
  _ <- oneOf ['e', 'E']
  signed <- parseSigned
  (x, _) <- p_integer
  case toIntegralSized x of
    Just x' -> pure $ signed x'
    Nothing -> fail $ "Exponent is too large: " <> show x

-- | ref: (3.14)
-- sign := '+' | '-'
p_sign :: Parser Char
p_sign = label "sign" $ do
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
p_keyword_number = do
  choice
    [ Inf <$ string "#inf"
    , NegInf <$ string "#-inf"
    , NaN <$ string "#nan"
    ]

{----- (3.15) Boolean -----}

-- | ref: (3.15)
-- boolean := '#true' | '#false'
p_boolean :: Parser Bool
p_boolean = label "boolean" $ do
  choice
    [ True <$ string "#true"
    , False <$ string "#false"
    ]

{----- (3.17) Whitespace -----}

-- | ref: (3.17)
-- ws := unicode-space | multi-line-comment
p_ws :: Parser Text
p_ws = label "whitespace" $ do
  (Text.singleton <$> p_unicode_space) <|> p_multi_line_comment

-- | ref: (3.17)
-- unicode-space := See Table
--     (All White_Space unicode characters which are not `newline`)
p_unicode_space :: Parser Char
p_unicode_space = label "unicode space" $ do
  choice
    [ char c <?> l
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
p_single_line_comment = label "single-line comment" $ do
  withSource_ $ do
    _ <- string "//"
    _ <- takeWhileP Nothing (/= '\n')
    _ <- p_newline <|> p_eof
    pure ()

-- | ref: (3.17.2)
-- multi-line-comment := '/*' commented-block
p_multi_line_comment :: Parser Text
p_multi_line_comment = label "multi-line comment" . withSource_ $ do
  string "/*" *> p_commented_block

-- | ref: (3.17.2)
-- commented-block :=
--     '*/' | (multi-line-comment | '*' | '/' | [^*/]+) commented-block
p_commented_block :: Parser ()
p_commented_block = label "commented block" $ do
  choice
    [ void $ string "*/"
    , do
        choice
          [ void p_multi_line_comment
          , void $ char '*'
          , void $ char '/'
          , void $ takeWhileP Nothing (`notElem` ['*', '/'])
          ]
        p_commented_block
    ]

data SlashdashResult = Slashdash Text | NoSlashdash

resolveSlashdash :: SlashdashResult -> Parser a -> Parser (Either Text a)
resolveSlashdash = \case
  Slashdash sdash -> fmap (Left . (sdash <>)) . withSource_
  NoSlashdash -> fmap Right

-- | ref: (3.17.3)
-- slashdash := '/-' line-space*
p_slashdash :: Parser SlashdashResult
p_slashdash = hidden $ do
  sdash <- withSource_ $ string "/-" *> many p_line_space
  pure $ Slashdash sdash

{----- (3.18) Newline -----}

-- | ref: (3.18)
-- newline := See Table (All Newline White_Space)
p_newline :: Parser Text
p_newline = label "newline" $ do
  choice [string s <?> l | (s, l) <- chars_newline']

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
p_unicode = label "unicode scalar value" $ do
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

repeat0 :: (Monoid a) => Parser a -> Parser a
repeat0 = fmap mconcat . many

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
p_eof = hidden $ "" <$ eof

undigits :: (Num a) => a -> [a] -> a
undigits base = foldl' (\acc x -> acc * base + x) 0

-- | Group all 'Left' values and put it in the leading whitespace of the next
-- element. Return any leftover whitespace (i.e. the list ends with 'Left'
-- values).
mergeLeadingWS :: (HasWsFormat a) => Text -> [Either Text a] -> ([a], Text)
mergeLeadingWS initialLeading =
  bimap Seq.toList toText . foldl' go (Seq.empty, Seq.singleton initialLeading)
 where
  toText = Text.concat . Seq.toList
  go (nodes, buf) = \case
    Left t -> (nodes, buf Seq.|> t)
    Right node -> (nodes Seq.|> prependLeading (toText buf) node, Seq.empty)

class HasFormat a where
  type KdlFormat a
  mapFormat :: (KdlFormat a -> KdlFormat a) -> a -> a
instance HasFormat NodeList where
  type KdlFormat NodeList = NodeListFormat
  mapFormat f NodeList{..} = NodeList{ext = ext{NodeListExtension.format = f <$> ext.format}, ..}
instance HasFormat Node where
  type KdlFormat Node = NodeFormat
  mapFormat f Node{..} = Node{ext = ext{NodeExtension.format = f <$> ext.format}, ..}
instance HasFormat Ann where
  type KdlFormat Ann = AnnFormat
  mapFormat f Ann{..} = Ann{ext = ext{AnnExtension.format = f <$> ext.format}, ..}
instance HasFormat Entry where
  type KdlFormat Entry = EntryFormat
  mapFormat f Entry{..} = Entry{ext = ext{EntryExtension.format = f <$> ext.format}, ..}

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
