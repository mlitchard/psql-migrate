{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Database.PostgreSQL.Simple.Migrate.Internal.Wrap
-- Description : Wrap an error message.
-- Copyright   : (c) Brian Hurt, 2023
-- License     : None
-- Maintainer  : Brian Hurt <bhurt42@gmail.com>
-- Stability   : experimental
--
-- This is an internal module of "Database.PostgreSQL.Simple.Migrate",
-- you probably want that module instead.  Anything exported by this
-- module that is not also exported by the main module is subject
-- to change without notice.
--
-- Rather than horsing around with some fancy pretty printer, when
-- all I want to do is just wrap the error messages in a nice-ish
-- way, I just wrote my own wrap function.
--
module Database.PostgreSQL.Simple.Migrate.Internal.Wrap (
    wrap
) where

    import           Data.Char                    (isSpace)
    import qualified Data.List                    as List
    import qualified System.Console.Terminal.Size as Term

    wrap :: String -> IO String
    wrap [] = pure []
    wrap msg = do
        mwin :: Maybe (Term.Window Int) <- Term.size
        let width :: Int
            width = case mwin of
                        Nothing -> 70
                        Just win ->
                            let w = Term.width win in
                            w - (w `quot` 10)
        pure $ doWrap width msg

    doWrap :: Int -> String -> String
    doWrap lineLen msg =
            -- Note: List.unlines does the wrong thing (it adds a newline
            -- to the end of the string).  So we use intercalate instead.
            List.intercalate "\n" $ List.lines msg >>= wrapLine

        where
            -- | Break a single line into multiple lines.
            --
            -- Attempting to keep the line length less than lineLen.
            wrapLine :: String -> [ String ]
            wrapLine []   = []
            wrapLine line =
                case findBreak line of
                    Nothing ->
                        -- Everything fits on the current line
                        [ line ]
                    Just b ->
                        let hd :: String
                            tl :: String
                            (hd, tl) = List.splitAt b line
                        in
                        hd : wrapLine (List.dropWhile isSpace tl)


            -- | Find the break point.
            --
            -- The most words we can fit on the line without going
            -- over lineLen.  Returns Nothing if the full string
            -- fits on a line.
            findBreak :: String -> Maybe Int
            findBreak = leadingSpace 0

            -- Notice that the following set of functions are basically
            -- one big hand-rolled finite automaton.  Each function
            -- is a state in the automaton, and it transitions to
            -- another state via tail-calls.

            leadingSpace :: Int -> String -> Maybe Int
            leadingSpace _  [] = Nothing
            leadingSpace !n (x : xs)
                | isSpace x   = leadingSpace   (n+1) xs
                | (x == '\\') = leadingWordEsc (n+1) xs
                | (x == '"')  = leadingQuote   (n+1) xs
                | otherwise   = leadingWord    (n+1) xs

            leadingWord :: Int -> String -> Maybe Int
            leadingWord _  [] = Nothing
            leadingWord !n (x:xs)
                | isSpace x = breakSpace n   (n+1) xs
                | x == '\\' = leadingWordEsc (n+1) xs
                | x == '"'  = leadingQuote   (n+1) xs
                | otherwise = leadingWord    (n+1) xs

            leadingWordEsc :: Int -> String -> Maybe Int
            leadingWordEsc _  []     = Nothing
            leadingWordEsc !n (_:xs) = leadingWord (n+1) xs

            leadingQuote :: Int -> String -> Maybe Int
            leadingQuote _ [] = Nothing
            leadingQuote !n (x:xs)
                | x == '\\' = leadingQuoteEsc (n+1) xs
                | x == '"'  = leadingWord     (n+1) xs
                | otherwise = leadingQuote    (n+1) xs

            leadingQuoteEsc :: Int -> String -> Maybe Int
            leadingQuoteEsc _  []     = Nothing
            leadingQuoteEsc !n (_:xs) = leadingQuote (n+1) xs

            breakSpace :: Int -> Int -> String -> Maybe Int
            breakSpace b _  []     = Just b
            breakSpace b !n (x:xs)
                | n > lineLen      = Just b
                | isSpace x        = breakSpace   b (n+1) xs
                | x == '\\'        = breakWordEsc b (n+1) xs
                | x == '"'         = breakQuote   b (n+1) xs
                | otherwise        = breakWord    b (n+1) xs

            breakWord :: Int -> Int -> String -> Maybe Int
            breakWord b !n []
                | n > lineLen       = Just b
                | otherwise         = Nothing
            breakWord b !n (x:xs)
                | n > lineLen       = Just b
                | isSpace x         = breakSpace   n (n+1) xs
                | x == '\\'         = breakWordEsc b (n+1) xs
                | x == '"'          = breakQuote   b (n+1) xs
                | otherwise         = breakWord    b (n+1) xs

            breakWordEsc :: Int -> Int -> String -> Maybe Int
            breakWordEsc b !n []
                | n > lineLen       = Just b
                | otherwise         = Nothing
            breakWordEsc b !n (_:xs)
                | n > lineLen       = Just b
                | otherwise         = breakWord b (n+1) xs

            breakQuote :: Int -> Int -> String -> Maybe Int
            breakQuote b !n []
                | n > lineLen       = Just b
                | otherwise         = Nothing
            breakQuote b !n (x:xs)
                | n > lineLen       = Just b
                | x == '"'          = breakWord     b (n+1) xs
                | x == '\\'         = breakQuoteEsc b (n+1) xs
                | otherwise         = breakQuote    b (n+1) xs

            breakQuoteEsc b !n []
                | n > lineLen       = Just b
                | otherwise         = Nothing
            breakQuoteEsc b !n (_:xs)
                | n > lineLen       = Just b
                | otherwise         = breakQuote b (n+1) xs
