{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
module PostgreSQL.Snippetbox where


import ClassyPrelude
import Database.PostgreSQL.Simple
import PostgreSQL.Common
import Model
import Katip


insertSnippet :: (PG r m, KatipContext m) => Snippet -> m (Maybe SnippetId)
insertSnippet Snippet{snippetTitle, snippetContent, snippetCreated, snippetExpires} = do
  res <- withConn $ \con -> (query con stmt (snippetTitle, snippetContent, snippetCreated, snippetExpires) :: IO [Only SnippetId])
  case res of
    [Only sId] -> pure (Just sId)
    _ -> do
      $(logTM) ErrorS $ ls ("Should not happen: PG doesn't return snippetId" :: Text)
      pure Nothing
  where
    stmt = 
      "INSERT INTO snippets (title, content, created, expires) \
      \VALUES(?, ?, (now() at time zone 'utc'), (SELECT CURRENT_DATE + INTERVAL '? day')) returning id"


getSnippet :: (PG r m, KatipContext m) => SnippetId -> m (Maybe Snippet)
getSnippet snippetId = do
  res <- withConn $ \con -> (query con stmt (Only snippetId) :: IO [(Text, Text, UTCTime, UTCTime)])
  case res of
    [(snippetTitle, snippetContent, snippetCreated, snippetExpires)] -> 
      pure (Just Snippet{snippetId, snippetTitle, snippetContent, snippetCreated, snippetExpires})
    _ -> do
      $(logTM) InfoS $ ls ("getSnippet for non existing snippetId: " <> tshow snippetId :: Text)
      pure Nothing
  where
    stmt = 
      "SELECT title, content, created, expires FROM snippets \
      \WHERE expires > now() AND id = ?"


latestSnippets :: (PG r m, KatipContext m) => m [Snippet]
latestSnippets = pure []


