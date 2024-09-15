module Model where

import ClassyPrelude

type SnippetId = Int

data Snippet = Snippet
  { snippetId :: SnippetId
  , snippetTitle :: Text
  , snippetContent :: Text
  , snippetCreated :: UTCTime
  , snippetExpires :: UTCTime
  } deriving (Show, Eq, Ord)



class Monad m => SnippetsRepo m where
  insertSnippet :: Snippet -> m (Maybe SnippetId)
  getSnippet ::  SnippetId -> m (Maybe Snippet)
  latestSnippets :: m [Snippet]