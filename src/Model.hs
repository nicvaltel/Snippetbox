module Model where

import ClassyPrelude
import Data.Time (Year)

type SnippetId = Int

data Snippet = Snippet
  { snippetId :: SnippetId
  , snippetTitle :: Text
  , snippetContent :: Text
  , snippetCreated :: UTCTime
  , snippetExpires :: UTCTime
  } deriving (Show, Eq, Ord)

data SnippetField =
    SnippetFieldTitle
  | SnippetFieldContent
  | SnippetFieldExpires
  deriving (Show, Eq, Ord)

data SnippetCreateForm = SnippetCreateForm
  { scfTitle :: Maybe Text
  , scfContent :: Maybe Text
  , scfExpires :: Int
  , scfFieldErrors :: Map SnippetField [SnippetFormValidationError]
  } deriving (Show, Eq, Ord)

defaultSnippetCreateForm :: SnippetCreateForm
defaultSnippetCreateForm = SnippetCreateForm
  { scfTitle = Nothing
  , scfContent = Nothing
  , scfExpires = 365
  , scfFieldErrors = mempty
  }

data SnippetFormValidationError =
    FieldCannotBeBlank
  | FieldCannotBeMoreThan100CharactersLong
  | FieldMustEqual1Or7Or365
  deriving (Eq, Ord)


instance Show SnippetFormValidationError where
  show FieldCannotBeBlank = "This field cannot be blank"
  show FieldCannotBeMoreThan100CharactersLong = "This field cannot be more than 100 characters long"
  show FieldMustEqual1Or7Or365 = "This field must equal 1, 7 or 365"

newtype CommonAppData = CommonAppData
  { cadCurrentYear :: Year -- Year = Integer
  } deriving (Show, Eq, Ord)


class Monad m => SnippetsRepo m where
  insertSnippet :: Snippet -> m (Maybe SnippetId)
  getSnippet ::  SnippetId -> m (Maybe Snippet)
  latestSnippets :: m [Snippet]

class Monad m => CommonData m where
  getCurrentYear :: m Year