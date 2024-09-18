module Web.ValidateForm where


import ClassyPrelude
import Test.Tasty.Options (safeRead)
import Model
import qualified Data.Map as M



validateCreateForm :: Text -> Text -> String -> SnippetCreateForm
validateCreateForm title content expires = 
  let form = defaultSnippetCreateForm 
        { scfTitle = Just title
        , scfContent = Just content
        , scfFieldErrors = validateTitle title ++ validateContent content
        }
    in case validateExpires expires of
      Left expiresErr -> form { scfFieldErrors = scfFieldErrors form ++ expiresErr}
      Right n -> form { scfExpires = n}


validateTitle :: Text -> Map SnippetField [SnippetFormValidationError]
validateTitle title = check (length title)
  where
    check n
      | n == 0 = M.singleton SnippetFieldTitle [FieldCannotBeBlank]
      | n > 100 = M.singleton SnippetFieldTitle [FieldCannotBeMoreThan100CharactersLong]
      | otherwise = mempty

validateContent :: Text -> Map SnippetField [SnippetFormValidationError]
validateContent "" = M.singleton SnippetFieldContent [FieldCannotBeBlank]
validateContent _ = mempty

validateExpires :: String -> Either (Map SnippetField [SnippetFormValidationError]) Int
validateExpires nstr = 
  case safeRead nstr :: Maybe Int of
    Nothing -> Left $ M.singleton SnippetFieldExpires [FieldMustEqual1Or7Or365]
    Just n 
      | n == 1 || n == 7 || n == 365 -> Right n
      | otherwise -> Left $ M.singleton SnippetFieldExpires [FieldMustEqual1Or7Or365]
