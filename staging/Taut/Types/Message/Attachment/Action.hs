{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module Taut.Types.Message.Attachment.Action
       ( Action( Action )
       , ActionType( Button )
       , ButtonStyle( Default
                    , Primary
                    , Danger
                    )
       , button
       , confirm
       , name
       , style
       , text
       , type'
       , value
       ) where

import           Control.Applicative                                   ( (<|>) )
import           Control.Lens                                          ( makeLenses )
import           Control.Monad                                         ( fail )
import           Data.Aeson                                            ( (.:)
                                                                       , (.:?)
                                                                       , FromJSON
                                                                       , FromJSON( parseJSON )
                                                                       , Object
                                                                       , ToJSON( toJSON )
                                                                       , defaultOptions
                                                                       , genericParseJSON
                                                                       , genericToJSON
                                                                       , withObject
                                                                       )
import qualified Data.Aeson                                   as Aeson
import           Data.Aeson.Types                                      ( Options( constructorTagModifier
                                                                                , fieldLabelModifier
                                                                                , omitNothingFields
                                                                                )
                                                                       , camelTo2
                                                                       , typeMismatch
                                                                       )
import           Data.Aeson.Types                                      ( Parser )
import           Data.Char                                             ( toLower )
import           Data.Text                                             ( Text )
import qualified Data.Text                                    as Text
import           GHC.Generics                                          ( Generic )
import           Taut.Types.Message.Attachment.Action.Confirm          ( Confirm )

(.:??) :: FromJSON a => Object -> Text -> Parser (Maybe a)
(.:??) v t = v .:? t <|> return Nothing

data ButtonStyle
  = Danger
  | Default
  | Primary
  deriving (Enum, Eq, Ord, Generic, Read, Show)

instance FromJSON ButtonStyle where
  parseJSON = genericParseJSON buttonStyleOptions

instance ToJSON ButtonStyle where
  toJSON = genericToJSON buttonStyleOptions

buttonStyleOptions :: Options
buttonStyleOptions = defaultOptions
  { constructorTagModifier = fmap toLower
  }

data ActionType = Button
  deriving (Eq, Generic, Ord, Read, Show)

instance FromJSON ActionType where
  parseJSON (Aeson.String s) = case s of
    "button" -> return Button
    e -> fail $ "Invalid ActionType: " ++ Text.unpack e
  parseJSON invalid = typeMismatch "ActionType" invalid

instance ToJSON ActionType where
  toJSON _ = Aeson.String "button"

data Action = Action
  { _confirm :: Maybe Confirm
  , _name    :: Text
  , _style   :: Maybe ButtonStyle
  , _text    :: Maybe Text
  , _type'   :: ActionType
  , _value   :: Text
  } deriving (Eq, Generic, Ord, Read, Show)

makeLenses ''Action

button :: Text -> Text ->  Text -> Action
button name' text' value' = Action
  { _confirm = Nothing
  , _name    = name'
  , _style   = Nothing
  , _text    = Just text'
  , _type'   = Button
  , _value   = value'
  }

instance FromJSON Action where
  parseJSON = withObject "Action" $ \v -> Action
        <$> v .:?? "confirm"
        <*> v .:   "name"
        <*> v .:?? "style"
        <*> v .:?? "text"
        <*> v .:   "type"
        <*> v .:   "value"

instance ToJSON Action where
  toJSON = genericToJSON defaultOptions
    { fieldLabelModifier = camelTo2 '_' . drop 1 . filter (/= '\'')
    , omitNothingFields  = True
    }
