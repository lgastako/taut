{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Taut.Types.Message.Attachment.Action
       ( ButtonStyle(Default, Primary, Danger)
       , ActionType(Button)
       , Action(Action)
       , confirm
       , name
       , text
       , type'
       , style
       , value
       , button
       )
       where

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

data ButtonStyle = Default
                 | Primary
                 | Danger
  deriving (Generic, Show)

instance ToJSON ButtonStyle where
  toJSON = genericToJSON customUnionTypeOptions

instance FromJSON ButtonStyle where
  parseJSON = genericParseJSON customUnionTypeOptions

customUnionTypeOptions :: Options
customUnionTypeOptions = defaultOptions
  { constructorTagModifier = fmap toLower
  }

data ActionType = Button
  deriving (Generic, Show)

instance ToJSON ActionType where
  toJSON _ = Aeson.String "button"

instance FromJSON ActionType where
  parseJSON (Aeson.String s) = case s of
    "button" -> return Button
    e -> fail $ "Invalid ActionType: " ++ Text.unpack e
  parseJSON invalid = typeMismatch "ActionType" invalid

data Action = Action
  { _confirm :: Maybe Confirm
  , _name    :: Text
  , _text    :: Maybe Text
  , _type'   :: ActionType
  , _style   :: Maybe ButtonStyle
  , _value   :: Text
  } deriving (Generic, Show)

makeLenses ''Action

button :: Text -> Text ->  Text -> Action
button name' text' value' = Action
  { _confirm = Nothing
  , _name = name'
  , _text = Just text'
  , _type' = Button
  , _style = Nothing
  , _value = value'
  }

instance ToJSON Action where
  toJSON = genericToJSON customActionOptions

instance FromJSON Action where
  parseJSON = withObject "Action" $ \v -> Action
        <$> v .:?? "confirm"
        <*> v .:   "name"
        <*> v .:?? "text"
        <*> v .:   "type"
        <*> v .:?? "style"
        <*> v .:   "value"

customActionOptions :: Options
customActionOptions = defaultOptions
  { fieldLabelModifier = camelTo2 '_' . drop 1 . filter (/= '\'')
  , omitNothingFields = True
  }
