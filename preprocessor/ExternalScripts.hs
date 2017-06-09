module ExternalScripts where


import           Control.Applicative
import           Data.Aeson          hiding (object)
import           Data.Monoid         ((<>))


data ExternalScript
    = ModuleOnly String
    | ModuleAndScripts String [String]


getModule :: ExternalScript -> String
getModule (ModuleOnly m)         = m
getModule (ModuleAndScripts m _) = m


getScripts :: ExternalScript -> [String]
getScripts (ModuleOnly m)          = [m <> ".script"]
getScripts (ModuleAndScripts m xs) = [m <> "." <> s | s <- xs]


instance FromJSON ExternalScript where
    parseJSON v = (ModuleOnly <$> parseJSON v) <|> (parseJSON v >>= fromArr) <|> (parseJSON v >>= fromObj)
      where
        fromArr (x:xs) = pure $ ModuleAndScripts x xs -- ENHANCEMENT verification for module names?
        fromArr _      = fail "Array form for external script needs at least one element"

        fromObj o = do
            module_ <- o .: "module"
            ModuleAndScripts module_
                <$> (return <$> o .: "script"
                    <|> o .: "scripts")
