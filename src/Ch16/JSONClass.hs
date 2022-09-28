module Ch16.JSONClass where


data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JObject (JObj JValue)
            | JArray (JAry JValue)
            deriving (Eq, Ord, Show)

newtype JAry a = JAry {
    fromJAry :: [a]
    } deriving (Eq, Ord, Show)

newtype JObj a = JObj {
    fromJObj :: [(String, a)]
    } deriving (Eq, Ord, Show)
