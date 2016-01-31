module Message where


data Message = Message {
  m :: String
  }

deserialize :: String -> Message
deserialize = Message

serialize :: Message -> String
serialize = m
