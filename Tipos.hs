module Tipos where


data Livro = Livro
     { titulo  :: String
     , autor   :: String
     , ano     :: Integer
     , idLivro :: Int
     } deriving (Show, Eq)

data Usuario = Usuario
     { nome      :: String
     , matricula :: String
     , email     :: String
     } deriving (Show, Eq)
