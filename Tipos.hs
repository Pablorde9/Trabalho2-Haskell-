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

type DiaMesAno = (Int, Int, Integer)

data Emprestimo = Emprestimo
     { dataEmprestimo        :: DiaMesAno
     , livroE                :: Livro
     , usuarioE              :: String
     , devolvido             :: Bool
     } deriving (Show, Eq)

data Devolucoes = Devolucoes
     { dataDevolucao   :: DiaMesAno
     , livroD          :: Livro
     , usuarioD        :: Usuario
     } deriving (Show, Eq)
