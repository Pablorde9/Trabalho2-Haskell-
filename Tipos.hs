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
     , usuarioE              :: Usuario
     , devolvido             :: Bool
     } deriving (Show, Eq)

data Devolucao = Devolucao
     { dataDevolucao   :: DiaMesAno
     , livroD          :: Livro
     , usuarioD        :: Usuario
     } deriving (Show, Eq)

data Espera = Espera
     { usuarioEsp   :: Usuario
     , livroEsp     :: Livro
     } deriving (Show, Eq)
     
     
-- Livros de exemplo
livro1 :: Livro
livro1 = Livro 
    { titulo = "Dom Quixote"
    , autor = "Miguel de Cervantes"
    , ano = 1605
    , idLivro = 1
    }

livro2 :: Livro
livro2 = Livro 
    { titulo = "1984"
    , autor = "George Orwell"
    , ano = 1949
    , idLivro = 2
    }

livro3 :: Livro
livro3 = Livro 
    { titulo = "O Senhor dos Aneis"
    , autor = "J.R.R. Tolkien"
    , ano = 1954
    , idLivro = 3
    }

-- Usuários de exemplo
usuario1 :: Usuario
usuario1 = Usuario 
    { nome = "Joao Silva"
    , matricula = "2023A001"
    , email = "joao@email.com"
    }

usuario2 :: Usuario
usuario2 = Usuario 
    { nome = "Maria Souza"
    , matricula = "20230B02"
    , email = "maria@email.com"
    }

-- Empréstimos de exemplo
emprestimo1 :: Emprestimo
emprestimo1 = Emprestimo 
    { dataEmprestimo = (15, 4, 2023)
    , livroE = livro1
    , usuarioE = usuario1
    , devolvido = False
    }

emprestimo2 :: Emprestimo
emprestimo2 = Emprestimo 
    { dataEmprestimo = (20, 5, 2023)
    , livroE = livro2
    , usuarioE = usuario2
    , devolvido = True
    }

-- Devoluções de exemplo
devolucao1 :: Devolucao
devolucao1 = Devolucao 
    { dataDevolucao = (25, 5, 2023)
    , livroD = livro2
    , usuarioD = usuario2
    }

-- Listas de espera de exemplo
espera1 :: Espera
espera1 = Espera 
    { usuarioEsp = usuario1
    , livroEsp = livro3
    }

espera2 :: Espera
espera2 = Espera 
    { usuarioEsp = usuario2
    , livroEsp = livro1
    }

-- Coleções completas para teste
livrosTeste :: [Livro]
livrosTeste = [livro1, livro2, livro3]

usuariosTeste :: [Usuario]
usuariosTeste = [usuario1, usuario2]

emprestimosTeste :: [Emprestimo]
emprestimosTeste = [emprestimo1, emprestimo2]

devolucoesTeste :: [Devolucao]
devolucoesTeste = [devolucao1]

esperasTeste :: [Espera]
esperasTeste = [espera1, espera2]
