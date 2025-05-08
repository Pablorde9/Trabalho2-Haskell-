module Relatorios where

import Data.List
import Tipos

-- lista todos emprestimos ativos (n devolvidos)
emprestimosAtivos :: [Emprestimo] -> [Emprestimo]
emprestimosAtivos = filter (not .devolvido )

-- mostra o historico de emprestimo de um usuario especifico pela sua matricula
historicoUsuario :: String -> [Emprestimo] -> [Emprestimo]
historicoUsuario matricula = filter (\e -> usuarioE e == matricula)

-- livros com a lista de espera nao vazia e os usuarios
livrosComEspera :: [(Int, [Usuario])] -> [Livro] -> [(Livro, [usuario])]
livrosComEspera listaEspera livros =  [(livro, usuarios) | (idLivro, usuarios) <- listasEspera, not (null usuarios), Just livro <- [ find (\l -> idLivro l == idLivro) livros]]
-- verifica se existe usuarios e procura os livros com base no id da lista de espera

