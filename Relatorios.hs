module Relatorios where

import Data.List
import Tipos

-- lista todos emprestimos ativos (n devolvidos)
emprestimosAtivos :: [Emprestimo] -> [Emprestimo]
emprestimosAtivos = filter (not .devolvido )

-- mostra o historico de emprestimo de um usuario especifico pela sua matricula
historicoUsuario :: Usuario -> [Emprestimo] -> [Emprestimo]
historicoUsuario usuario = filter (\e -> usuarioE e == usuario)

-- livros com a lista de espera nao vazia e os usuarios
livrosComEspera :: [(Int, [Usuario])] -> [Livro] -> [(Livro, [Usuario])]
livrosComEspera listaEspera livros =  [(livro, usuarios) | (idLiv, usuarios) <- listaEspera, not (null usuarios), Just livro <- [ find (\l -> idLivro l == idLiv) livros]]
-- verifica se existe usuarios e procura os livros com base no id da lista de espera

-- livros disponiveis
livrosDisponiveis :: [Livro] -> [Emprestimo] -> [Livro]
livrosDisponiveis livros emprestimos = filter (\livro -> not $ any (\e -> idLivro (livroE e) == idLivro livro && not (devolvido e)) emprestimos) livros

--livros emprestados
livrosEmprestados :: [Livro] -> [Emprestimo] -> [Livro]
livrosEmprestados livros emprestimos = filter (\livro -> any (\e -> idLivro (livroE e) == idLivro livro && not (devolvido e)) emprestimos) livros
