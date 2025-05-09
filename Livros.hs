module Livros where

import Data.List (any,find)
import Tipos
-- Funcoes de cadastro de livros:

-- funcao auxiliar que retorna a lista de todos os ids de uma lista
todosIds :: [Livro] -> [Int] 
todosIds lista_livros = map idLivro lista_livros


--checa se o id de um livro ja pertence a uma lista
idPertenceLista :: Livro -> [Livro] -> Bool
idPertenceLista livro lista_livros = (elem (idLivro livro) (todosIds lista_livros)) 


-- Imprime uma mensagem dizendo se a adicao de livro na lista foi bem sucedida ou nao
validarAdicao :: Livro -> [Livro] -> IO ()
validarAdicao livro lista_livros = do
                if idPertenceLista livro lista_livros -- checa se ja existe um id na lista igual ao do livro tentando ser adicionado
                   then putStrLn "Erro! esse id ja foi cadastrado por outro livro, lista inalterada." -- caso sim
                   else putStrLn "Lista atualizada com sucesso!" -- caso nao

-- adiciona elementos a lista
adicionarLivro :: Livro -> [Livro] -> [Livro]
adicionarLivro livro lista_livros = 
     if (idPertenceLista livro lista_livros) -- compara se o id do novo livro ja pertence a lista
       then lista_livros -- se o id do novo livro ja existir na lista, retorna a mesma lista
       else (livro : lista_livros) -- coloca a novo livro no comeco da lista de livros

-- Imprime mensagemm dizendo se a remocao foi bem sucedida ou nao
validarRemocao :: Int -> [Livro] -> IO ()
validarRemocao id lista = do
    if any (\l -> idLivro l == id) lista
        then do
            putStrLn "Livro encontrado e removido"
        else do
            putStrLn "Erro! Livro nao encontrado"

-- funcao pura para remover livro
removerLivro :: Int -> [Livro] -> [Livro]
removerLivro id = filter (\l -> idLivro l /= id) --filter para remover os livros com mesmo id

-- Filtra livros por autor 
filtrarPorAutor :: String -> [Livro] -> [Livro]
filtrarPorAutor autorBusca = filter (\livro -> autor livro == autorBusca)

--Filtra livros por ano
filtrarPorAno :: Integer -> [Livro] -> [Livro]
filtrarPorAno anoBusca = filter (\livro -> ano livro == anoBusca)

--Filtra livros por titulo 
filtrarPorTitulo :: String -> [Livro] -> [Livro]
filtrarPorTitulo tituloBusca = filter (\livro -> titulo livro == tituloBusca)

-- Filtra livros pelo id
filtrarPorId :: Int -> [Livro] -> [Livro]
filtrarPorId id = filter (\l -> idLivro l == id)

alteraNome :: Livro -> String -> [Livro] -> [Livro]
alteraNome _ _ [] = []
alteraNome alvo novo_titulo lista_livros = map (\l -> if l == alvo then l { titulo = novo_titulo } else l) lista_livros

alteraAutor :: Livro -> String -> [Livro] -> [Livro]
alteraAutor _ _ [] = []
alteraAutor alvo novo_autor lista_livros = map (\l -> if l == alvo then l { autor = novo_autor } else l) lista_livros

alteraAno :: Livro -> Integer -> [Livro] -> [Livro]
alteraAno _ _ [] = []
alteraAno alvo novo_ano lista_livros = map (\l -> if l == alvo then l { ano = novo_ano } else l) lista_livros

