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

-- FUNCOES DE CADASTRO DE USUARIO

--todas as matriculas dos usuarios
todasMatriculas :: [Usuario] -> [String]
todasMatriculas = map matricula

--verifica se um usuario esta na lista pela matricula
matriculaPertenceLista :: Usuario -> [Usuario] -> Bool
matriculaPertenceLista usuario = elem (matricula usuario) . todasMatriculas

--verifica se uma matricula existe na lista
matriculaExiste :: String -> [Usuario] -> Bool
matriculaExiste mat = any (\u -> matricula u == mat)

-- verifica se um email existe na lista
emailExiste :: String -> [Usuario] -> Bool
emailExiste emailDado = any (\u -> email u == emailDado)

--adiciona um usuario se a matricula for unica
adicionarUsuario :: Usuario -> [Usuario] -> [Usuario]
adicionarUsuario usuario lista
    | matriculaPertenceLista usuario lista = lista
    | otherwise = usuario : lista



