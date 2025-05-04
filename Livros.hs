module Livros where

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



