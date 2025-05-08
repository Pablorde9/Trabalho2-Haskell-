module Usuarios where

import Data.List
import Tipos

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

-- funcao para validar a adicao de um usuario
validarAdicaoUsuario :: Usuario -> [Usuario] -> IO ()
validarAdicaoUsuario usuario lista = do
    if matriculaPertenceLista usuario lista
        then putStrLn "Erro! Matricula ja cadastrada."
    else if emailExiste (email usuario) lista
        then putStrLn "Erro! Email ja cadastrado."
        else putStrLn "Usuario adicionado com sucesso!"

--adiciona um usuario se a matricula for unica
adicionarUsuario :: Usuario -> [Usuario] -> [Usuario]
adicionarUsuario usuario lista
    | matriculaPertenceLista usuario lista = lista
    | otherwise = usuario : lista

--valida a remocao de um usuario
validarRemocaoUsuario :: String -> [Usuario] -> IO ()
validarRemocaoUsuario mat lista = do
    if matriculaExiste mat lista
        then putStrLn "Usuario encontrado e removido"
        else putStrLn "Erro! Usuario nao encontrado."

--remove um usuario da lista pela matricula dele
removerUsuario :: String -> [Usuario] -> [Usuario]
removerUsuario mat = filter (\u -> matricula u /= mat)

-- filtra usuarios por nome
filtrarPorNome :: String -> [Usuario] -> [Usuario]
filtrarPorNome nomeBusca = filter (\usuario -> nome usuario == nomeBusca)

--filtra usuarios por email
filtrarPorEmail :: String -> [Usuario] -> [Usuario]
filtrarPorEmail emailBusca = filter (\usuario -> email usuario == emailBusca)

-- ordena usuarios por nome
ordenarPorNome :: [Usuario] -> [Usuario]
ordenarPorNome = sortOn nome

-- ordena usuarios por matricula 
ordenarPorMatricula :: [Usuario] -> [Usuario]
ordenarPorMatricula = sortOn matricula

