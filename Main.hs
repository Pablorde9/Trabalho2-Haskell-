module Main where

import Livros
import Tipos
import Usuarios
import Relatorios
import System.IO


exibirOpçoes :: [String] -> IO ()
exibirOpçoes opçoes = do
    putStrLn "========================================"
    putStrLn "Menu Principal"
    putStrLn "========================================"
    putStrLn "Opções:"
    putStrLn ""
    putStrLn (unlines opçoes)
    putStrLn ""

lerOpçao :: Int -> Int -> IO Int
lerOpçao minima maxima = do putStr "Digite uma opção: "
                            opçao <- readLn
                            valida opçao
         where
           valida opçao
               | minima <= opçao && opçao <= maxima = return opçao
               | otherwise = do putStrLn "Opção inválida!"
                                putStrLn ""
                                putStr "Digite uma opção: "
                                opçao <- readLn
                                valida opçao

prompt :: Read a => String -> IO a
prompt mensagem = do putStr mensagem
                     readLn
menu :: IO Int
menu = do exibirOpçoes texto
          lerOpçao opMin opMax
  where
    texto = [ "1 - Cadastrar livros",
              "2 - Cadastrar usuários",
              "3 - Empréstimo e devolução",
              "4 - Relatórios",
              "5 - Editar Livro",
              "6 - Editar usuário",
              "7 - Salvar e Sair"
            ]
    opMin = 1
    opMax = 7

subMenu :: String -> Int -> Int -> IO Int
subMenu str min max = do 
                         exibirOpçoes str
                         lerOpçao min max


laçoSubMenu1 :: [Livro] -> IO ()
laçoSubMenu1 l =  do
                  opçao <- subMenu ["1 - Adicionar Livro",
                                    "2 - Remover Livro",
                                    "3 - Lista Livros",
                                    "0 - Voltar"] 0 3
                  case opçao of
                   1 -> do putStrLn ""
                   2 -> do putStrLn ""
                   3 -> do putStrLn ""

laçoSubMenu2 :: [Usuario] -> IO () 
laçoSubMenu2 u = do
                 opçao <- subMenu ["1 - Adicionar Usuário",
                                   "2 - Remover Usuário",
                                   "3 - Listar Usuários",
                                   "0 - Voltar"] 0 3
                 case opçao of
                   1 -> do putStrLn ""
                   2 -> do putStrLn ""
                   3 -> do putStrLn ""

laçoSubMenu3 :: [Emprestimo] -> [Devolucoes] -> IO ()
laçoSubMenu3 e d = do
                   opçao <- subMenu ["1 - Registrar empréstimos",
                                   "2 - Registrar devoluções",
                                   "3 - Listar livros emprestados e disponíveis",
                                   "4 - Lista de Espera"
                                   "0 - Voltar"] 0 3
                   case opçao of
                     1 -> do putStrLn ""
                     2 -> do putStrLn ""
                     3 -> do putStrLn ""


laçoMenu :: [Usuario] -> [Livro] -> [Emprestimo] -> [Devolucoes] -> IO ()
laçoMenu u l e d = do
                     opçao <- menu
                     case opçao of
                       1 -> do laçoSubMenu
                       2 -> do laçoSubMenu
                       
