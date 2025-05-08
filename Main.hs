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


laçoSubMenu1 :: [Usuario] -> [Livro] -> [Emprestimo] -> [Devolucoes] -> [Espera] -> IO ()
laçoSubMenu1 u l e d ep  =  do
                              opçao <- subMenu ["1 - Adicionar Livro",
                                                "2 - Remover Livro",
                                                "3 - Listar Livros",
                                                "0 - Voltar"] 0 3
                            case opçao of
                              1 -> do titulo <- prompt "Digite o titulo do livro: "
                                      autor <- prompt "Digite o autor do livro: "
                                      ano <- prompt "Digite o ano do livro: "
                                      idLivro <- prompt "Digite o id do livro: "
                                      let livro = Livro titulo autor ano idLivro
                                      let novaLista = adicionarLivro livro l
                                      validarAdicao livro novaLista
                                      laçoSubMenu1 u novaLista e d ep
                              2 -> do idLivro <- prompt "Digite o id do livro: "
                                      removerLivro idLivro l
                                      validarRemocao idLivro l
                                      laçoSubMenu1 u l e d ep
                              3 -> print l
                              _ -> do laçoMenu l e d u ep

laçoSubMenu2 :: [Usuario] -> [Livro] -> [Emprestimo] -> [Devolucoes] -> [Espera] -> IO () 
laçoSubMenu2  u l e d ep = do
                             opçao <- subMenu ["1 - Adicionar Usuário",
                                               "2 - Remover Usuário",
                                               "3 - Listar Usuários",
                                               "0 - Voltar"] 0 3
                             case opçao of
                               1 -> do nome <- prompt "Digite o nome do usuário: "
                                       matricula <- prompt "Digite a matricula do usuário: "
                                       email <- prompt "Digite o email do usuário: "
                                       let usuario = Usuario nome matricula email
                                       let novaLista = adicionarLivro livro l
                                       validarAdicao livro novaLista
                                       laçoSubMenu1 u novaLista e d ep
                               2 -> do putStrLn ""
                               3 -> do putStrLn ""
                               _ -> laçoMenu u l e d ep

laçoSubMenu3 :: [Usuario] -> [Livro] -> [Emprestimo] -> [Devolucoes] -> [Espera] -> IO ()
laçoSubMenu3 u l e d ep = do
                            opçao <- subMenu ["1 - Registrar empréstimos",
                                              "2 - Registrar devoluções",
                                              "3 - Listar livros emprestados e disponíveis",
                                              "4 - Lista de Espera"
                                              "0 - Voltar"] 0 3
                            case opçao of
                              1 -> do putStrLn ""
                              2 -> do putStrLn ""
                              3 -> do putStrLn ""
                              _ -> laçoMenu u l e d ep

laçoSubMenu4 :: [Usuario] -> [Livro] -> [Emprestimo] -> [Devolucoes] -> [Espera] -> IO ()
laçoSubMenu4  u l e d ep = do
                             opçao <- subMenu ["1 - Empréstimos ativos",
                                           "2 - Historico de Empréstimos",
                                           "3 - Lista de Espera",
                                           "4 - Livros Disponiveis",
                                           "5 - Livros Emprestados",
                                           "0 - Voltar"] 0 3
                             case opçao of
                               1 -> do let listaEmprestimos = emprestimosAtivos e
                                       if e == [] then putStrLn "Sem empréstimos cadastrados!"
                                       else putStrLn ("Empréstimos ativos: " ++ show listaEmprestimos ++ "/n")
                                       laçoSubMenu4 u l e d ep
                               2 -> do putStrLn ""
                               3 -> do putStrLn ""
                               _ -> laçoMenu u l e d ep


laçoMenu :: [Usuario] -> [Livro] -> [Emprestimo] -> [Devolucoes] -> [Espera] -> IO ()
laçoMenu  u l e d ep = do
                         opçao <- menu
                         case opçao of
                           1 -> laçoSubMenu1
                           2 -> laçoSubMenu2
                           3 -> laçoSubMenu3
                           4 -> laçoSubMenu4
                           5 -> do laçoSubMenu
                           6 -> do laçoSubMenu
                           7 -> do laçoSubMenu
                       
