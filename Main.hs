module Main where

import Livros
import Tipos
import Usuarios
import Relatorios
import Emprestimos
import System.IO


main :: IO ()
main = laçoMenu [] [] [] [] []

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
              "5 - Salvar e Sair"
            ]
    opMin = 1
    opMax = 5

subMenu :: [String] -> Int -> Int -> IO Int
subMenu str min max = do
                         exibirOpçoes str
                         lerOpçao min max


laçoSubMenu1 :: [Usuario] -> [Livro] -> [Emprestimo] -> [Devolucao] -> [Espera] -> IO ()
laçoSubMenu1 u l e d ep  = do
                             opçao <- subMenu ["1 - Adicionar Livro", "2 - Remover Livro", "3 - Listar Livros", "0 - Voltar"] 0 3
                             case opçao of
                              1 -> do putStrLn "Digite o titulo do livro: "
                                      titulo <- getLine
                                      putStrLn "Digite o autor do livro: "
                                      autor <- getLine
                                      ano <- prompt "Digite o ano do livro: "
                                      idLivro <- prompt "Digite o id do livro: "
                                      let novolivro = Livro titulo autor ano idLivro
                                      validarAdicao novolivro l
                                      let novaLista = adicionarLivro novolivro l
                                      laçoSubMenu1 u novaLista e d ep
                              2 -> do idLivro <- prompt "Digite o id do livro: "
                                      validarRemocao idLivro l
                                      let novaLista = removerLivro idLivro l
                                      laçoSubMenu1 u novaLista e d ep
                              3 -> do print l
                                      laçoSubMenu1 u l e d ep
                              _ -> laçoMenu u l e d ep

laçoSubMenu2 :: [Usuario] -> [Livro] -> [Emprestimo] -> [Devolucao] -> [Espera] -> IO () 
laçoSubMenu2  u l e d ep = do
                             opçao <- subMenu ["1 - Adicionar Usuário", "2 - Remover Usuário", "3 - Listar Usuários", "0 - Voltar"] 0 3
                             case opçao of
                               1 -> do putStrLn "Digite o nome do usuário: "
                                       nome <- getLine
                                       putStrLn "Digite a matricula do usuário: "
                                       matricula <- getLine
                                       putStrLn "Digite o email do usuário: "
                                       email <- getLine
                                       let novoUsuario = Usuario nome matricula email
                                       validarAdicaoUsuario novoUsuario u
                                       let novaLista = adicionarUsuario novoUsuario u
                                       laçoSubMenu2 novaLista l e d ep
                               2 -> do putStrLn "Digite a matricula do Usuario: " 
                                       matricula <- getLine
                                       validarRemocaoUsuario matricula u
                                       let novaLista = removerUsuario matricula u
                                       laçoSubMenu2 novaLista l e d ep
                               3 -> do print u
                                       laçoSubMenu2 u l e d ep
                               _ -> laçoMenu u l e d ep

laçoSubMenu3 :: [Usuario] -> [Livro] -> [Emprestimo] -> [Devolucao] -> [Espera] -> IO ()
laçoSubMenu3 u l e d ep = do
                            opçao <- subMenu ["1 - Registrar empréstimos", "2 - Registrar devoluções", "3 - Listar livros emprestados e disponíveis", "4 - Lista de Espera", "0 - Voltar"] 0 3
                            case opçao of
                              1 -> do dia <- prompt "Digite o dia: "
                                      mes <- prompt "Digite o mes: "
                                      putStrLn "Digite o ano: "
                                      anoStr <- getLine
                                      let ano = read anoStr :: Integer
                                      putStrLn "Digite a matricula do usuário: "
                                      matricula <- getLine
                                      idLivro <- prompt "Digite o id do livro: "
                                      let usuario = head (filtraPorMatricula matricula u)
                                      let livro = head (filtrarPorId idLivro l)
                                      let emprestimo = criarEmprestimo (dia,mes,ano) livro usuario
                                      c <- validaEmprestimoIO emprestimo e
                                      case c of
                                        's' -> do
                                            cadastrarEsperaIO usuario livro ep
                                            let listaEspera = cadastrarEspera usuario livro ep
                                            laçoSubMenu3 u l e d listaEspera
                                        'n' -> laçoSubMenu3 u l e d ep
                                        ' ' -> do
                                            let novaListaE = registrarEmprestimo emprestimo e
                                            laçoSubMenu3 u l novaListaE d ep
                              2 -> do
                                   dia <- prompt "Digite o dia: "
                                   mes <- prompt "Digite o mes: "
                                   putStrLn "Digite o ano: "
                                   anoStr <- getLine
                                   let ano = read anoStr :: Integer
                                   putStrLn "Digite a matricula do usuário: "
                                   matricula <- getLine
                                   idLivro <- prompt "Digite o id do livro: "
                                   let usuario = head (filtraPorMatricula matricula u)
                                   let livro = head (filtrarPorId idLivro l)
                                   let novo_d = registrarDevolucao livro usuario (dia,mes,ano) d
                                   putStrLn "Lista de devolucoes atualizada! Atualize o emprestimo agora"
                                   dia <- prompt "Digite o dia do emprestimo: "
                                   mes <- prompt "Digite o mes do emprestimo: "
                                   putStrLn "Digite o ano do emprestimo: "
                                   anoStr <- getLine
                                   let emprestimo = criarEmprestimo (dia,mes,ano) livro usuario
                                   let novo_e = marcarDevolvido emprestimo e
                                   putStrLn "Pronto!"
                                   laçoSubMenu3 u l novo_e novo_d ep
                              3 -> do 
                                   putStrLn "Livros Emprestados:"
                                   let livros_emprestados = filtraLivrosEmprestados e
                                   putStrLn (unlines (map show livros_emprestados))
                                   putStrLn ""
                                   putStrLn "Livros Disponiveis:"
                                   let livros_disponiveis = filtraLivrosDisponiveis l e
                                   putStrLn (unlines (map show livros_disponiveis))
                                   laçoSubMenu3 u l e d ep
                              4 -> do 
                                   id_livro <- prompt "Digite o id do livro"
                                   let livro = head (filtrarPorId id_livro l)
                                   let espera_livro = filtraEsperaPorLivro livro ep
                                   putStrLn (unlines (map show espera_livro))
                                   laçoSubMenu3 u l e d ep
                              _ -> laçoMenu u l e d ep
-- Relatórios submenu
laçoSubMenu4 :: [Usuario] -> [Livro] -> [Emprestimo] -> [Devolucao] -> [Espera] -> IO ()
laçoSubMenu4 u l e d ep = do
    opçao <- subMenu "Menu Relatórios" 
        ["1 - Empréstimos ativos",
         "2 - Histórico de Empréstimos por Usuário",
         "3 - Lista de Espera Completa",
         "4 - Livros Disponíveis",
         "5 - Livros Emprestados",
         "0 - Voltar"] 0 5
    case opçao of
        1 -> do 
            putStrLn "Empréstimos ativos:"
            print $ emprestimosAtivos e
            laçoSubMenu4 u l e d ep
        2 -> do 
            matricula <- prompt "Digite a matrícula do usuário: "
            case head (filtraPorMatricula matricula u) of
                Just usuario -> do
                    putStrLn $ "Histórico de empréstimos para " ++ nome usuario ++ ":"
                    print $ historicoUsuario usuario e
                Nothing -> putStrLn "Usuário não encontrado!"
            laçoSubMenu4 u l e d ep
        3 -> do 
            putStrLn "Lista de espera completa:"
            print $ livrosComEspera (map (\esp -> (idLivro $ livroEsp esp, [usuarioEsp esp])) ep) l
            laçoSubMenu4 u l e d ep
        4 -> do 
            putStrLn "Livros disponíveis:"
            print $ livrosDisponiveis l e
            laçoSubMenu4 u l e d ep
        5 -> do 
            putStrLn "Livros emprestados:"
            print $ livrosEmprestados l e
            laçoSubMenu4 u l e d ep
        0 -> laçoMenu u l e d ep
        _ -> laçoSubMenu4 u l e d ep


laçoMenu :: [Usuario] -> [Livro] -> [Emprestimo] -> [Devolucao] -> [Espera] -> IO ()
laçoMenu  u l e d ep = do
                         opçao <- menu
                         case opçao of
                           1 -> laçoSubMenu1 u l e d ep
                           2 -> laçoSubMenu2 u l e d ep
                           3 -> laçoSubMenu3 u l e d ep
                           4 -> laçoSubMenu4 u l e d ep
                           5 -> laçoSubMenu1 u l e d ep
                           6 -> laçoSubMenu1 u l e d ep
                           7 -> laçoSubMenu1 u l e d ep
                       
