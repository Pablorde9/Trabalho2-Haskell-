module Persistencia where

import Data.Char (toLower)
import Data.List (intercalate)
import Usuarios
import Livros
import Tipos


-- Funções para String (Serializacao)


livroParaString :: Livro -> String
livroParaString livro = intercalate "/" 
    [ show (idLivro livro)
    , titulo livro
    , autor livro
    , show (ano livro)
    ]

usuarioParaString :: Usuario -> String
usuarioParaString usuario = intercalate "/"
    [ matricula usuario
    , nome usuario
    , email usuario
    ]

emprestimoParaString :: Emprestimo -> String
emprestimoParaString emp = intercalate "/"
    [ arrumaData (dataEmprestimo emp)
    , show (idLivro (livroE emp))
    , matricula (usuarioE emp)
    , map toLower (show (devolvido emp))
    ]
  where
    arrumaData (d, m, a) = intercalate "-" [show a, show m, show d]

devolucaoParaString :: Devolucao -> String
devolucaoParaString devol = intercalate "/"
    [ arrumaData (dataDevolucao devol)
    , show (idLivro (livroD devol))
    , matricula (usuarioD devol)
    ]
  where
    arrumaData (d, m, a) = intercalate "-" [show a, show m, show d]

esperaParaString :: Espera -> String
esperaParaString esp = intercalate "/"
    [ show (idLivro (livroEsp esp))
    , matricula (usuarioEsp esp)
    ]


-- Funções de String (Desserializacao)


stringParaLivro :: String -> Maybe Livro
stringParaLivro string = case split '/' string of
    [idStr, tit, aut, anoStr] ->
        case (readValido idStr, readValido anoStr) of
            (Just id', Just ano') -> Just Livro
                { idLivro = id'
                , titulo = tit
                , autor = aut
                , ano = ano'
                }
            _ -> Nothing
    _ -> Nothing

stringParaUsuario :: String -> Maybe Usuario
stringParaUsuario string = case split '/' string of
    [mat, nom, ema] -> Just Usuario
        { matricula = mat
        , nome = nom
        , email = ema
        }
    _ -> Nothing

stringParaEmprestimo :: [Livro] -> [Usuario] -> String -> Maybe Emprestimo
stringParaEmprestimo livros usuarios string = case split '/' string of
    [dateStr, idLivStr, matriculaStr, devStr] ->
        case (dataValido dateStr, readValido idLivStr, parseBool devStr) of
            (Just (d, m, a), Just idLiv, Just dev) ->
                case (encontraLivro idLiv livros, encontraUsuario matriculaStr usuarios) of
                    (Just livro, Just usuario) -> Just Emprestimo
                        { dataEmprestimo = (d, m, a)
                        , livroE = livro
                        , usuarioE = usuario
                        , devolvido = dev
                        }
                    _ -> Nothing
            _ -> Nothing
    _ -> Nothing

stringParaDevolucao :: [Livro] -> [Usuario] -> String -> Maybe Devolucao
stringParaDevolucao livros usuarios string = case split '/' string of
    [dateStr, idLivStr, matriculaStr] ->
        case (dataValido dateStr, readValido idLivStr) of
            (Just (d, m, a), Just idLiv) ->
                case (encontraLivro idLiv livros, encontraUsuario matriculaStr usuarios) of
                    (Just livro, Just usuario) -> Just Devolucao
                        { dataDevolucao = (d, m, a)
                        , livroD = livro
                        , usuarioD = usuario
                        }
                    _ -> Nothing
            _ -> Nothing
    _ -> Nothing

stringParaEspera :: [Livro] -> [Usuario] -> String -> Maybe Espera
stringParaEspera livros usuarios string = case split '/' string of
    [idLivStr, matriculaStr] ->
        case readValido idLivStr of
            Just idLiv ->
                case (encontraLivro idLiv livros, encontraUsuario matriculaStr usuarios) of
                    (Just livro, Just usuario) -> Just Espera
                        { livroEsp = livro
                        , usuarioEsp = usuario
                        }
                    _ -> Nothing
            _ -> Nothing
    _ -> Nothing


-- Funções aux


split :: Char -> String -> [String]
split sep = filter (not . null) . foldr (\c (x:xs) -> 
    if c == sep 
    then []:x:xs 
    else (c:x):xs) [""]

readValido :: Read a => String -> Maybe a
readValido string = case reads string of
    [(x, "")] -> Just x
    _ -> Nothing

dataValido :: String -> Maybe (Int, Int, Integer)
dataValido string = case split '-' string of
    [aStr, mStr, dStr] ->
        case (readValido aStr, readValido mStr, readValido dStr) of
            (Just a, Just m, Just d) -> Just (d, m, a)
            _ -> Nothing
    _ -> Nothing

encontraLivro :: Int -> [Livro] -> Maybe Livro
encontraLivro idLiv livros = case filtrarPorId idLiv livros of
    [livro] -> Just livro  
    _       -> Nothing     


encontraUsuario :: String -> [Usuario] -> Maybe Usuario
encontraUsuario mat usuarios = case filtraPorMatricula mat usuarios of
    [usuario] -> Just usuario
    _         -> Nothing     

mapValido :: (a -> Maybe b) -> [a] -> [b]
mapValido _ []     = []
mapValido f (x:xs) = case f x of
    Just y  -> y : mapValido f xs
    Nothing -> mapValido f xs

parseBool :: String -> Maybe Bool
parseBool s = case map toLower s of
    "true"  -> Just True
    "false" -> Just False
    "t"     -> Just True
    "f"     -> Just False
    "1"     -> Just True
    "0"     -> Just False
    _       -> Nothing


-- Funções de arquivo

salvarDados :: FilePath -> [Livro] -> [Usuario] -> [Emprestimo] -> [Devolucao] -> [Espera] -> IO ()
salvarDados arquivo livros usuarios emprestimos devolucoes esperas = do
    let conteudo = unlines $
            ["-- LIVROS --"] ++
            map livroParaString livros ++
            ["-- USUARIOS --"] ++
            map usuarioParaString usuarios ++
            ["-- EMPRESTIMOS --"] ++
            map emprestimoParaString emprestimos ++
            ["-- DEVOLUCOES --"] ++
            map devolucaoParaString devolucoes ++
            ["-- ESPERAS --"] ++
            map esperaParaString esperas
    writeFile arquivo conteudo

carregarDados :: FilePath -> IO (Either String ([Livro], [Usuario], [Emprestimo], [Devolucao], [Espera]))
carregarDados arquivo = do
    conteudo <- readFile arquivo
    let partes = dividePartes (lines conteudo)
    
    case partes of
        (livrosStr, usuariosStr, emprestimosStr, devolucoesStr, esperasStr) -> do
            let livros' = mapValido stringParaLivro livrosStr
                usuarios' = mapValido stringParaUsuario usuariosStr
                emprestimos' = mapValido (stringParaEmprestimo livros' usuarios') emprestimosStr
                devolucoes' = mapValido (stringParaDevolucao livros' usuarios') devolucoesStr
                esperas' = mapValido (stringParaEspera livros' usuarios') esperasStr
                
            if any (\xs -> length xs /= length (filter (/= "") xs)) 
                   [livrosStr, usuariosStr, emprestimosStr, devolucoesStr, esperasStr]
            then return $ Left "Erro na conversao de algum registro"
            else return $ Right (livros', usuarios', emprestimos', devolucoes', esperas')
  where
    dividePartes :: [String] -> ([String], [String], [String], [String], [String])
    dividePartes lines' =
        let (livros, rest1) = break (== "-- USUARIOS --") lines'
            (usuarios, rest2) = break (== "-- EMPRESTIMOS --") (drop 1 rest1)
            (emprestimos, rest3) = break (== "-- DEVOLUCOES --") (drop 1 rest2)
            (devolucoes, rest4) = break (== "-- ESPERAS --") (drop 1 rest3)
            esperas = drop 1 rest4
        in
            ( filter (/= "-- LIVROS --") livros
            , filter (/= "-- USUARIOS --") usuarios
            , filter (/= "-- EMPRESTIMOS --") emprestimos
            , filter (/= "-- DEVOLUCOES --") devolucoes
            , filter (/= "-- ESPERAS --") esperas
            )
