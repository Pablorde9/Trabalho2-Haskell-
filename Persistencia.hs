livroParaString :: Livro -> String
livroParaString livro = unwords [show (idLivro livro) 
                           , "/ " ++ titulo livro
                           , "/ " ++ autor livro
                           , "/ " ++ show (ano livro)
                           , "/ " ++ show (idLivro livro)
                           ]

usuarioParaString :: Usuario -> String
usuarioParaString usuario = unwords [ nome usuario
                              , "/ " ++ matricula usuario
                              , "/ " ++ email usuario
                              ]

emprestimoParaString :: Emprestimo -> String
emprestimoParaString emp = unwords [ showDia dataEmp
                                 , "/ " ++ show (idLivro $ livroE emp)
                                 , "/ " ++ usuarioE emp
                                 , "/ " ++ show devolv
                                 ]
  where
    dataEmp = dataEmprestimo emp
    devolv   = devolvido emp
    showDia (d,m,y) = show y ++ "-" ++ show m ++ "-" ++ show d

salvarEmArquivo :: FilePath -> [a] -> (a -> String) -> IO ()
salvarEmArquivo arquivo lista conversor = writeFile arquivo (unlines $ map conversor lista)

carregarDeArquivo :: FilePath -> (String -> a) -> IO [a]
carregarDeArquivo arquivo conversor = do
  conteudo <- readFile arquivo
  return $ map conversor (lines conteudo)
