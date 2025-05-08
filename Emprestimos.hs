module Emprestimos where


import Tipos

filtraEsperaPorLivro :: Livro -> [Espera] -> [Espera]
filtraEsperaPorLivro alvo lista_espera = filter (\ esp -> (livroEsp esp) == alvo) lista_espera


cadastrarEspera :: Usuario -> Livro -> [Espera] -> [Espera]
cadastrarEspera usuario livro lista_espera = if usuario_valido then lista_espera ++ [(Espera {usuarioEsp = usuario, livroEsp = livro})] else lista_espera
  where
   usuario_valido = foldl (\ b esp -> if esp == (Espera {usuarioEsp = usuario, livroEsp = livro}) then False && b else True && b) True lista_espera



validaEmprestimo :: Emprestimo -> [Emprestimo] -> Bool
validaEmprestimo novo_emprestimo lista_emprestimo = foldl (\ b emp -> (devolvido emp)) True lista_filtrada
  where
   lista_filtrada = filter (\ emp -> (livroE emp) == (livroE novo_emprestimo)) lista_emprestimo

validaEmprestimoIO :: Emprestimo -> [Emprestimo] -> IO Char
validaEmprestimoIO novo_emprestimo lista_emprestimo         = do
                                                              if (validaEmprestimo novo_emprestimo lista_emprestimo)
                                                                 then do
                                                                      putStrLn "Emprestimo feito com sucesso"
                                                                      return ' '
                                                                 else do 
                                                                      putStrLn "Erro! livro nao esta disponivel no momento"
                                                                      print "Deseja entrar na fila de espera[s|n]: "
                                                                      escolha <- getChar
                                                                      return escolha



criarEmprestimo :: DiaMesAno -> Livro -> Usuario -> Emprestimo
criarEmprestimo data_emprestimo livro_emprestimo usuario = Emprestimo { dataEmprestimo = data_emprestimo, livroE = livro_emprestimo, usuarioE = usuario, devolvido = False }

registrarEmprestimo :: Emprestimo -> [Emprestimo] -> [Emprestimo]
registrarEmprestimo novo_emprestimo lista_emprestimos = if (validaEmprestimo novo_emprestimo lista_emprestimos) then lista_emprestimos ++ [novo_emprestimo] else lista_emprestimos


registrarDevolucao :: Emprestimo -> DiaMesAno -> [Devolucao] -> [Devolucao]
registrarDevolucao emprestimo data_devolucao lista_devolucoes = lista_devolucoes ++ [Devolucao { dataDevolucao = data_devolucao, livroD = (livroE emprestimo), usuarioD = (usuarioE emprestimo)} ]

marcarDevolvido :: Emprestimo -> [Emprestimo] -> [Emprestimo]
marcarDevolvido _ [] = []
marcarDevolvido alvo lista_emprestimos = map (\e -> if e == alvo then (head empDevolvido) else e) lista_emprestimos
    where
     empDevolvido = filter (\emp -> alvo == emp) lista_emprestimos
