module Emprestimos where


import Tipos

-- filtra os livros que estao emprestados no momento 
filtraLivrosEmprestados :: [Emprestimo] -> [Livro]
filtraLivrosEmprestados lista_emprestimos = foldl (\ i emp -> if (devolvido emp) == False then i ++ [livroE emp] else i) [] lista_emprestimos -- se nao tiver devolvido entra na lista


-- filtra os livros disponiveis no momento. Le a lista de livros e os procura na lista de emprestimos e checa se estao emprestados(devolvido == False)
filtraLivrosDisponiveis :: [Livro] -> [Emprestimo] -> [Livro]
filtraLivrosDisponiveis lista_livros lista_emprestimos = foldl (\ i livro -> if checaLivro livro lista_emprestimos then i ++ [livro] else i) [] lista_livros
 where
  checaLivro livro lista_emprestimos = foldl (\ i emp -> if livro == (livroE emp) then (devolvido emp) else i) True lista_emprestimos


-- filtra a lista de espera para um determinado livro
filtraEsperaPorLivro :: Livro -> [Espera] -> [Espera]
filtraEsperaPorLivro alvo lista_espera = filter (\ esp -> (livroEsp esp) == alvo) lista_espera


-- cadastra um usuario na lista de espera caso ainda nao esteja cadastrado
cadastrarEspera :: Usuario -> Livro -> [Espera] -> [Espera]
cadastrarEspera usuario livro lista_espera = if usuario_valido then lista_espera ++ [(Espera {usuarioEsp = usuario, livroEsp = livro})] else lista_espera
  where
   usuario_valido = foldl (\ b esp -> if esp == (Espera {usuarioEsp = usuario, livroEsp = livro}) then False && b else True && b) True lista_espera


-- printa se o usuario foi cadastrado na lista de espera ou nao
cadastrarEsperaIO :: Usuario -> Livro -> [Espera] -> IO ()
cadastrarEsperaIO usuario livro lista_espera = do
                                               if (foldl (\ b esp -> if esp == (Espera {usuarioEsp = usuario, livroEsp = livro}) then False && b else True && b) True lista_espera)
                                                  then putStrLn ("Pronto! Voce esta cadastrado na lista de espera do livro" ++ show (titulo livro))
                                                  else putStrLn "Parece que voce ja esta cadastrado na lista de espera deste livro! Agora e so esperar :)"



-- checa se o emprestimo pode ser feito(se o livro esta disponivel)
validaEmprestimo :: Emprestimo -> [Emprestimo] -> Bool
validaEmprestimo novo_emprestimo lista_emprestimo = foldl (\ b emp -> (devolvido emp)) True lista_filtrada
  where
   lista_filtrada = filter (\ emp -> (livroE emp) == (livroE novo_emprestimo)) lista_emprestimo



-- funcao IO para dizer se o emprestimo foi bem sucedido ou nao. caso nao, pergunta se quer entrar na lista de espera e retorna sua resposta
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



-- cria uma variavel emprestimo a partir dos campos de dados necessarios
criarEmprestimo :: DiaMesAno -> Livro -> Usuario -> Emprestimo
criarEmprestimo data_emprestimo livro_emprestimo usuario = Emprestimo { dataEmprestimo = data_emprestimo, livroE = livro_emprestimo, usuarioE = usuario, devolvido = False }


-- adiciona um novo emprestimo na lista de emprestimos
registrarEmprestimo :: Emprestimo -> [Emprestimo] -> [Emprestimo]
registrarEmprestimo novo_emprestimo lista_emprestimos = if (validaEmprestimo novo_emprestimo lista_emprestimos) then lista_emprestimos ++ [novo_emprestimo] else lista_emprestimos


-- adiciona uma devolucao na lista de devolucoes
registrarDevolucao :: livro -> Usuario -> DiaMesAno -> [Devolucao] -> [Devolucao]
registrarDevolucao livro usuario data_devolucao lista_devolucoes = lista_devolucoes ++ [Devolucao { dataDevolucao = data_devolucao, livroD = livro, usuarioD = usuario} ]


-- altera o estado do emprestimo para devolvido
marcarDevolvido :: Emprestimo -> [Emprestimo] -> [Emprestimo]
marcarDevolvido _ [] = []
marcarDevolvido alvo lista_emprestimos = map (\e -> if e == alvo then (head empDevolvido) else e) lista_emprestimos
    where
     empDevolvido = filter (\emp -> alvo == emp) lista_emprestimos
