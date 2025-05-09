Sistema de Gerenciamento de Biblioteca em Haskell

📌 Descrição do Projeto
Sistema de gerenciamento de bibliotecas desenvolvido em Haskell como trabalho acadêmico para a disciplina de Programação Funcional. Permite criar, modificar, organizar e consultar livros, usuários, empréstimos e devoluções.

👥 Participantes
- Gabriel Valentim
- Pablo Rodrigues
- Pedro De Colla
- Ruan Pablo

✨ Funcionalidades Principais:

📚 Gerenciamento Básico
- ✅ Adicionar, remover e modificar livros e usuários
- ✅ Validação de IDs únicos para livros e matrículas únicas para usuários
- ✅ Verificação de e-mails válidos e únicos

🔄 Empréstimos e Devoluções
- 📖 Registrar empréstimos de livros
- 🔙 Registrar devoluções
- ⏳ Sistema de lista de espera para livros indisponíveis

🔍 Filtros e Buscas
- 🔎 Buscar livros por título, autor ou ano
- 👤 Buscar usuários por nome, matrícula ou e-mail
- 📊 Listar livros disponíveis e emprestados

📊 Relatórios
- 📋 Listar todos os empréstimos ativos
- 🕵️ Histórico de empréstimos por usuário
- 📝 Livros com lista de espera

⚙️ Pré-requisitos
- [GHC](https://www.haskell.org/ghc/) (versão 8.10 ou superior)
- [Cabal](https://www.haskell.org/cabal/) (ou Stack)
- Biblioteca `directory` para manipulação de arquivos

🚀 Como Executar

1. Clone o repositório
git clone https://github.com/Pablorde9/Trabalho2-Haskell-.git
cd Trabalho2-Haskell-

2. Compile o projeto
ghc --make Main.hs -o biblioteca

3. Execute o programa
./biblioteca

📂 Estrutura de Arquivos
- `Main.hs` - Módulo principal com a interface do sistema
- `Tipos.hs` - Definições dos tipos de dados
- `Livros.hs` - Operações relacionadas a livros
- `Usuarios.hs` - Operações relacionadas a usuários
- `Emprestimos.hs` - Lógica de empréstimos e devoluções
- `Relatorios.hs` - Geração de relatórios
- `Persistencia.hs` - Funções para salvar/carregar dados

💾 Persistência de Dados
O sistema automaticamente cria/usa o arquivo para armazenar todos os dados entre execuções.

🧪 Testes
O sistema inclui validações automáticas para:
- IDs de livros únicos
- Matrículas de usuários únicas
- E-mails válidos e únicos
- Disponibilidade de livros para empréstimo
