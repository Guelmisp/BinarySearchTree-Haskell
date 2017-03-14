-- Parte 1 **
-- estrutura 
data ArvoreBinaria a = Vazio | No a (ArvoreBinaria a) (ArvoreBinaria a) deriving Show

-- Arvores para teste
av01 :: ArvoreBinaria Int
av01 = Vazio

av02 :: ArvoreBinaria Int
av02 = No 10 Vazio Vazio

av03 :: ArvoreBinaria Int
av03 = No 20 Vazio Vazio

av04 :: ArvoreBinaria Int
av04 = No 15 av02 av03

av05 :: ArvoreBinaria Int
av05 = No 15
       (No 5 (No 4 Vazio Vazio)
             (No 10 (No 8 (No 6 Vazio Vazio)
             	           Vazio)
                     Vazio)
        )
       (No 16 Vazio (No 19
       	                (No 17 Vazio (No 18 Vazio Vazio))
                        (No 21 Vazio Vazio))
       )

-- Parte 2 ** 
--a) Funcao que verifica se uma arvore é uma estrutura vazia
verificaVazio :: ArvoreBinaria a -> String
verificaVazio Vazio = "A arvore esta Vazia"
verificaVazio (No a esquerda direita) = "A arvore contem elementos"

--b) Funcao que retorna o nodo da arvore
retornaNo :: ArvoreBinaria a -> a
retornaNo (No a esquerda direita) = a  

--c) Funcao que retorna o lado esquerdo
retornaEsquerdo :: ArvoreBinaria a -> ArvoreBinaria a
retornaEsquerdo (No a esquerda direita) = esquerda

--d) Funcao que retorna o lado direiro
retornaDireito :: ArvoreBinaria a -> ArvoreBinaria a
retornaDireito (No a esquerda direita) = direita

-- Parte 3 **
-- Retorna o peso de uma arvore binaria
verificaPeso :: ArvoreBinaria a -> Int
verificaPeso Vazio = 0
verificaPeso (No a Vazio Vazio) = 1
verificaPeso (No a esquerda direita) = 1 + max (verificaPeso esquerda) (verificaPeso direita)

-- Parte 4 **
--instance Show a => Show (ArvoreBinaria a) where
--	showsPrec _ Vazio = showString "_"
--	showsPrec _ (No x e d) = showString "{" . (shows x) .
--	          showString ":" . (shows e) .
--	          showString "|" . (shows d) .
--	          showString "}"

-- Parte 5 **
-- a) Imprimir a arvore em ordem (Menor para Maior)
emOrdem :: (Ord a) => ArvoreBinaria a -> [a]
emOrdem Vazio = []
emOrdem (No a esquerda direita) = emOrdem esquerda ++ [a] ++ emOrdem direita

-- b) Imprimir a arvore em pre ordem (No, esquerda, direira)
preOrdem :: (Ord a) => ArvoreBinaria a -> [a]
preOrdem Vazio = []
preOrdem (No a esquerda direita) = [a] ++ (preOrdem esquerda) ++ (preOrdem direita)

-- c) Imprimir a arvore em pos ordem (Esquerda, direita, No)
posOrdem :: (Ord a) => ArvoreBinaria a -> [a]
posOrdem Vazio = []
posOrdem (No a esquerda direita) = (posOrdem esquerda) ++ (posOrdem direita) ++ [a]

-- Parte 6 ** 
-- Tracos de uma arvore binaria (todos caminhos)
tracos :: ArvoreBinaria a -> [[a]]
tracos Vazio = [[]]
tracos (No a Vazio Vazio) = [[a]]
tracos (No a esquerda direita) = do 
	t <- [esquerda, direita]
	tracar <- tracos t
	return (a:tracar)

