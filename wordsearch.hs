module Main where -- cabal tava enchendo o saco, coloquei isso e funfou
import System.Random
--import System.IO
import System.Console.ANSI



type Board = [[Char]]

board :: Board  --10x10
board = [['-','-','-','-','-','-','-','-','-','-'],
         ['-','-','-','-','-','-','-','-','-','-'],
         ['-','-','-','-','-','-','-','-','-','-'],
         ['-','-','-','-','-','-','-','-','-','-'],
         ['-','-','-','-','-','-','-','-','-','-'],
         ['-','-','-','-','-','-','-','-','-','-'],
         ['-','-','-','-','-','-','-','-','-','-'],
         ['-','-','-','-','-','-','-','-','-','-'],
         ['-','-','-','-','-','-','-','-','-','-'],
         ['-','-','-','-','-','-','-','-','-','-']]


-- ooops o banco de palavras só tem nomes de bandas
-- palavras com espaço ficariam estranhas. Acento é complicado, então fds
-- eu tenho um excel com nomes de bandas (em ordem alfabética). ai eu meio que to usando eles.
-- eu tentaria leitura de arquivo, se não estivesse fazendo essa parte 2 dias antes da entrega.
wordbank :: [String]
wordbank = ["accept", "aggaloch", "ahab", "alcest", "alestorm", "angra", "annihilator", "anvil", "bathory", "batushka", "behemoth", "beherit",
            "belphegor", "blasphemy", "burzum", "candlemass", "carcass", "catacombs", "clouds", "coroner", "danzig", "darkthrone", "death", "decomposed", 
            "demilich", "destruction", "dethklok", "dio", "disembowelment", "dokken", "draconian", "dragonforce", "evoken", "exodus", "eyehategod",
            "girlschool", "gojira", "gorgoroth", "hammerfall", "havok", "holocausto", "horna", "immortal", "indestroy", "inquisiton", "insomnium", "kataklism",
            "katatonia","kiss", "kreator", "manowar", "mastodon", "mayhem", "megadeth", "melvins", "messhugah", "metallica", "mgla", "mortification",
            "motorhead", "nargaroth", "neurosis", "nitro", "nucleus", "obituary", "omen", "opeth", "ostrogoth", "overkill", "pallbearer", "pantera", "pantheist", "pentagram",
            "powerwolf", "proclamation", "pustulated", "riot", "rippikoulu", "sabaton", "sanctuary", "sarcofago", "saturnus", "satyricon", "savatage", "saxon",
            "scorpions", "sepultura", "skeletonwitch", "skepticism", "slayer", "sleep", "sodom", "terrorizer", "testament", "thergothon", "trouble", "triptykon",
            "tvangeste", "ufo", "urfaust", "venom", "virus", "visigoth", "wallachia", "warbringer", "warning", "yob"] -- lenght 107

-- nao to tentando me mostra como kvlt, só peguei palavras que eu ja tinha amazenado num arquivo (era uma ideia de app q acabou morrendo).
-- o bom dessas palavras é q tem coisa bem curta e bem comprida, ai da pra espalhar bem no tabuleiro.

--Palavra, l e c da posicao inicial e final, método de inserção;
type WordList = [([Char], ((Int, Int),(Int, Int)), Int)]

wordslist :: WordList
wordslist =  []
 
-- EM  MINHA DEFESA, HASKELL É UMA LINGUAGEM DE PROGRAMAÇÃO FUNCIONAL
-- E UM DOS PILARES DE PF É REUTILIZAR CÓDIGO
-- LOGO ISSO É PERFEITAMENTE ACEITAVEL, AINDA MAIS PQ EU Q ESCREVI [a maior parte] DESSES MÉTODOS

----- INICIO DOS MÉTODOS ROUBADOS -----

uArr :: Int -> a -> [a] -> [a]
uArr 0 v (x:xs) = v:xs
uArr p v (x:xs) = x : uArr (p-1) v xs

uPos :: Int -> Int ->  a -> [[a]] -> [[a]]
uPos 0 c v (x:xs) = (uArr c v x):xs
uPos l c v (x:xs) = x : (uPos (l-1) c v xs)

gArr :: Int -> [t] -> t
gArr _ (x:[]) = x
gArr 0 (x:xs) = x
gArr p (x:xs) = gArr (p-1) xs

gPos :: Int -> Int -> [[a]] -> a
gPos l c (x:xs) = gArr c $ gArr l (x:xs) 

----- FIM DOS MÉTODOS ROUBADOS -----

-- No main: compara lista antiga com lista nova; se diferentes, adiciona palavra na lista.

-- Insere a palavra recém sorteada na lista. A posição final é calculada agora.
-- Calcular agora evita um cálculo no 'front' na hora de ver se acertou. 

addOnList :: WordList -> [Char] -> (Int,Int) -> Int -> WordList
addOnList list p (l,c) m 
    | m == 1 = (p, ((l, c), (l, c - 1 +(length p))), m):list
    | m == 2 = (p, ((l, c), (l, c + 1 - (length p))), m):list
    | m == 3 = (p, ((l, c), (l - 1 + (length p), c)), m):list
    | m == 4 = (p, ((l, c), (l + 1 - (length p), c)), m):list

--- O que eu fiz aqui é carregar um backup do board pra lá e pra cá.
--- Isso porque, como o posicionamento é aleatório, eu não sei se vai ter problemas inserindo ou não.
--- Caso tiver um problema, ele detecta 'on the run' e manda o backup. Senão, manda o board normal.

validInsertH :: [Char] -> (Int, Int) -> Board -> Board
validInsertH p (l,c) b 
    | (c + (length p)) >= (length b) = b
    | otherwise                      = aux p (l,c) b b
        where
            aux :: [Char] -> (Int, Int) -> Board -> Board -> Board
            aux [] (l,c) b backup = b
            aux (p:ps) (l,c) b backup
                | (gPos l c b) == '-' = aux ps (l,c+1) (uPos l c p b) backup
                | (gPos l c b) == p   = aux ps (l,c+1) (uPos l c p b) backup
                | (gPos l c b) /= p   = backup


validInsertInvH :: [Char] -> (Int, Int) -> Board -> Board
validInsertInvH p (l,c) b 
    | (c - (length p) +1) < 0 = b
    | otherwise                      = aux p (l,c) b b
        where
            aux :: [Char] -> (Int, Int) -> Board -> Board -> Board
            aux [] (l,c) b backup = b
            aux (p:ps) (l,c) b backup
                | (gPos l c b) == '-' = aux ps (l,c-1) (uPos l c p b) backup
                | (gPos l c b) == p   = aux ps (l,c-1) (uPos l c p b) backup
                | (gPos l c b) /= p   = backup


validInsertV :: [Char] -> (Int, Int) -> Board -> Board
validInsertV p (l,c) b 
    | (l + (length p)) >= (length b) = b
    | otherwise                      = aux p (l,c) b b
        where
            aux :: [Char] -> (Int, Int) -> Board -> Board -> Board
            aux [] (l,c) b backup = b 
            aux (p:ps) (l,c) b backup
                | (gPos l c b) == '-' = aux ps (l+1,c) (uPos l c p b) backup
                | (gPos l c b) == p   = aux ps (l+1,c) (uPos l c p b) backup
                | (gPos l c b) /= p   = backup

validInsertInvV :: [Char] -> (Int, Int) -> Board -> Board
validInsertInvV p (l,c) b 
    | (l - (length p) +1) < 0 = b
    | otherwise                      = aux p (l,c) b b
        where
            aux :: [Char] -> (Int, Int) -> Board -> Board -> Board
            aux [] (l,c) b backup = b 
            aux (p:ps) (l,c) b backup
                | (gPos l c b) == '-' = aux ps (l-1,c) (uPos l c p b) backup
                | (gPos l c b) == p   = aux ps (l-1,c) (uPos l c p b) backup
                | (gPos l c b) /= p   = backup



-- Palavra a inserir, posicao, método, board;
-- nao fiz diagonal pq isso ia ficar uma zona
inserir :: [Char] -> (Int, Int) -> Int -> WordList -> Board -> (Board, WordList)
inserir p (l,c) m wl b
    | m == 1    = aux p (l,c) m b wl $ validInsertH p (l,c) b
    | m == 2    = aux p (l,c) m b wl $ validInsertInvH p (l,c) b
    | m == 3    = aux p (l,c) m b wl $ validInsertV p (l,c) b
    | m == 4    = aux p (l,c) m b wl $ validInsertInvV p (l,c) b
        where 
            aux :: [Char] -> (Int, Int) -> Int -> Board -> WordList -> Board -> (Board, WordList)
            aux p (l,c) m backup wl b
                | backup == b   = (backup, wl)
                | backup /= b   = (b, addOnList wl p (l,c) m)

--1: esquerda pra direita; (horizontal normal)
--2: direita pra esquerda; (horizontal inverso) (inverte a palavra, e insere ela);
--3: cima pra baixo (vertical normal);
--4: baixo pra cima (vertical inverso) (inverte a palvra, e insere ela);


-- position faz referencia a posicao no arrey, adress é oq o user digitou
findWord :: WordList -> ((Int,Int),(Int, Int)) -> (WordList, String)
findWord [] _ = ([], [])
findWord (((w, position, m)):wl) adress
    | position == adress = (wl,w)
    | position /= adress = ((((w, position, m)) : (fst (findWord wl adress))),(snd (findWord wl adress))) -- lisp; humm que código gostosinho

onList :: [Char] -> WordList -> Bool
onList _ [] = False
onList p ((w, _, _):wl) 
    | p == w    = True
    | otherwise = onList p wl

execute :: Board -> (Board,WordList)
execute board = let l = inserir "ahab" (0,6) 3 (snd (inserir "batushka" (1,1) 1 wordslist board)) (fst (inserir "batushka" (1,1) 1 wordslist board))
                in (fst l, snd l)

--- Front End ---
-- só a titúlo de curiosidade, eu nunca mechi com front na hut8
-- estou considerando o IO como front end em alguns casos, também.

----- INICIO DOS MÉTODOS ROUBADOS -----

printBoard :: Board -> WordList -> String
printBoard (x:xs) wl = "    " ++ cabecalho 0 (length(x)-1) ++ "\n   " ++ barra (length(x)-1) ++ "\n" ++ looper 0 (x:xs) ++ displayWords wl

cabecalho :: Int -> Int -> String
cabecalho n t
      | n == t    = show(n)
      | otherwise = show(n) ++ " " ++ cabecalho (n+1) t

barra :: Int -> String
barra 0 = " v"
barra b = " v" ++ barra (b-1)
looper :: Int -> Board -> String
looper n ((x:xs):rs) 
      | n == (length (x:xs)-1)     = show(n) ++ " > " ++ linha 0 ((x:xs):rs)
      | otherwise                   = show(n) ++ " > " ++ linha 0 ((x:xs):rs) ++ looper (n+1) (rs)
linha :: Int -> Board -> String
linha n ((x:xs):rs)
      | n == (length(x:xs)-1) = x:[] ++ "\n"
      | (x:xs) == []          = linha (n+1) rs
      | otherwise             = x:[] ++ " " ++ linha n (xs:rs)

      
displayWords :: WordList -> String
displayWords [] = ""
displayWords ((p, _, _):ls) = p ++ "\n" ++ displayWords ls

----- FIM DOS MÉTODOS ROUBADOS -----
--game :: IO ()

geraNovoTabuleiro :: Int -> Board
geraNovoTabuleiro 0 = []
geraNovoTabuleiro n = geraLista n $ geraLista n '-'

geraLista :: Int -> a -> [a]
geraLista 0 _ = []
geraLista n v = v : geraLista (n-1) v

main :: IO ()
main = do
    clearScreen
    putStrLn ("digite o tamanho do tabuleiro [recomendado: 10]")
    tamanho <- getLine
    let (helper) = geraNovoTabuleiro (read tamanho)
    (b,wl) <- fillWordTable (read tamanho) helper [] wordbank
    gameLoop b wl []


gameLoop :: Board -> WordList -> [String] -> IO ()
gameLoop b wl foundwords = do
    clearScreen
    putStr (printBoard b wl)
    setSGR [SetColor Foreground Vivid Red]
    putStrLn (printWordsFound foundwords)
    setSGR [Reset]  
    order1 <- getLine 
    if (order1 == "start")
        then main
        else do 
            if (order1 == "help")
                then do
                    help
                else do
                    let (l1,c1) = parse order1
                    order2 <- getLine
                    putStrLn (printBoard b wl)
                    if (order2 == "start")
                        then main
                        else do 
                            if (order2 == "help")
                            then do
                                help
                            else do
                                let (l2,c2) = parse order2
                                let (newwordlist, found) = findWord wl ((l1,c1),(l2,c2))
                                if (victory newwordlist)
                                    then do
                                        clearScreen -- por algum motivo isso n funfa direito pra mim, deve ser coisa do windows
                                        setSGR [SetColor Foreground Vivid Blue]
                                        putStrLn ("Voce venceu!")
                                        setSGR [Reset]
                                        putStrLn ("digite start para recomeçar, ou outra coisa para sair") --'outra coisa' bruh
                                        restart <- getLine
                                        if (restart == "start")
                                            then main
                                            else putStrLn ("ok")
                                    else do 
                                        if (newwordlist == wl) 
                                            then gameLoop b newwordlist foundwords --errou
                                            else do
                                                let newfoundwords = found : foundwords
                                                gameLoop b newwordlist newfoundwords --acertou


help :: IO ()
help = do
    putStrLn ("Digite 'start' para gerar um novo caça palavras")
    putStrLn ("durante o jogo, digite a linha e coluna do inicio da palavra [separado por espaço], e aperte enter")  
    putStrLn ("em seguida, digite a linha e a coluna do fim da palavra, também separado por espaço.")
    putStrLn ("exemplo: 1 1 [enter] 1 8")


superFor :: [[Char]] -> IO [[Char]]
superFor [] = return []
superFor b = looper 0 0 (length b) b
    where
        looper :: Int -> Int -> Int -> [[Char]] -> IO [[Char]]
        looper l c size [] = return []
        looper l c size b = do
            r  <- randomRIO ('a','z') :: IO Char
            if (c == size-1 && l == size -1) 
                then return (uPosG l c r b)
                else do
                    if (c == size-1)
                        then looper (l+1) (0) size (uPosG l c r b)
                        else do 
                            if (l == size-1)
                                then (looper l (c+1) size (uPosG l c r b))
                                else (looper l (c+1) size (uPosG l c r b))



uArrG :: Int -> Char -> [Char] -> [Char] -- eis aqui a morte da programação funcional
uArrG 0 v (x:xs)
    | x == '-'  = v:xs
    | otherwise = x:xs
uArrG p v (x:xs) = x : uArrG (p-1) v xs

uPosG :: Int -> Int ->  Char -> [[Char]] -> [[Char]]
uPosG 0 c v (x:xs) = (uArrG c v x):xs
uPosG l c v (x:xs) = x : (uPosG (l-1) c v xs)


fillWordTable :: Int -> Board -> WordList -> [String] -> IO (Board, WordList) -- amount=numero de palavras max q vai tentar por no tabuleiro
fillWordTable amount b wl wb = do
    palavra <- (randomRIO (0,106::Int))
    let wording = (gArr palavra wb)
    if (onList wording wl)
        then do fillWordTable amount b wl wb -- palavra ja foi adicionada, refaz a busca tentando achar outra
        else do
            if ((length wording) > (length b))
                then do fillWordTable amount b wl wb -- pra não desperdiçar uma iteração inteira
                else do
                    --putStrLn ((gArr palavra wb))
                    metodo <- (randomRIO (1,4::Int))
                    let ((l1,l2),(c1,c2)) = getOptimalLineAndCol metodo (length wording) (length b)
                    linha <- (randomRIO (l1,l2::Int))
                    coluna <- (randomRIO (c1,c2::Int))
                    if (amount > 1)
                        then do 
                            let (newb, newwl) = (inserir wording (linha,coluna) metodo wl b)
                            fateDice <- (randomRIO (True,False::Bool))
                            if (newb == b && fateDice) 
                                then fillWordTable (amount) newb newwl wb
                                else do fillWordTable (amount-1) newb newwl wb
                        else do
                            let (newb, newwl) = (inserir wording (linha,coluna) metodo wl b)
                            j <- (superFor $ newb)
                            return (j, newwl)
                            --putStrLn (printBoard j newwl)


getOptimalLineAndCol :: Int -> Int -> Int -> ((Int, Int),(Int, Int))  --retorna o upper bound e lower bound do valor "ótimo"
getOptimalLineAndCol 1 lengthw sizeb = ((0, sizeb-1),(0, sizeb-lengthw)) --getOptimalLineAndCol m lengthw sizeb =  tive essa ideia dormindo, praticamente
getOptimalLineAndCol 2 lengthw sizeb = ((0, sizeb-1),(lengthw-1, sizeb-1))
getOptimalLineAndCol 3 lengthw sizeb = ((0, sizeb-lengthw),(0, sizeb-1))
getOptimalLineAndCol 4 lengthw sizeb = ((lengthw-1, sizeb-1),(0, sizeb-1))


victory :: WordList -> Bool
victory []  = True
victory _   = False


-- DO NO GO BEYOUND THIS POINT

parse :: String -> (Int, Int)
parse (s) = outroOutroParse (outroParse (parseUm s))

outroOutroParse :: (String, String) -> (Int, Int)
outroOutroParse (a,b) = ((read a::Int),(read b::Int))

outroParse :: [String] -> (String, String)
outroParse (p:ps) = (p, (head ps))

parseUm :: String -> [String]
parseUm (s) = splitA s ' '

splitA :: String -> Char -> [String]
splitA [] cutat = [""]
splitA (c:cs) cutat
    | c == cutat = "" : rest
    | otherwise = (c : head rest) : tail rest
    where
        rest = splitA cs cutat

-- eu admito, ficou um lixo.
-- o que o cara n faz pra evitar pegar um pacote só pra dar split


getHeadFst :: WordList -> String
getHeadFst [] = []
getHeadFst ((p,t,m):l) = p

printWordsFound :: [String] -> String
printWordsFound [] = ""
printWordsFound (p:ps) = p ++ "\n" ++ printWordsFound ps