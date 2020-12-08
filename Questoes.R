####### Lista 2 - Estatística
####### Mestrado Profissionalizante em Economia
####### Aluno: Edevaldo Siqueira Gaudencio

####### Carregando pacotes


########Questão 1
# Um fabricante de peças de automóveis garante que uma caixa de suas peças conterá no 
# máximo, duas defeituosas. Se a caixa contém 18 peças, e sabendo que a experiência mostra 
# que 5% das peças produzidas apresentam defeito, qual a probabilidade de que uma caixa 
# satisfaça a garantia? 

# Dica use a função dbinom ou no excel DISTR.BINOM(número de sucesso;
# número de tentativas; probabilidade; cumulativo=1)  para calcular a probabilidade de 0 peças, 
# 1 peça ou 2 peças defeituosas e calcule a função de distribuição acumulada até 2.)

    # Interpretando o enunciado:
    # Amostra=18
    # Defeito=5% (0.05)
    # Tentativas=2
    
    prob_0 <- dbinom(0, 18, 0.05)
    prob_1 <- dbinom(1, 18, 0.05)
    prob_2 <- dbinom(2, 18, 0.05)
    sum(prob_0+prob_1+prob_2)


########Questão 2
# Se X tem distribuição binomial com parâmetros n=5 e p=½, faça os gráficos da 
# distribuição de X e a função de distribuição acumulada F(X).  

    # Interpretando o enunciado:
    # n=5
    # p=0.5
    par(mfrow = c(1, 2))
    x <- 0:5
    dist_x <- dbinom(x, size = 5, prob = 0.5)
    plot(x, dist_x, type = "h", xlab = "Distribuição Binomial de X")
    dist_acumulada <- pbinom(x, size = 5, prob = 0.5)
    plot(x, dist_acumulada, type = "s", xlab = "Distribuição Binomial Acumulada de X")
    par(mfrow = c(1, 2))


## item A:
# Considere n=5 e p=¼. Obtenha o gráfico da distribuição de X. 
# Qual a diferença do resultado do exercício anterior?

    # Interpretando o enunciado:
    # n=5
    # p=0.25
    par(mfrow = c(1, 2))
    x <- 0:5
    dist_x <- dbinom(x, size = 5, prob = 0.25)
    plot(x, dist_x, type = "h", xlab = "Distribuição Binomial de X")
    dist_acumulada <- pbinom(x, size = 5, prob = 0.25)
    plot(x, dist_acumulada, type = "s", xlab = "Distribuição Binomial Acumulada de X")
    par(mfrow = c(1, 2))


########Questão 3 - Distribuição Normal 
# xi é uma variável aleatória com distribuição normal padrão (média zero e variância 1).
## Item A
# Qual a média e variância de uma distribuição Normal (0,1)? Gere mil observações 
# dessa distribuição Normal (0,1). Calcule a média, variância, 1o. Quartil, media, 
# 3o. Quartil dessas observações geradas. Faça um box-plot e histograma dos dados 
# gerados, qual a sua conclusão com a comparação distribuição teórica. 
# (Dica: No excel vá em Dados/Análise de Dados/Gerar Números Aleatórios. 
# Selecione 1000 números e distribuição normal. No r, use a função rnorm) 

    # Interpretando o enunciado:
    #media=0
    #variancia=1
    #amostra=1000
    
    # Gerando amostra de mil observações
    set.seed(100)
    dist_normal <- rnorm(1000,0,1)
    
    # Média, 1o. Mediana, Quartil, 3o. Quartil
    summary(dist_normal)
    
    # Variância
    var(dist_normal)
    
    # Faça um box-plot dos dados gerado.
    boxplot(dist_normal, main = "Distirbuição Normal")
    
    # Faça um histograma dos dados gerado.
    par(mfrow = c(1, 1))
    hist(dist_normal, col = "darkblue", xlab = "Distirbuição Normal", 
         ylab = "Frequência", border = "white", main = "Histograma - Distirbuição Normal",
         ylim=c(0, 250))    


## Item B
# Gere mais quarto distribuições de uma distribuição normal padrão com 1000 observações. 
# Assim, ficaremos com 5 distribuições normais padrões independentes. (Se manteve a 
# semente fixa, lembre-se de alterá-la). Eleve ao quadrado cada uma das séries e some 
# os valores das 5 séries geradas. Essa soma, de acordo com os resultados acima, deve 
# ter distribuição Y ~ 2(5), ou seja, distribuição chi quadrado com 5 graus de 
# liberdade. Sabemos também que E(Y)=número de graus=5 e Var(Y)=2*número de graus 
# de liberdade=10, uma vez que número de graus de liberdade de X é 5. Faça um 
# histograma, calcule a média e variância. Compare os resultados com os valores 
# teóricos da distribuição 2(5).    

    # Interpretando o enunciado:
    # 5 distribuições normais independentes
    # Elevar ao quadrado 
    # Some os valores das 5 séries geradas
    # Faça um histograma
    # Calcule a média e variância.
    
    
    # Gerando 5 amostras de mil observações cada
    set.seed(101)
    dist_normal_1 <- rnorm(1000,0,1) 
    set.seed(102)
    dist_normal_2 <- rnorm(1000,0,1) 
    set.seed(103)
    dist_normal_3 <- rnorm(1000,0,1)     
    set.seed(104)
    dist_normal_4 <- rnorm(1000,0,1) 
    set.seed(105)
    dist_normal_5 <- rnorm(1000,0,1) 
    
    # Elevar ao quadrado cada uma das distribuições e somar os valores das 5 séries geradas
    qui_quadrado <- (dist_normal_1^2+dist_normal_2^2+dist_normal_3^2+dist_normal_4^2+dist_normal_5^2)    
    
    # Faça um histograma
    par(mfrow = c(1, 1))
    hist(qui_quadrado , col = "darkblue", xlab = "Distribuição Qui-Quadrado", 
         ylab = "Frequência", border = "white", main = "Histograma - Distribuição Qui-Quadrado",
         ylim=c(0, 350), xlim=c(0, 20))   
    
    # Média, 1o. Mediana, Quartil, 3o. Quartil
    summary(qui_quadrado)
    
    # Variância
    var(qui_quadrado)
    
    # Comparando com os valores teóricos, percebe-se que a variancia tende a 10 e a média tende a 5. 
    # Realizada uma simulação com 10.000 números em cada distribuição normal, os valores foram ainda
    # mais próximos do referencial teórico: média de 4.94 e variancia de 9.67
    

## Item C
# Gere agora uma normal X~ N(5,10), primeiro vamos padronizar essa série, isto é, vamos gerar 
# uma nova série da seguinte forma: 
# Calcule a média e desvio-padrão de z, faça um histograma de z. 
# De fato, podemos dizer que Z~N(0,1)_.
    
    # Interpretando o enunciado:    
    # Média: 5
    # Variancia: 10
    # Amostra: 1000
    
    # Gerando amostra de mil observações
    set.seed(106)
    dist_normal_6 <- rnorm(1000,5,sqrt(10))
    
    #vamos padronizar essa série: Z=(X-μ)/√(σ^2 )
    dist_normal_6_padronizada <- (dist_normal_6 - 5)/sqrt(10)
    
    # Faça um histograma
    par(mfrow = c(1, 1))
    hist(dist_normal_6_padronizada, col = "darkblue", xlab = "Distribuição Normal", 
         ylab = "Frequência", border = "white", main = "Histograma - Normal",
         ylim=c(0, 250))   
    
    # Média, 1o. Mediana, Quartil, 3o. Quartil
    summary(dist_normal_6_padronizada)
    
    # Desvio padrão
    sd(dist_normal_6_padronizada)
    
    # A média tende a zero e o desvio padrão tende a 1.
    # Realizada uma simulação com 100.000 números na distribuicao , os valores foram ainda
    # mais próximos do referencial teórico: média de -0.0004 e devio padrão de  0.9963.
    
    
## Item D
# Por fim, vamos simular uma distribuição t(5) com 5 graus de liberdade. 
# Use as séries geradas no item para calcular:t(5)=Z/√(Y/5),
# Use os valores gerados no item c) para o numerador e os valores gerados no item b) 
# para o denominador. Calcule média e variância dessa nova distribuição. 
# Faça um histograma e um box-plot. Há diferenças para a normal (0,1)? 
    
    # Interpretando o enunciado:    
    # simular uma distribuição t(5) com 5 graus de liberdade
    # Valores do item C para o numerador e B para denominador
    # Calcule média e variância dessa nova distribuição
    # Faça um histograma e um box-plot
    # Diferenças em relação a distribuição normal (0,1?
    
    # simular uma distribuição t(5)
    dist_t_student <- (dist_normal_6_padronizada/(sqrt(qui_quadrado/5)))
    
    # Média, 1o. Mediana, Quartil, 3o. Quartil
    summary(dist_t_student)
    
    # Variância
    var(dist_t_student)
    
    # Faça um histograma
    par(mfrow = c(1, 1))
    hist(dist_t_student, col = "darkblue", xlab = "Distribuição t-Student", 
         ylab = "Frequência", border = "white", main = "Histograma - t-Student",
         xlim=c(-6, 6), ylim=c(0, 500))   
    
    # A média tende a zero e a variancia tende a 2.
    # Na distribuição normal, a variância tendia a 1
