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





