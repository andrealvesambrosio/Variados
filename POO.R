# Funcoes bonus ----
# Função cat
iter <- 0
for(i in 1:10){
  Sys.sleep(0.5)
  cat('iteration = ', iter <- iter + 1, '\r')
}

# Funções closure 
# (de cláusula, quando uma função depende de outra)
potencia <- function(exp){
  function(x){
    x^exp
  }
}

quadrado <- potencia(2)
quadruplo <- potencia(4)



# S3 ----
s = list(
  nome = 'André',
  idade = 22,
  curso = 'Bacharelado em Estatística',
  nota_calculo = 9,
  nota_alglin = 8,
  nota_inferencia = 7
  )

class(s) = 'estudante'

# Definindo uma função genérica
curso = function(obj){
  UseMethod("curso")
}
curso(s)

# Implementando um método default para a Curso
curso.default = function(obj){
  cat("Isso é uma função genérica")
}
curso(s)

# Criando um novo método para Curso
curso.estudante <- function(obj){
  cat("Seu curso é", obj$curso, "\n")
}
curso(s)




# Reference Class ----
cliente_banco <- setRefClass(
  # Nome da classe
  "cliente_banco",
  
  # Atributos da classe
  fields = list(
    nome = 'character',
    sobrenome = 'character',
    cpf = 'character',
    saldo_cc = 'numeric'
  ),
  
  # Métodos da classe
  methods = list(
    
    saque_cc = function(x){
      saldo_cc <<- saldo_cc - x
    },
    deposito_cc = function(x){
      saldo_cc <<- saldo_cc + x
    }
  )
  
)

paola <- cliente_banco(nome = 'Paola', sobrenome  = 'Gatinha', cpf = '123',
                       saldo_cc = 1000)


cat('\r', 'Seu saldo é: ', paola$saldo_cc)
paola$saque_cc(500)
cat('Seu saldo é: ', paola$saldo_cc)
paola$deposito_cc(100)
cat('Seu saldo é: ', paola$saldo_cc)


# R6
library(R6)
library(tidyverse)
Cliente_Banco <- R6Class(
  "Pessoa",
  
  public = list(
    
    # Valores públicos
    name = NULL,
    second_name = NULL,
    born = NULL,
    cc = NULL,
    ag = NULL,
    saldo_cc = NULL,
    saldo_poupanca = NULL,
    
    # Inicialização
    initialize = function(name, second_name, born, cc, ag, saldo_cc, saldo_poupanca){
      self$name = name
      self$second_name = second_name
      self$born = born
      self$cc = cc
      self$ag = ag
      self$saldo_cc = saldo_cc
      self$saldo_poupanca = saldo_poupanca
      self$greet()
      self$database
      self$popular_db(Acao = "Criar Conta", Valor = NA)
      
    },
    # Histórico de ações
    database = tibble(Data = character(), Acao = character(), Valor = numeric()),
    
    popular_db = function(Acao, Valor){
      new_row = tibble(Data = as.character(Sys.time()), Acao = Acao, Valor = Valor)
      self$database = self$database %>%
        bind_rows(new_row)
    },
    
    # Função de cumprimentar
    greet = function(){
      cat("Bem vindo,", self$name, self$second_name)
    },
    
    # Função de saque
    saque = function(local = c('poupanca', 'conta corrente'), valor){
      if(local == 'poupanca'){
        self$saldo_poupanca <- self$saldo_poupanca - valor
      }
      if(local == 'conta corrente'){
        self$saldo_cc <- self$saldo_cc - valor
      }
      self$popular_db(Acao = paste('Saque', local), Valor = valor)
    },
    
    # Função de depósito
    deposito = function(local = c('poupanca', 'conta corrente'), valor){
      if(local == 'poupanca'){
        self$saldo_poupanca <- self$saldo_poupanca + valor
      }
      if(local == 'conta corrente'){
        self$saldo_cc <- self$saldo_cc + valor
      }
      self$popular_db(Acao = paste('Deposito', local), Valor = valor)
    }
    
    
    )
  )

andre <- Cliente_Banco$new(name = "André", 
                           second_name = "Alves Ambrósio", 
                           born ='1997-07-24',
                           cc = '159913',
                           ag = '09806',
                           saldo_cc = 1000,
                           saldo_poupanca = 500)

andre$saque(local = 'poupanca', valor = 100)
andre$saque(local = 'conta corrente', valor = 250)

andre$deposito(local = 'poupanca', valor = 50)
andre$deposito(local = 'conta corrente', valor = 300)

andre
andre$database






































