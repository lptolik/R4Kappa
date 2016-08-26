library(qmrparser)
#%agent: signature_expression
#signature_expression ::= Id(sig )
#sig ::= Id internal_state_list, sig | ε
#internal_state_list ::= ~Id internal_state_list | ε

agent_exp<- function()
  concatenation(
    keyword('%agent:'),whitespace(),signature_expression(),
    action=function(s) print(
      exprToString(
        s
      )
    )
  )
signature_expression<-function()
  concatenation(
    ID(),keyword('('),sig(),keyword(')'),
    action=function(s) print(
      exprToString(
        s[[1]]
      )
    )    
  )
sig<-function()
  concatenation(
    repetition0N(ID(),internal_state_list(),keyword(','),sig()),
    action=function(s) print(
      exprToString(
        s[[1]]
      )
    )    
  )

internal_state_list<-function()
  concatenation(
    keyword('~'),ID(),repetition0N(internal_state_list()),
    action=function(s) print(
      exprToString(
        s[[2]]
      )
    )    
  )
ID<-function()
  symbolic(charFirst=isLetter,charRest=function(ch) isLetter(ch) || isDigit(ch) || ch == "_"|| ch == "-" || ch == "+") 
