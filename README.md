# Índices de Intertextualidade Lexical baseados em redes semânticas [dados e código-fonte]

[![](https://zenodo.org/badge/DOI/10.5281/zenodo.20449709.svg)](https://doi.org/10.5281/zenodo.20449709)

O presente repositório disponibiliza os dados e código-fonte utilizado no artigo intitulado "Índices de Intertextualidade Lexical baseados em redes semânticas", de autoria de Davi Alves Oliveira, Roberto Carlos dos Santos Pacheco e Hernane Borges de Barros Oereira, submetido para apresentação no IV Simpósio Brasileiro de Ciência e Teoria das Redes.

## Resumo do trabalho

A intertextualidade refere-se às formas pelas quais os textos se relacionam entre si, implícita ou explicitamente. Uma dessas formas ocorre pelo uso de conjuntos semelhantes de itens lexicais. O presente estudo define índices de intertextualidade baseados nas interseções de vértices e de arestas de pares de redes que representam textos. A hipótese levantada foi a de que pares de textos de uma mesma categoria (análise intracategoria) apresentam maior similaridade no léxico utilizado, resultando em maiores índices de intertextualidade, enquanto pares de textos de diferentes categorias (análise intercategoria) apresentam índices menores. Para testar a hipótese, comparamos os índices de intertextualidade intercategoria e intracategoria de cartas de dois corpora distintos. Os resultados corroboram essa hipótese, mas também mostram diferenças entre categorias na análise intracategoria, sugerindo que os índices podem ser sensíveis à autoria ou a outras características do processo de produção textual.

**PALAVRAS-CHAVE:** intertextualidade; textualidade; redes semânticas; redes textuais.

## Materiais suplementares

### Tabela S.1 - Modelo linear com valores de $\chi_{\bullet}$

| Coeficientes         | Estimativa | Erro Padrão | Valor *t* | Valor *p* |
|----------------------|------------|-------------|-----------|-----------|
| Coef. Linear (INTER) | $0,0445$   | $0,0005$    | $90,12$   | $<0,001$  |
| ICIC                 | $0,0370$   | $0,0009$    | $42,87$   | $<0,001$  |
| EXTR                 | $0,0116$   | $0,0009$    | $13,48$   | $<0,001$  |

: $R^2=0,26$ (múltiplo); $R^2=0,26$ (ajustado)

### Tabela S.2 - Modelo linear com valores de $\underline{\chi}$

| Coeficientes         | Estimativa | Erro Padrão | Valor *t* | Valor *p* |
|----------------------|------------|-------------|-----------|-----------|
| Coef. Linear (INTER) | $0,00127$  | $0,00005$   | $27,53$   | $<0,001$  |
| ICIC                 | $0,00198$  | $0,00008$   | $24,55$   | $<0,001$  |
| EXTR                 | $0,00095$  | $0,00008$   | $11,77$   | $<0,001$  |

: $R^2=0,11$ (múltiplo); $R^2=0,11$ (ajustado)
