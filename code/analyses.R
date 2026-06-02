library(tidyverse)
library(igraph)
library(openxlsx)
library(jtools)
library(ggbeeswarm)

set_theme(theme_apa())

# ANALYSIS NUMBER OF SEGMENTS PER TEXT -----------------------------------------
descriptives_icic = list.files('data/cartas_icic_OANC/', 
                               '_descriptives.xlsx') %>%
  lapply(function(file) {
    read.xlsx(paste0('data/cartas_icic_OANC/', file)) %>%
      filter(Variable == 'Number of segments') %>%
      mutate(text = file)
  }) %>%
  bind_rows()

descriptives_extr = list.files('data/cartas_extraordinarias/', 
                               '_descriptives.xlsx') %>%
  lapply(function(file) {
    read.xlsx(paste0('data/cartas_extraordinarias/', file)) %>%
      filter(Variable == 'Number of segments') %>%
      mutate(text = file)
  }) %>%
  bind_rows()

# Distribution of number of segments in the ICIC corpus
descriptives_icic %>%
  ggplot(aes(Value)) +
  geom_density()

# Distribution of number of segments in the EXTR corpus
descriptives_extr %>%
  ggplot(aes(Value)) +
  geom_density()

# Read the table with the manually created paired samples
paired_samples = read.xlsx('data/paired_samples.xlsx')

# Calculate the intertextuality indices for vertices and edges of two given 
# networks as igraph objects
calculate_intextextuality_indices = function(G1, G2) {
  # Identify vertices and edges in the first network
  G1 = G1 %>%
    set_vertex_attr(name = 'network', value = '1') %>%
    set_edge_attr(name = 'network', value = '1')
  
  # Identify vertices and edges in the second network
  G2 = G2 %>%
    set_vertex_attr(name = 'network', value = '2') %>%
    set_edge_attr(name = 'network', value = '2')
  
  # Create a union by amalgamation of both networks
  G_union = G1 %u% G2
  
  # Identify which edges were present in both networks
  E(G_union)$network = ifelse(!is.na(E(G_union)$network_1) & 
                                !is.na(E(G_union)$network_2), 'both',
                              ifelse(!is.na(E(G_union)$network_1), '1', '2'))
  
  # Identify which vertices were present in both networks
  V(G_union)$network = ifelse(!is.na(V(G_union)$network_1) & 
                                !is.na(V(G_union)$network_2), 'both',
                              ifelse(!is.na(V(G_union)$network_1), '1', '2'))
  
  # Clear the unnecessary vertices' and edges' attributes
  G_union = G_union %>%
    delete_vertex_attr('network_1') %>%
    delete_vertex_attr('network_2') %>%
    delete_edge_attr('network_1') %>%
    delete_edge_attr('network_2')
  
  # Format the returned information as a tibble
  tibble(vertices_index = mean(V(G_union)$network == 'both'),
         edges_index = mean(E(G_union)$network == 'both'))
}

# INTRA-CATEGORY ANALYSIS -------------------------------------------------------

# Read the networks from texts of the same category from the paired samples, 
# stored in XLSX, convert to igraph objects and store them into a list
networks_icic = paired_samples$icic %>% 
  lapply(function(text){
    read.xlsx(paste0('data/cartas_icic_OANC/', text, '_network.xlsx'), 
              sheet = 2) %>%
      select(Source, Target) %>%
      as.matrix() %>%
      graph_from_edgelist() %>%
      set_graph_attr('text', text)
  })

# Read the networks from texts of the same category from the paired samples, 
# stored in XLSX, convert to igraph objects and store them into a list
networks_extr = paired_samples$extr %>%
  lapply(function(text){
    read.xlsx(paste0('data/cartas_extraordinarias/', text, '_network.xlsx'), 
              sheet = 2) %>%
      select(Source, Target) %>%
      as.matrix() %>%
      graph_from_edgelist() %>%
      set_graph_attr('text', text)
  })

# Calculate the indices for each pair of network in the ICIC list
indices_icic = seq_along(networks_icic) %>% 
  lapply(function(i) {
    seq_along(networks_icic) %>% 
      lapply(function(j) {
        if (i < j) {
          calculate_intextextuality_indices(networks_icic[[i]], 
                                            networks_icic[[j]]) %>%
            mutate(network_1 = networks_icic[[i]]$text, 
                   network_2 = networks_icic[[j]]$text)
        }
      })
  }) %>%
  bind_rows() %>%
  mutate(edges_index_log = log10(edges_index + 1))

# Calculate the indices for each pair of networks in the EXTR list
indices_extr = seq_along(networks_extr) %>% 
  lapply(function(i) {
    seq_along(networks_extr) %>% 
      lapply(function(j) {
        if (i < j) {
          calculate_intextextuality_indices(networks_extr[[i]], 
                                            networks_extr[[j]]) %>%
            mutate(network_1 = networks_extr[[i]]$text, 
                   network_2 = networks_extr[[j]]$text)
        }
      })
  }) %>%
  bind_rows() %>%
  mutate(edges_index_log = log10(edges_index + 1))

# Intra-category indices
indices_intra = indices_icic %>%
  mutate(corpus = 'ICIC') %>%
  bind_rows(indices_extr %>%
              mutate(corpus = 'EXTR'))

## Figure 2.1 ------------------------------------------------------------------

# Intra-category vertices index distribution
Figure2.1 = indices_intra %>%
  group_by(corpus) %>%
  mutate(M = mean(vertices_index),
            SE = sd(vertices_index)/sqrt(n()),
            ymin = M - (1.96 * SE),
            ymax = M + (1.96 * SE)) %>%
  ggplot(aes(corpus, vertices_index, ymin = ymin, ymax = ymax, fill = corpus)) +
  geom_boxplot() +
  geom_quasirandom(alpha = .1, stroke = NA) +
  geom_point(aes(y = M), size = 2, color = 'white') +
  geom_errorbar(width = .1, color = 'white') +
  xlab('Categoria') +
  ylab('Índice de Intertextualidade Lexical\n de Vértices') +
  scale_fill_discrete(palette = 'Dark2') +
  guides(fill = 'none')

ggsave('results/Figura2.1.png', 
       Figure2.1, 
       'png', 
       width = 85, 
       height = 96, 
       units = 'mm')

## Figure 2.2 ------------------------------------------------------------------

# Intra-category edges index distribution
Figure2.2 = indices_intra %>%
  group_by(corpus) %>%
  mutate(M = mean(edges_index_log),
         SE = sd(edges_index_log)/sqrt(n()),
         ymin = M - (1.96 * SE),
         ymax = M + (1.96 * SE)) %>%
  ggplot(aes(corpus, 
             edges_index_log, 
             ymin = ymin, 
             ymax = ymax, 
             fill = corpus)) +
  geom_boxplot() +
  geom_quasirandom(alpha = .1, stroke = NA) +
  geom_point(aes(y = M), size = 2, color = 'white') +
  geom_errorbar(width = .1, color = 'white') +
  xlab('Categoria') +
  ylab('Índice de Intertextualidade Lexical\n de Arestas (log)') +
  scale_fill_discrete(palette = 'Dark2') +
  guides(fill = 'none')

ggsave('results/Figura2.2.png', 
       Figure2.2, 
       'png', 
       width = 85, 
       height = 96, 
       units = 'mm')

## Example 1 -------------------------------------------------------------------

# Example of analysis with the pair of networks with higher intertextuality 
# indices in the ICIC corpus
g_120CUL044 = read.xlsx('data/cartas_icic_OANC/120CUL044_network.xlsx',
                        sheet = 2) %>%
  select(Source, Target) %>%
  as.matrix() %>%
  graph_from_edgelist() %>%
  set_graph_attr('text', file) %>%
  set_vertex_attr(name = 'network', value = '1') %>%
  set_edge_attr(name = 'network', value = '1')

g_120CUL045 = read.xlsx('data/cartas_icic_OANC/120CUL045_network.xlsx',
                        sheet = 2) %>%
  select(Source, Target) %>%
  as.matrix() %>%
  graph_from_edgelist() %>%
  set_graph_attr('text', file) %>%
  set_vertex_attr(name = 'network', value = '2') %>%
  set_edge_attr(name = 'network', value = '2')

g_union_1 = g_120CUL044 %u% g_120CUL045

E(g_union_1)$network = ifelse(!is.na(E(g_union_1)$network_1) & 
                              !is.na(E(g_union_1)$network_2), 'both',
                            ifelse(!is.na(E(g_union_1)$network_1), '1', '2'))

V(g_union_1)$network = ifelse(!is.na(V(g_union_1)$network_1) & 
                              !is.na(V(g_union_1)$network_2), 'both',
                            ifelse(!is.na(V(g_union_1)$network_1), '1', '2'))

g_union_1 = g_union_1 %>%
  delete_vertex_attr('network_1') %>%
  delete_vertex_attr('network_2') %>%
  delete_edge_attr('network_1') %>%
  delete_edge_attr('network_2')

# Write list of edges
g_union_1 %>%
  as_data_frame('edges') %>%
  rename(Source = from, Target = to) %>%
  mutate(Type = 'Undirected') %>%
  write_csv('results/union_120CUL044_120CUL045_edges.csv')

# Write list of vertices
g_union_1 %>%
  as_data_frame('vertices') %>%
  rename(Id = name) %>%
  mutate(Label = Id) %>%
  write_csv('results/union_120CUL044_120CUL045_vertices.csv')

## Example 2 -------------------------------------------------------------------
# Example of analysis with the pair of networks with lower intertextuality 
# indices
g_301CUL073 = read.xlsx('data/cartas_icic_OANC/301CUL073_network.xlsx',
                        sheet = 2) %>%
  select(Source, Target) %>%
  as.matrix() %>%
  graph_from_edgelist() %>%
  set_graph_attr('text', file) %>%
  set_vertex_attr(name = 'network', value = '1') %>%
  set_edge_attr(name = 'network', value = '1')

g_501C_L078 = read.xlsx('data/cartas_icic_OANC/501C-L078_network.xlsx',
                        sheet = 2) %>%
  select(Source, Target) %>%
  as.matrix() %>%
  graph_from_edgelist() %>%
  set_graph_attr('text', file) %>%
  set_vertex_attr(name = 'network', value = '2') %>%
  set_edge_attr(name = 'network', value = '2')

g_union_2 = g_301CUL073 %u% g_501C_L078

E(g_union_2)$network = ifelse(!is.na(E(g_union_2)$network_1) & 
                              !is.na(E(g_union_2)$network_2), 'both',
                            ifelse(!is.na(E(g_union_2)$network_1), '1', '2'))

V(g_union_2)$network = ifelse(!is.na(V(g_union_2)$network_1) & 
                              !is.na(V(g_union_2)$network_2), 'both',
                            ifelse(!is.na(V(g_union_2)$network_1), '1', '2'))

g_union_2 = g_union_2 %>%
  delete_vertex_attr('network_1') %>%
  delete_vertex_attr('network_2') %>%
  delete_edge_attr('network_1') %>%
  delete_edge_attr('network_2')

# Write list of edges
g_union_2 %>%
  as_data_frame('edges') %>%
  rename(Source = from, Target = to) %>%
  mutate(Type = 'Undirected') %>%
  write_csv('results/union_301CUL073_501C-L078_edges.csv')

# Write list of vertices
g_union_2 %>%
  as_data_frame('vertices') %>%
  rename(Id = name) %>%
  mutate(Label = Id) %>%
  write_csv('results/union_301CUL073_501C-L078_vertices.csv')

# INTER-CATEGORY ANALYSIS ------------------------------------------------------

# Calculate the indices for each pair of networks in different lists
indices_inter = seq_along(networks_icic) %>% 
  lapply(function(i) {
    seq_along(networks_extr) %>% 
      lapply(function(j) {
        calculate_intextextuality_indices(networks_icic[[i]], 
                                          networks_extr[[j]]) %>%
          mutate(network_1 = networks_icic[[i]]$text, 
                 network_2 = networks_extr[[j]]$text)
      })
  }) %>%
  bind_rows() %>%
  mutate(edges_index_log = log10(edges_index + 1))

# All indices
indices = indices_icic %>%
  mutate(corpus = 'ICIC') %>%
  bind_rows(indices_extr %>%
              mutate(corpus = 'EXTR')) %>%
  bind_rows(indices_inter %>%
              mutate(corpus = 'INTER'))

## Figure 3.1 ------------------------------------------------------------------

# Inter- and intra-category vertices index distributions
Figure3.1 = indices %>%
  group_by(corpus) %>%
  mutate(M = mean(vertices_index),
         SE = sd(vertices_index)/sqrt(n()),
         ymin = M - (1.96 * SE),
         ymax = M + (1.96 * SE)) %>%
  ggplot(aes(corpus, vertices_index, ymin = ymin, ymax = ymax, fill = corpus)) +
  geom_boxplot() +
  geom_quasirandom(alpha = .1, stroke = NA) +
  geom_point(aes(y = M), size = 2, color = 'white') +
  geom_errorbar(width = .1, color = 'white') +
  xlab('Categoria') +
  ylab('Índice de Intertextualidade Lexical\n de Vértices') +
  scale_fill_discrete(palette = 'Dark2') +
  guides(fill = 'none')

ggsave('results/Figura3.1.png', 
       Figure3.1, 
       'png', 
       width = 170, 
       height = 96, 
       units = 'mm')

## Figure 3.2 -------------------------------------------------------------------

# Inter- and intra-category edges index distributions
Figure3.2 = indices %>%
  group_by(corpus) %>%
  mutate(M = mean(edges_index_log),
         SE = sd(edges_index_log)/sqrt(n()),
         ymin = M - (1.96 * SE),
         ymax = M + (1.96 * SE)) %>%
  ggplot(aes(corpus, edges_index_log, ymin = ymin, ymax = ymax, fill = corpus)) +
  geom_boxplot() +
  geom_quasirandom(alpha = .1, stroke = NA) +
  geom_point(aes(y = M), size = 2, color = 'white') +
  geom_errorbar(width = .1, color = 'white') +
  xlab('Categoria') +
  ylab('Índice de Intertextualidade Lexical\n de Arestas (log)') +
  scale_fill_discrete(palette = 'Dark2') +
  guides(fill = 'none')

ggsave('results/Figura3.2.png', 
       Figure3.2, 
       'png', 
       width = 170, 
       height = 96, 
       units = 'mm')

## Table 1 ---------------------------------------------------------------------

indices %>%
  select(corpus, vertices_index, edges_index_log) %>%
  pivot_longer(c(vertices_index, edges_index_log)) %>%
  group_by(corpus, name) %>%
  summarise(M = mean(value),
            SE = sd(value)/sqrt(n()),
            ymin = M - (1.96 * SE),
            ymax = M + (1.96 * SE))

# LINEAR MODELS ----------------------------------------------------------------

# Differences between intra-category vertices indices of both categories
indices_intra %>%
  mutate(corpus = corpus %>% as_factor()) %>%
  lm(vertices_index ~ corpus, data = .) %>%
  summary()

# Differences between intra-category edges indices of both categories
indices_intra %>%
  mutate(corpus = corpus %>% as_factor()) %>%
  lm(edges_index_log ~ corpus, data = .) %>%
  summary()

# Differences between inter- and intra-category vertices indices
indices %>%
  mutate(corpus = corpus %>% as_factor() %>% relevel('INTER')) %>%
  lm(vertices_index ~ corpus, data = .) %>%
  summary()

# Differences between inter- and intra-category edges indices
indices %>%
  mutate(corpus = corpus %>% as_factor() %>% relevel('INTER')) %>%
  lm(edges_index_log ~ corpus, data = .) %>%
  summary()
