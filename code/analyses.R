library(tidyverse)
library(igraph)
library(openxlsx)

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

# INTRA-SPECIES ANALYSIS -------------------------------------------------------

# Read the networks from texts of the same species from the paired samples, 
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

# Read the networks from texts of the same species from the paired samples, 
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
  mutate(edges_index_log = log10(edges_index))

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
  mutate(edges_index_log = log10(edges_index))

# Intra-species vertices index distribution
indices_icic %>%
  mutate(corpus = 'ICIC') %>%
  bind_rows(indices_extr %>%
              mutate(corpus = 'EXTR')) %>%
  group_by(corpus) %>%
  mutate(M = mean(vertices_index),
            SE = sd(vertices_index)/sqrt(n()),
            ymin = M - (1.96 * SE),
            ymax = M + (1.96 * SE)) %>%
  ggplot(aes(corpus, M, ymin = ymin, ymax = ymax)) +
  geom_point() + 
  geom_errorbar()

# t(1274) = 27.77, p < .001, 95% CI = [0.023, 0.027]
t.test(indices_icic$vertices_index, indices_extr$vertices_index)

# Intra-species edges index distribution
indices_icic %>%
  mutate(corpus = 'ICIC') %>%
  bind_rows(indices_extr %>%
              mutate(corpus = 'EXTR')) %>%
  filter(edges_index_log != -Inf) %>%
  group_by(corpus) %>%
  mutate(M = mean(edges_index_log),
         SE = sd(edges_index_log)/sqrt(n()),
         ymin = M - (1.96 * SE),
         ymax = M + (1.96 * SE)) %>%
  ggplot(aes(corpus, M, ymin = ymin, ymax = ymax)) +
  geom_point(size = 2) + 
  geom_errorbar(width = .1) +
  geom_jitter(alpha = .1, width = .1)

# TODO t-test with log transformed values with inf values removed
indices_icic %>%
  mutate(corpus = 'ICIC') %>%
  bind_rows(indices_extr %>%
              mutate(corpus = 'EXTR')) %>%
  filter(edges_index_log != -Inf) %>%
  t.test(edges_index_log ~ corpus, .)

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

# INTER-SPECIES ANALYSIS -------------------------------------------------------

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
  bind_rows()

# Inter- and intra-species vertices index distributions
indices_icic %>%
  mutate(corpus = 'ICIC') %>%
  bind_rows(indices_extr %>%
              mutate(corpus = 'EXTR')) %>%
  bind_rows(indices_inter %>%
              mutate(corpus = 'INTER')) %>%
  ggplot(aes(vertices_index, group = corpus, fill = corpus)) +
  geom_density(alpha = .5)

# t(2094.6) = -13.47, p < .001, 95% CI = [-0.013, -0.010]
t.test(indices_inter$vertices_index, indices_extr$vertices_index)

# Intra-species edges index distribution
indices_icic %>%
  mutate(corpus = 'ICIC') %>%
  bind_rows(indices_extr %>%
              mutate(corpus = 'EXTR')) %>%
  bind_rows(indices_inter %>%
              mutate(corpus = 'INTER')) %>%
  ggplot(aes(edges_index, group = corpus, fill = corpus)) +
  geom_density(alpha = .5) +
  scale_x_log10()

# TODO t-test with log transformed values with inf values removed
