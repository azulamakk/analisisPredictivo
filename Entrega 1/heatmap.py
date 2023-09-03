import pandas as pd
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt
from scipy.spatial.distance import pdist, squareform
from scipy.cluster.hierarchy import linkage, fcluster

# Cargar tus datos
datos = pd.read_csv("~/Universidad/Datasets/Predictiva/dataset.csv")

# Filtrar los datos
genres_to_keep = ['classical', 'metal', 'jazz', 'punk-rock', 'techno', 'reggae', 'sleep', 'trance', 'study', 'hip-hop']
datosFiltrados = datos[datos['track_genre'].isin(genres_to_keep)]

# Definir las columnas de identificación
id_cols = ["track_id", "artists", "album_name", "track_name", 'mode', "key", "time_signature",
            'explicit', 'track_genre', 'duration_ms']

# Crear un dataframe con las columnas numéricas normalizadas usando el método min-max
numeric_cols = datosFiltrados.select_dtypes(include=np.number).columns
gdat = datosFiltrados.copy()
gdat[numeric_cols] = (gdat[numeric_cols] - gdat[numeric_cols].min()) / (gdat[numeric_cols].max() - gdat[numeric_cols].min())

# Calcular unique(datosFiltrados['track_genre']) y order_hc una vez
unique_track_genre = datosFiltrados['track_genre'].unique()
dist_matrix = pdist(gdat.drop(columns=id_cols), metric='cityblock')
order_hc = linkage(dist_matrix, method="average")

num_clusters = 8

# Obtener los clústeres y las etiquetas correspondientes
clusters = fcluster(order_hc, t=num_clusters, criterion='maxclust')

# Crear un nuevo dataframe con las columnas genero1 y genero2 modificadas
gdat['genero1'] = pd.Categorical(clusters, categories=range(1, num_clusters + 1))
gdat['genero2'] = gdat['genero1'].copy()  # Copiar los mismos clústeres para genero2

# Crear una matriz de distancias
dist_matrix = squareform(dist_matrix)

# Crear el heatmap sin etiquetas en los ejes X e Y
plt.figure(figsize=(10, 8))
sns.heatmap(dist_matrix, cmap="viridis", xticklabels=False, yticklabels=False)
plt.title("Mapa de Calor de Distancias")
plt.xlabel("")
plt.ylabel("")
plt.show()
