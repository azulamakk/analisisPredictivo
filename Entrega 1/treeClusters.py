import pandas as pd
from scipy.cluster.hierarchy import linkage, dendrogram
from sklearn.preprocessing import StandardScaler
import matplotlib.pyplot as plt
import numpy as np

# Cargar los datos
datos = pd.read_csv("~/Universidad/Datasets/Predictiva/dataset.csv")

# Filtrar géneros
generos_seleccionados = ['classical', 'metal', 'jazz', 'punk-rock', 'techno', 'reggae', 'sleep', 'trance', 'study', 'hip-hop']
datosFiltrados = datos[datos['track_genre'].isin(generos_seleccionados)]

# Preprocesamiento
id_cols = ["track_id", "artists", "album_name", "track_name", 'mode', "key", "time_signature",
           'explicit', 'track_genre', 'duration_ms']

scaler = StandardScaler()
dat_c_scaled = scaler.fit_transform(datosFiltrados.drop(columns=id_cols))

# Matriz de distancias
dist_matrix = linkage(dat_c_scaled, method="average", metric="cityblock")

# Calcular los valores de distancia en incrementos de 0.5
max_distance = dist_matrix[:, 2].max()
min_distance = dist_matrix[:, 2].min()
step_size = 0.5
marked_distances = np.arange(min_distance, max_distance + step_size, step_size)

# Dendrograma
plt.figure(figsize=(10, 5))
dendrogram(dist_matrix, orientation='top')
plt.title("Dendrograma de Clustering Jerárquico")
plt.xlabel("Distancia")
plt.yticks(ticks=range(len(generos_seleccionados)), labels=generos_seleccionados)

# Agregar líneas verticales en las distancias marcadas con desplazamiento horizontal
for i, distance in enumerate(marked_distances):
    plt.axvline(x=i, color='red', linestyle='--', linewidth=0.8)

plt.xticks(ticks=range(len(marked_distances)), labels=["%.2f" % distance for distance in marked_distances], rotation=90)  # Etiquetas en distancias específicas
plt.show()
