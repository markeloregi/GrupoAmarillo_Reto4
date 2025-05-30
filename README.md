### DISCLAIMER 

El proyecto está incompleto en Github. Debido a algunos problemas de implementación de GitHub en el reto y perjudicados por tener que cambiar a mitad del reto de metodo de sincronización,
el proyecto ha sido desarrollado integramente en OneDrive. Aun así, se creó la correspondiente estructura de ramas en GitHub, y la carpeta esta compartida habilitando el trabajo
en local de todos los integrantes del grupo. Uno de los mayores problemas que nos surgió al cambiar de formato era que los Datos Transformados (faltantes en este repositorio,
hay que ejecutar el script de Reducción.R para que se actualicen y se crean los ficheros; lo mismo hace falta para que todos los scripts funcionen, ya que varios scripts funcionan mediante
esos ficheros de datos que no estan actualmente subidos en el repositorio por tamaño) varios scripts cambiaron su forma de trabajo por la diferencia de lectura de los archivos
CSV y RDS. Dando por hecho que este problema surgió a finales del Reto, y tras comentarlo con algun PM, se tomó la decisión de terminar el reto en OneDrive y hacer los commits
para actualizar y terminar de subir el Reto via GitHub. 

### LÓGICA DEL PROYECTO

El script principal sirve para ejecutar todo lo necesario para el cumplimiento del Reto. Comenzando por un preprocesamiento que puede encontrarse en su debida carpeta,
sigue por el análisis exploratorio, pasando a los objetivos. Por último, están los scripts de visualizacion, data mining y data science. Este último es el script de la API.
La API es uno de los scripts que funciona mediante un CSV que ha de generarse ejecutandose el proyecto completo. El funcionamiento de la API es simple: al ejecutar la API, debe
ingresarse un ID de cliente para que la API recomiende 5 productos segun sus similitudes con el resto de clientes. En visualización, por su parte, se ha desarrollado una APP
de Shiny donde poder visualizar el trabajo realizado con los datos de una forma interactiva. Y respecto a Data Mining se refiere, el script de los modelos sirve tan solo
para evaluar diferentes algoritmos a la hora de resolver el problema puesto sobre la mesa: hacer recomendaciones a clientes.



### Estructura del proyecto:
Listado de rutas de carpetas
El número de serie del volumen es 4E73-569C
C:.
│   Análisis exploratorio.R
│   Api.R
│   Clustering.R
│   customer_summary.RDS
│   Funcion.R
│   Librerias.R
│   outliers.R
│   principal.R
│   Reducción.R
│   RETO_04_PROYECTO_R.Rproj
│   Rplot01.png
│
├───Datos
│   ├───Originales
│   │       maestroestr.RDS
│   │       objetivos.RDS
│   │       tickets_enc.RDS
│   │
│   └───Transformados
├───Exportados
│       clientes_unicos_mes.csv
│       datos_cluster.csv
│       frecuencia_clientes.csv
│       tickets_por_cliente.csv
│       top10_clientes_compras.csv
│       top10_clientes_frecuentes.csv
│       top10_clientes_variedad.csv
│       top10_productos.csv
│
├───Gráficos
│       DENDROGRAMA CLUSTERING JERÁRQUICO .png
│       Grafico recomendaciones Objetivo 2.png
│       GRÁFICO CLÚSTERES EN 2D.png
│       MÉTODO DEL CODO.png
│       RECOMENDACIONES OBJETIVO 3.png
│
├───Resultados
│       objetivo_04_recomendaciones_lista.RDS
│       recoemendaciones_objetivo2.rds
│       resultados_Objetivo1.rds
│
├───Scripts_EDA
├───Scripts_modelos
│       clusters.csv
│       clusters.RDS
│       Modelos_Recomendacion_DMining.R
│
├───Scripts_objetivos
│       Objetivo 1-Unai.R
│       Objetivo 1.R
│       Objetivo 2.R
│       Objetivo 3.R
│       Objetivo 4.R
│
├───Scripts_preprocesamiento
│       Preprocesamiento.R
│       Reducción.R
│
├───Shiny
│       Visu.R
│
└───VISUALIZACION
    │   Visu.R
    │
    └───graficos
            Afinidad.rds
            frecuencia_visitas.rds
            graficos_obj2
            grafico_obj2.rds
            grafico_obj2_1.rds
            grafico_obj2_10.rds
            grafico_obj2_2.rds
            grafico_obj2_3.rds
            grafico_obj2_4.rds
            grafico_obj2_5.rds
            grafico_obj2_6.rds
            grafico_obj2_7.rds
            grafico_obj2_8.rds
            grafico_obj2_9.rds
            grafico_obj4.rds
            grafico_recomendaciones
            grafico_recom_obj3
            grafico_recom_obj3.rds
            medias_por_cluster.rds
            metodo_codo.rds
            plotly_dendro_euc.rds
            plotly_dendro_man.rds
            plot_3d.rds
            plot_comparacion_metodos.rds
            plot_frecuencia_cluster.rds
            plot_jer_euc.rds
            plot_jer_man.rds
            plot_kmeans_euc.rds
            plot_kmeans_man.rds
            plot_productos_clusters.rds
            plot_tickets_cluster.rds
            productos_por_ticket.rds
            tabla_obj3
            tabla_objetivo3
            tabla_objetivo4
            tabla_recomendaciones.rds
            tickets_totales.rds
            Top10_afinidad.rds
