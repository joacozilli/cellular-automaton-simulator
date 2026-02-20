# Sobre CAsim
CAsim es un lenguaje de dominio específico para simular autómatas celulares; provee una forma sencilla
y expresiva de definirlos y permite observar sus evoluciones con una interfaz gráfica amigable y fácil de usar.


# Guía de instalación

El programa necesita las librerias de C de OpenGL y GLUT. En Ubuntu, se pueden instalar con:

sudo apt install freeglut3-dev libgl1-mesa-dev

También se necesita la herramienta stack. Para instalarlo consulte su página: https://docs.haskellstack.org/en/stable/

Todo lo demás necesario debería ser instalado automáticamente por stack al correr stack build.


# Guía de uso

Para compilar el programa, usar:

stack build

Una vez compilado, para ejecutar el programa, usar:

stack exec CAsim-exe -- FILEPATH [OPTIONS]

donde FILEPATH es la ubicación del archivo en el que está definido el autómata.

[OPTIONS] es una serie de opciones que se detallan a continuación:
- -r N la cantidad de filas de la red pasa a ser N (valor por defecto: 100 , mínimo: 10, máximo: 1000)

- -c M la cantidad de columnas de la red pasa a ser M (valor por defecto: 100 , mínimo: 10, máximo: 1000)

- -f CHOICE, donde CHOICE es toroidal o default, especifica el tipo de frontera en la simulación (valor
por defecto: default)

- -s K la cantidad de pasos por segundo en la simulación pasa a ser K (valor por defecto: 10 , mínimo: 1,
máximo: 30)

- -g CHOICE, donde CHOICE es seq o par, especifica si la función de transición global computa cada celda
secuencialmente o en paralelo respectivamente (valor por defecto: par). La opción en paralelo hará
que el programa use todos los núcleos disponibles.

Una vez ejecutado, se abrirá una ventana gráfica para comenzar la simulación.

Para manipular la simulación, CAsim ofrece los siguientes eventos:

- lick izquierdo sobre una celda rota su estado entre los definidos. Esto permite dar la configuración
inicial y solo es posible si la simulación todavía no ha comenzado.

- Barra espaciadora pausa/reanuda la simulación.

- N avanza la simulación en un solo paso. Solo funciona si está en pausa.

- R reanuda la simulación al estado inicial.

- WASD para mover la cámara.

- C regresa la cámara al centro.

- Rueda del ratón/flechas arriba y abajo para acercar/alejar la cámara.

- ESC cierra la ventana y finaliza el programa.