# TP Final
---

## Compilación

El Makefile tiene 4 comandos principales

```shell
> make tests    # construye los tests
> make memcache # construye el programa principal
> make all      # construye los dos programas
> make clean    # borra los archivos de compilación ./build y ./bin
```

Dentro de la carpeta `./bin` se encuentran los dos programas, para correrlos simplemente escribimos

```shell
> ./bin/tests
> ./bin/memcache
```
Los tests que limitan la memoria no se llevan bien con `valgrind`, así que comentarlos es imperativo para revisar que no haya _memory leaks_. Por otro lado `valgrind` no se lleva bien con el _multithreading_, porque lo serializa, y hay tests que tardan una eternidad en finalizar. Nosotros testeamos con y sin `valgrind` quitando los tests adecuados para revisar que no haya _memory leaks_, pero la forma natural de correr los tests es así nomás.

## Trayecto del TP

#### Comenzando

Al inicio del proyecto, nos pareció pertinente pensar las estructuras de datos, mayormente para saber alrededor de qué iba a girar todo el código. A cada struct le correspondenría un módulo que serviría como API, y se volvería un detalle de implementación para la idea general.

Esto resultó bien en un comienzo, ya que podíamos poner sobre la mesa todas las partes que se nos ocurrían que podíamos necesitar, hacer de cuenta que había código que implementaba la idea, y ver en líneas generales cómo iban a interactuar todas las partes. Casi que podíamos ver luz al final del túnel.

Ideas muy interesantes comenzaron a surgir muy temprano en esta fase. Los ejemplos más contundentes son:
 * Llamar `bucket` a los nodos que contienen los datos, y almacenar los datos en `blob`s
 * Usar una tabla hash con direccionamiento abierto
 * Usar una LRU para liberar memoria
 * Embeber ambas estructuras en `bucket`, en la forma de 4 punteros, 2 `next` y 2 `prev` diferentes.
 * Meter los datos en `counter64`s concurrentes, y todos ellos en un `record`

A estas alturas no estaba claro dónde los locks eran necesarios, estaba la idea de bloquear cada `cell` de la tabla hash que apunta a una lista de `bucket` y bloquear toda la LRU. Esta idea era el respaldo. ¡Quizás granular los locks en cada `bucket` no es tan difícil! - Dicen los tontos.

Fast-forward al final de la arquitecturación, teníamos una API para el backend, que el frontend podía asumir cierta, así que el development del Backend y el Frontend era oficialmente paralelizable. ¡Así que a trabajar!

---
#### Backend

Algunas reuniones fueron necesarias para discutir más a fondo el desarrollo del Backend, y de estas surgieron importantes problemas de concurrencia e importantes soluciones. Pero ocurrieron más adelante, lo primero era pensar sobre el elefante en la sala...

###### Luciano en el colectivo

Pobre de mí, que suelo pensar más de la cuenta, cuando uso el transporte público dejo de prestar atención y empiezo a pensar en lo que venga. Estaba moviendo cosas de acá para allá en el Backend, y me puse a pensar en más adelante, cuando los locks sean importantes. ¿Qué íbamos a hacer? Dicen que los programas concurrentes se arquitecturan concurrentes desde el comienzo, pero no habíamos hecho eso bien, así que me puse a pensar.

Exploré la idea de poner un lock en cada `bucket`, no parecía funcionar, ¿Poner más? Cada `bucket` estaba atravesado por cuatro posibles direcciones, uno por cada puntero que tiene asociado. ¡Quizás que los locks sean para las conexiones y no los `bucket`s! ¿Pero dónde los pongo? Ah, tengo el mismo problema...

Así fue todo el viaje de 40 minutos. Pensaba trazas, donde interactúan la tabla hash y la LRU, todo se rompía. Pero la paz se ahondó en mí cuando me dí cuenta de un par de detalles que no habíamos tenido en cuenta.

1. Las tablas hash tienen baja probabilidad de colisión, e incluso si lo hacen la cantidad esperada por `cell` es baja
2. Las operaciones a la LRU, por cómo embebemos la estructura en los `bucket`s, son O(1)

¡Salvados! Entonces bloquear cada celda y la LRU entera no parecía tan malo después de todo. Además, para protegerme de la deshonra, di el argumento que de todos modos el _overhead_ de hacer locks granulares iba a ser peor al final de lo que se ganaría en paralelismo.

###### Industrialización

Ya teníamos estructuras de datos, pero nos faltaba código, y ese código debía estar mejor separado en componentes. Ya que las estructuras iniciales estaban todas amontonadas en pocos `struct`s. Cuando comenzó la escritura de código del Backend, nació la estructura de carpetas `src tests bin build Makefile` que es tan familiar. Se extrajeron algunas _utilities_ y fueron testeadas. Además el Makefile recibió mucho amor, para que la incorporación de nuevos módulos y de nuevos tests sea lo más _automágica_ posible.

###### Extracción de componentes

Si viéramos la estructura `database` original, tenía la tabla hash junto con sus celdas y la LRU sueltas. Así que al intentar escribir código relacionado a las operaciones el código era mucho y muy complejo. Así nacieron `lru_queue`, `cell` y `hash_table`, aunque al último lo retiramos, por razones explicadas más adelante.

Estos módulos fueron separados en archivos diferentes, con tests que probaban no sólo su funcionalidad, sino también su concurrencia, y en un comienzo esto funcionó, pero no iba a mantenerse así por siempre.

###### El problema de las referencias

Otro problema surgió en el medio del `development`, cuando todo el código parecía estar por empezar a poder intergrarse, y nos íbamos a tener que preocupar por el trabajo conjunto de múltiples estructuras que manejaban los mismo datos, y es que más de una se hacía responsable de la memoria, como si suya fuese.

Esto era un problema enorme, puesto que si borraban un `bucket` mientras otro _thread_ lo está leyendo para enviarlo como respuesta a un cliente, entonces esa memoria se va a liberar. Esto es C, así que eso quizás no rompa nada inmediatamente, el problema se presentó en realidad en la forma de `free`s dobles en situaciones específicas de testing.

¿Acaso necesitábamos locks para los `bucket`s? No podíamos soportar la idea de tener que añadir semejante complejidad de repente, cuando todo iba tan bien. Pero la respuesta provino del hermano menor y más radical de C, C++, con sus `shared_ptr`. Esta idea era justo lo que necesitábamos, así que esa misma noche luego de la reunión estratégica se hizo realidad. Y fue bueno. Muy bueno.

Ahora los `bucket`s contaban referencias y se eliminaban solos al llegar a 0, lo malo es que sin RAII, debíamos encargarnos de aumentarlas y reducirlas a mano ¿Pero a quién le importa? Reutilizamos `counter64`, no hay realidad mejor que esta. Un `SHARE` macro y todo se volvió bello. Eso sí, pensar la cantidad de referencias existentes en cada momento fue necesario. Ejemplo: crear nuevas referencias lo antes posible para pasar un `bucket` como argumento por la posibilidad de que alguien borre su referencia antes y perdamos el `bucket`.

###### La envidia de `database`

Se avecinaba el final, todo estaba testeado, funcionaba, quedaba integrarlo en `database` y cerrar el circuito. No fue tarea fácil.

El primer paso fue quitar todas las responsabilidades de memoria de los módulos y dejárselas a `database`. Era aquí que debía nacer y morir la memoria, puesto que tenemos un método para liberar memoria si se llega a terminar, y el programa debe continuar.

`database` es el centro de comandos, donde se toman todas las decisiones y reside toda la lógica de coordinación. Era una buena filosofía pero faltaba ponerla en práctica. El segundo paso fue quitar la responsabilidad de las estructuras de datos de poner y quitar sus propios locks, y dejar ese trabajo a alguna lógica superior.

Este es el punto donde habría que confesar que nuestra `hash_table` se redimensionaba cuando necesitaba memoria y superaba un 75% de ocupación. Resulta que esto era un problema enorme a la hora de coordinar, porque en cualquier momento había que parar todo y redimensionar. Como redimensionar es algo importante pero no algo _urgente_, o sea que podía esperar, la mejor solución era un _read write lock - write preferring_. Fácil de programar, poco uso de memoria, y funcionaba.

Este es el punto donde `hash_table` murió, porque era difícil que `database` maneje la memoria, cuando los grandes usos de memoria ocurrían en un módulo del cuál esta dependía. Así que el comenzamos a construir `database` de abajo para arriba con _Test Driven Development_ combinando código original con el de `hash_table`

###### Adiós `rw_lock`, nadie te va a extrañar

Lo más doloroso es que quizás no había necesidad de hacer nada de lo mencionado en los últimos dos párrafos, si tan solo hubiéramos prestado atención al consejo de hacer la tabla hash de tamaño fijo. Pero lo hecho, hecho está. Con este nuevo cambio, la vida se volvió más simple, 400 líneas y un módulo más simple.

_Bye bye 400 líneas de código_

###### Prueba final

El tramo final constaba en testear `database` concurrentemente, lo cuál parecía una tarea monumental, con posibilidades de fracaso brutal, porque era en este punto donde iban a surgir las fallas críticas de diseño que posiblemente obligarían a cambiar gran parte del Backend. Cada test que se hacía revelaba uno o dos _bugs_, lo cuál era reconfortante, ya que el código sin _bugs_ es demasiado sospechoso.

Lo más interesante vino con que las soluciones fueron bastante evidentes luego de poca indagación. Comentarios explicando cada una fueron dejados, porque eran observaciones demasiado interesantes para ser olvidadas.

Así que eso fue el final, el Backend funcionaba, y funciona. _Fingers crossed_

---
#### Frontend

// Completar