## Jardin ornamental

### Item 1

El resultado no suele ser el esperado porque hay un race condition en la variable
visitantes. Como el incremento de "visitantes" no es atómico, puede que un thread
(thread 1) lea el valor de la variable, luego el otro (thread 2) la incremente y cuando el primero haga el incremento voy a haber perdido el visitante que agrego el thread 2.

### Item 2

Con valores chicos de N_VISITANTES no se suele ver un resultado incorrecto porque
no hay suficientes iteraciones como para ver un race condition.

### Item 3

El valor mínimo es 2. Se da bajo la circunstancia de que:
El thread 1 lea el valor 0. Luego el thread 2 haga N_VISITANTES-1 incrementos.
Para que después el primero escriba el valor 1 en la variable. Finalmente, el
thread 2 lee el valor 1 en la variable, en el medio se mete el thread 1 y completa
sus iteraciones, y el thread 2 las pisa escribiendo un 2 en la variable.


## Filosofos

### Item 1

Puede ocurrir un deadlock si todos toman el tenedor a su derecha y ninguno llega
a tomar el izquierdo. Van a quedar esperando que se libere el lock de la izquierda.

### Item 2

Esta solución funciona porque si todos toman el tenedor a su derecha (que es el caso del deadlock) el que esta sentado a la derecha del zurdo va a poder comer porque su tenedor izquierdo estará libre.

### Item 3

Funciona porque quien este sentado a la derecha de alguien que no come, si tomo su tenedor derecho podrá tomar su izquierdo y comer. Si no gano el derecho, significa que quien esta a su derecha tomo el tenedor izquierdo lo que equivale a que este comiendo.


## Fumadores

### Item 1

Puede ocurrir un deadlock si el agente da dos recursos, de los cuales un fumador toma uno y otro el que queda. Haciendo que ambos fumadores queden esperando su otro recurso para poder fumar. Esto puede suceder con todas las combinaciones de recursos que ponga el agente.

### Item 2

La solución consiste en 3 threads nuevos (inspectores) que cada uno se encargue de esperar un recurso distinto. Si alguno ve un recurso, incrementa dos semáforos que corresponden a los fumadores que podrían fumar estando ese recurso en la mesa.
Cuando el agente ponga dos recursos, dos inspectores van a ejecutarse e incrementar los semáforos. Luego de esto los semáforos de "puede fumar" quedaran dos con valor 1 y uno solo con valor 2. Este ultimo seria el correspondiente al fumador que debería fumar.
Entonces otro thread (middle_man) decrementa todos los semáforos, quedando solo 1 con valor 1 y el resto en 0. Haciendo trywaits podemos decirle al fumador que le toca fumar que lo haga, utilizando otro semáforo.
