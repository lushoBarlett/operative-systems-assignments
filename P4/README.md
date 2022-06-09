Ej 3

b) vemos una diferencia importante en el uso de memoria a favor del servidor en Erlang.
Creemos que se debe al espacio que ocupa la creacion de threads con cada cliente.
Con 10000 clientes ya vemos que ocupamos toda la memoria disponible del sistema.

c) No podemos atender 50000 conexiones en simultaneo con el servidor en C porque ocuparia
demasiada memoria, siendo que cada uno tiene un stack distinto. Ademas es convencion tener
alrededor de 32000 PIDs. El de Erlang podria manejarlas.
