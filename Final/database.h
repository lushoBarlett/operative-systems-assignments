/*
 * Un bucket contiene toda la información relevante
 * a los valores almacenados en la base de datos,
 * implementada como una tabla hash.
 *
 * Está incluida la lista enlazada usada para resolver
 * colisiones de la tabla hash.
 *
 * Está también incluida una cola, implementada como
 * una lista doblemente enlazada, para determinar
 * qué valores borrar de la caché cuando se supera
 * el límite de memoria establecido.
 */
typedef struct bucket_t {
	/*
	 * Puntero al valor almacenado,
	 * seguido de la cantidad de bytes
	 * que este valor ocupa en memoria.
	 */
	void* value;
	uint64_t bytes;

	/*
	 * TODO: explicar
	 */
	uint64_t cell_index;

	/*
	 * TODO: exclusión mutua de "filas"
	 */

	/*
	 * Siguiente valor en la lista enlazada de datos
	 * que han colisionado por ser mapeados a la misma
	 * celda de la tabla hash.
	 * 
	 * TODO: es una doble
	 */
	struct bucket_t* next_value;
	struct bucket_t* prev_value;

	/*
	 * Usamos una lista doblemente enlazada para
	 * representar una cola de valores ordenados
	 * por su última instancia de uso en la base de datos.
	 *
	 * De esa forma, está al frente el valor menos relevante
	 * y si necesitamos borrarlo lo podemos hacer.
	 *
	 * El siguiente valor de un nodo de la cola es aquel
	 * que se borraría después, y el anterior es aquel
	 * que se borraría antes.
	 */
	struct bucket_t* next_queue;
	struct bucket_t* prev_queue;
} bucket_t;

/*
 * Una celda de la tabla hash representa
 * la cabeza de una lista enlazada
 * de datos. Si la celda está vacía
 * contendrá puntero a NULL.
 */
typedef bucket_t* cell_t;

/*
 * Estadísticas sobre los accesos a la base
 * de datos, específicamente las cantidades de:
 * - inserciones
 * - borrados
 * - lecturas
 * - pares clave-valor
 * - bytes utilizados por los valores almacenados
 */
typedef struct record_t {
	uint64_t puts;
	uint64_t dels;
	uint64_t gets;
	uint64_t keys;
	uint64_t bytes;
} record_t;

/*
 * TODO: implementar counter64_t contador con mutex
 */
typedef struct concurrent_record_t {
	counter64_t puts;
	counter64_t dels;
	counter64_t gets;
	counter64_t keys;
	counter64_t bytes;
} concurrent_record_t;

/*
 * Un array de celdas es una tabla hash,
 * que en nuestro caso es el banco de datos en sí.
 * Se va redimensionando de acuerdo a
 * nuestras necesidades de almacenamiento.
 *
 * TODO: justificar LRU
 *
 * Como parte de la base de datos guardamos
 * los metadatos de sus operaciones y memoria usada.
 */
typedef struct database_t {
	cell_t* hash_table;
	bucket_t* least_recently_used;
	// last_recently_used_cell_index ?
	concurrent_record_t record;
} database_t;
