#pragma once

#include <stddef.h>

/*
 * Representa un bloque contiguo de memoria,
 * sobre el cuál sabemos su tamaño en bytes.
 * 
 * Su uso está pensado para pasar blobs por valor,
 * ya que deberían tratarse como inmutables.
 * 
 * Cuando alguna estructura se adueñe de estos blobs
 * el programador debe asegurarse de que nadie más
 * tenga copias de los mismos, así evitamos double frees.
 */
typedef struct {
	void* memory;
	size_t bytes;
} blob_t;

/*
 * Crea un blob con memoria NULL y 0 bytes
 */
blob_t blob_empty();

/*
 * Compara byte a byte ambos blobs para ver si son iguales.
 * 
 * Si son de diferentes tamaños la comparación por igualdad es falsa y es O(1).
 * 
 * Es segura para blobs vacíos.
 */
int blob_equals(blob_t a, blob_t b);

/*
 * Produce el hash de un blob, usando el algoritmo de Horner evaluando
 * el polinomio con 31.
 * 
 * Es segura para blobs vacíos, y hashean a 0.
 */
size_t blob_hash(blob_t blob);

/*
 * Libera la memoria de un blob.
 *
 * Es segura para blobs vacíos, liberando NULL.
 */
void blob_free(blob_t blob);
