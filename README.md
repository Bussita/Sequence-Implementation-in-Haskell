
````markdown
# Sequence-Implementation-in-Haskell

Implementaciones de una estructura de datos `Sequence` en Haskell utilizando listas enlazadas y arrays persistentes con *structural sharing*.

## Descripción del proyecto

Este repositorio contiene distintas formas de implementar un tipo abstracto de datos `Sequence`, comparando soluciones basadas en listas y en arrays persistentes.  
El objetivo es mostrar cómo diseñar estructuras persistentes y analizar sus ventajas y limitaciones desde una perspectiva educativa.

## Características principales

- Implementación basada en listas enlazadas.
- Implementación basada en arrays persistentes con *structural sharing*.
- Código en Haskell puro.
- Ejemplos simples de uso.
- Contraste conceptual entre las distintas implementaciones.

## Cómo compilar y ejecutar

```bash
git clone https://github.com/Bussita/Sequence-Implementation-in-Haskell.git
cd Sequence-Implementation-in-Haskell

# Compilación con GHC
ghc -O2 src/Sequence.hs

# O carga interactiva
ghci src/Sequence.hs
````

Una vez dentro de GHCi:

```haskell
import Sequence

let s = empty
let s2 = cons 1 s
let s3 = append s2 (fromList [2,3,4])
print (toList s3)   -- [1,2,3,4]
```

## Ejemplos de uso

```haskell
let seq1 = fromList [10,20,30]
let seq2 = cons 5 seq1
let seq3 = append seq2 (fromList [40,50])
print (toList seq3)   -- [5,10,20,30,40,50]
```

## Público objetivo

Este proyecto está pensado para:

* Estudiantes que quieran explorar implementaciones de estructuras de datos en Haskell.
* Personas interesadas en estructuras persistentes y diseño de ADTs.
* Usuarios que deseen comparar aproximaciones simples y avanzadas de la misma abstracción.

## Limitaciones

* No está pensado como una librería de producción.
* El foco es pedagógico, no de rendimiento óptimo.
* Las implementaciones priorizan claridad antes que optimización avanzada.

## Estructura del repositorio

```
Sequence-Implementation-in-Haskell/
├── src/
│   └── Sequence.hs
├── README.md
├── TP2.pdf
└── itemB.pdf
```

## Contribuciones

Se aceptan mejoras, documentación adicional, casos de prueba, optimizaciones o nuevas operaciones para la estructura `Sequence`.
Pueden enviarse issues o pull requests.

## Licencia

Agregar o especificar una licencia en caso de ser necesario mediante un archivo `LICENSE`.

```

Si querés, puedo generar también una versión más formal, una más minimalista, o una más académica tipo informe. ¿Te gustaría alguna variante?
```
