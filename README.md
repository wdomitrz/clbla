# clbla

Właściwy opis języka znajduje się w pliku `clbla.pdf`.

Polecenie make tworzy dwa pliki `clbla` i `clblai` (oraz wymagany plik `interpreter`). Ich opisy są w oddzielnych komentarzach.

## `clbla`

Plik `clbla` to interpreter - jak dostanie 1 argument, to wczytuje jego zawartość i interpretuje wypisując na wyjście zmienną `main`. Jeśli nie dostanie żadnego argumentu, to czyta wejście ze standardowego wejścia i to je interpretuje.

## `clblai`

Plik `clblai` jest stworzony w celu łatwiejszego sprawdzania zadania. Jest to tryb interaktywny.
* `:t (expression)` pokaże typ wyrażenia expression,
* `:v (variable)` pokaże wartość zmiennej variable,
* `:h` pokaże pomoc,
* inne wyrażenie będzie interpretowane i, jeśli uda się je zinterpretować, to zostanie dodane do aktualnego środowiska. Można używać tam rozszerzeń, importów, definicji typów i deklaracji typów z definicjami (ponieważ należy podać w jednej linijce deklarację i definicję typu, to trzeba użyć średnika. Na przykład `i :: a -> a ; i = s k k`).

## `interpreter`

Plik `interpreter` (wymagany w treści zadania) to kopia pliku `clbla`.

## Gramatyka

W pliku grammar/clbla.cf jest gramatyka języka. Nie ma w niej żadnych konfliktów. W celu uniknięcia problemu z kompilacją na studentsie już skompilowaną do plików `*.hs` gramatykę załączyłem w katalogu `src/Parser`. Pliki w `src/Parser` lekko zmodyfikowałem, żeby nie mieć warningów przy kompilacji - całość kompiluję z flagą `-Wall`.

## Przykłady

### `examples`

W folderze `examples` są przykłady różnych modułów.

### `bad`

W folderze `bad` są przykłady różnych błędów.

### `good`

Folder good to kopia folderu `examples`.

## Dodatkowe komentarze

### Realizacja założeń

Pomijając rekonstrukcję typów (mam sprawdzanie typów) zrealizowałem wszystkie założenia z deklaracji języka.

### `fold`y

Najtrudniejszym wyzwaniem było zrobienie automatycznie generowanego `fold`a, którego implementacja może nie wyglądać na długą, ale jednak ona sama zajęła mi kilka dni.

### Leniwość

Mój język jest leniwy, co dobrze prezentuje plik `ListNats.clbla`. Zdefiniowana w nim jest lista wszystkich liczb naturalnych `nats :: List Nat`, a wypisywana przez niego jest lista pierwszych sześciu liczb naturalnych.

### Oglądanie przykładów

Przykłady dobrze oglądać z użyciem `clblai`. Dobrym przykładem użycia kilku mechanizmów jest funkcja `fib` licząca liczby Fibonacciego. Na przykład użycie w trybie interaktywnym poleceń:

```clbla
f9 :: Nat ; f9 = fib (three * three)
:v f9
```

To dostaniemy na wyjściu reprezentację liczby 34 (łatwo sprawdzić jaką liczbę dostaliśmy licząc nawiasy zamykające, których jest o 1 mniej niż wartość liczby).
