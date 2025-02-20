# Задание 1

Разработайте функции `constant`, `variable`, `add`, `subtract`, `multiply`, `divide` и `negate` для представления арифметических выражений.

Пример описания выражения `2x - 3`:

```clojure
(def expr
  (subtract
    (multiply
      (constant 2)
      (variable "x"))
    (constant 3)))
```

Выражение должно быть функцией, возвращающей значение выражения при подстановке переменных, заданных отображением. Например, `(expr {"x" 2})` должно быть равно `1`.

Разработайте разборщик выражений, читающий выражения в стандартной для Clojure форме. Например:

```clojure
(parseFunction "(- (* 2 x) 3)")
```

должно быть эквивалентно `expr`.

### При выполнении задания следует обратить внимание на:
- Выделение общего кода для операций.

---

# Задание 2

Разработайте конструкторы `Constant`, `Variable`, `Add`, `Subtract`, `Multiply`, `Divide` и `Negate` для представления арифметических выражений.

Пример описания выражения `2x - 3`:

```clojure
(def expr
  (Subtract
    (Multiply
      (Constant 2)
      (Variable "x"))
    (Constant 3)))
```

Функция `(evaluate expression vars)` должна производить вычисление выражения `expression` для значений переменных, заданных отображением `vars`. Например, `(evaluate expr {"x" 2})` должно быть равно `1`.

Функция `(toString expression)` должна выдавать запись выражения в стандартной для Clojure форме.

Функция `(parseObject "expression")` должна разбирать выражения, записанные в стандартной для Clojure форме. Например:

```clojure
(parseObject "(- (* 2 x) 3)")
```

должно быть эквивалентно `expr`.


