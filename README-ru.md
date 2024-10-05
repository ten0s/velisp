[English version](README.md)

# VeLisp

**VeLisp** - интерпретатор AutoLISP с поддержкой DCL.
Основная идея проекта - AutoLISP как язык программирования общего назначения.
Цель - иметь возможность запускать AutoLISP программы за пределами AutoCAD.
Зачем? Чтобы изучать основы AutoLISP и DCL программирования, писать скрипты,
запускать DCL диалоги как обычные приложения в Windows, MacOS и Linux.

## Содержание

- [Windows](#windows)
  - [Установка](#установка)
  - [Использование](#использование)
- [MacOS](#macos)
  - [Установка](#установка-1)
  - [Использование](#использование-1)
- [Linux](#linux)
  - [Установка](#установка-2)
  - [Использование](#использование-2)
- [Реализованные функции](#реализованные-функции)
- [Известные проблемы и ограничения](#известные-проблемы-и-ограничения)
- [Версия](#версия)
- [Предлагаемые изменения](#предлагаемые-изменения)
- [Лицензия](#лицензия)
- [Дополнительно](#дополнительно)

## Windows

### Установка

Последнюю версию можно найти [здесь](https://github.com/ten0s/velisp/releases).

#### Автоматическая установка

1. Скачайте https://github.com/ten0s/velisp/releases/download/0.7.10/velisp-0.7.10-win-x64-setup.zip
2. Откройте папку Загрузки
3. Распакуйте velisp-0.7.10-win-x64-setup.zip
4. Запустите velisp-0.7.10-win-x64-setup.exe
5. Следуйте указаниям установщика. Настройки по умолчанию подходят большинству пользователей

#### Установка из архива

##### Скачайте и распакуйте

1. Скачайте https://github.com/ten0s/velisp/releases/download/0.7.10/velisp-0.7.10-win-x64.zip
2. Откройте папку Загрузки
3. Выделите velisp-0.7.10-win-x64.zip
4. Нажмите правую кнопку мышки
5. Выберите 'Распаковать все содержимое...'
6. Введите 'C:\'
7. Выберите 'Распаковать'

##### Добавьте папку в переменную окружения PATH

1. Нажмите 'Windows + Break' чтобы открыть Информацию о системе
2. Выберите 'Advanced system settings'
3. Выберите 'Environment Variables...'
4. Выберите 'Path' inside the User variables block
5. Выберите 'Edit...'
6. Выберите 'New...'
7. Введите 'C:\velisp-0.7.10-win-x64'
8. Выберите 'OK'
9. Выберите 'OK'

### Использование

Если вы установили VeLisp с помощью [автоматической установки](#автоматическая-установка)
запускайте 'Start' -> 'VeLisp' -> 'VeLisp Command Prompt'.

Если вы установили Velisp из [архива](#установка-из-архива)
запускайте командную оболочку (cmd.exe) и перейдите в папку распаковки:

```
> cd C:\velisp-0.7.10-win-x64
```

#### Запускаем REPL (Read–Eval–Print Loop)

```
> velisp
VeLisp 0.7.10 on Windows
Type ".license" or ".help" for more information
_$ (+ 1 2)
3
_$ (defun add (a b) (+ a b))
ADD
_$ (mapcar '(lambda (x y) (itoa (add x y))) '(1 2 3) '(9 8 7))
("10" "10" "10")
_$ (quit)
```

#### Запускаем код из файла

Вычисляем 10-е (по умолчанию) число Фибоначчи

```
> velisp examples\fib.lsp
55
```

Вычисляем 11-е число Фибоначчи

```
> velisp examples\fib.lsp 11
89
```

Запускаем пример Калькулятор

```
> velisp examples\calc.lsp
```

![App Calc Windows Image](/images/app-calc-windows.png)

Запускаем пример Сапер

```
> velisp examples\mines.lsp
```

![App Mines Windows Image](/images/app-mines-windows.png)

Запускаем пример Пятнашки

```
> velisp examples\fifteen.lsp
```

![App Fifteen Windows Image](/images/app-fifteen-windows.png)

Запускаем пример Демо

```
> velisp examples\demo.lsp
```

![App Demo Windows Image](/images/app-demo-windows.png)

Запускаем пример Слайды

```
> velisp examples\slides.lsp
```

![App Slides Windows Image](/images/app-slides-windows.png)

### Запускаем код из стандартного ввода

```
> type examples\fib.lsp | velisp
55
```

```
> type examples\fib.lsp | velisp -- 11
89
```

```
> echo (alert "Hello from VeLisp!") | velisp
```

![Alert Hello From VeLisp Windows Image](/images/alert-hello-velisp-windows.png)

```
> echo (alert (strcat "Hello from " (argv 2) "!")) | velisp -- Arg
```

![Alert Hello From Arg Windows Image](/images/alert-hello-arg-windows.png)

#### Запускаем без поддержки DCL (более быстрая загрузка, хорошо для скриптов)

```
> velisp --no-dcl examples\fib.lsp
55
```

### Запускаем примеры в AutoCAD

#### Добавьте папку примеров в пути AutoCAD

1. Выберите 'Tools' -> 'Options...'
2. Выберите 'Files' -> 'Support File Search Path'
3. Выберите 'Add...'
4. Выберите 'Browse...'
5. Выберите C:\Program Files\VeLisp\examples или C:\velisp-0.7.10-win-x64\examples
6. Выберите 'OK'
7. Выберите 'Apply'
7. Выберите 'OK'

#### Запускает примеры

1. Выберите 'Tools' -> 'AutoLISP' -> 'Visual LISP Editor'
2. Запускаем в Visual LISP Console

```
_$ (load "calc")
_$ (load "mines")
_$ (load "fifteen")
_$ (load "demo")
_$ (load "slides")
```

### Самораспаковывающийся exe-архив

Пример создания самораспаковывающегося exe-архива
https://ten0s.github.io/blog/2024/10/05/velisp-self-extracting-archive-ru

## MacOS

### Установка

Последнюю версию можно найти [здесь](https://github.com/ten0s/velisp/releases).

#### Скачайте и распакуйте

**Внимание**: Не используйте Safari чтобы скачать архив, не используйте Finder
чтобы распаковать архив и, самое главное, не используйте Finder чтобы запустить
VeLisp в первый раз. Так как VeLisp не подписан, вы не сможете запустить
его из-за [Apple's Gatekeeper](https://support.apple.com/en-us/HT202491).
Вместо этого, запустите Terminal (Finder -> Go -> Utilities -> Terminal) и выполните:

```
% cd $HOME
% curl -LJO https://github.com/ten0s/velisp/releases/download/0.7.10/velisp-0.7.10-macos-x64.tar.xz
% tar xfJ velisp-0.7.10-macos-x64.tar.xz
```

#### Добавьте в ~/.zshrc

```
export PATH=<PATH_TO>/velisp-0.7.10-macos-x64:$PATH
```

### Использование

```
% cd velisp-0.7.10-macos-x64
```

#### Запускаем REPL (Read–Eval–Print Loop)

```
% ./velisp
VeLisp 0.7.10 on MacOS
Type ".license" or ".help" for more information
_$ (+ 1 2)
3
_$ (defun add (a b) (+ a b))
ADD
_$ (mapcar '(lambda (x y) (itoa (add x y))) '(1 2 3) '(9 8 7))
("10" "10" "10")
_$ (quit)
```

#### Запускаем код из файла

Вычисляем 10-е (по умолчанию) число Фибоначчи

```
% ./velisp examples/fib.lsp
55
```

Вычисляем 11-е число Фибоначчи

```
% ./velisp examples/fib.lsp 11
89
```

Запускаем пример Калькулятор

```
% ./velisp examples/calc.lsp
```

![App Calc MacOS Image](/images/app-calc-macos.png)

Запускаем пример Сапер

```
% ./velisp examples/mines.lsp
```

![App Mines MacOS Image](/images/app-mines-macos.png)

Запускаем пример Пятнашки

```
% ./velisp examples/fifteen.lsp
```

![App Fifteen MacOS Image](/images/app-fifteen-macos.png)

Запускаем пример Демо

```
% ./velisp examples/demo.lsp
```

![App Demo MacOS Image](/images/app-demo-macos.png)

Запускаем пример Слайды

```
> ./velisp examples/slides.lsp
```

![App Slides MacOS Image](/images/app-slides-macos.png)

#### Запускаем код из стандартного ввода

```
% cat examples/fib.lsp | ./velisp
55
```

```
% cat examples/fib.lsp | ./velisp -- 11
89
```

```
% echo '(alert "Hello from VeLisp!")' | ./velisp
```

![Alert Hello From VeLisp MacOS Image](/images/alert-hello-velisp-macos.png)

```
% echo '(alert (strcat "Hello from " (argv 2) "!"))' | ./velisp -- Arg
```

![Alert Hello From Arg MacOS Image](/images/alert-hello-arg-macos.png)

#### Запускаем без поддержки DCL (более быстрая загрузка, хорошо для скриптов)

```
% ./velisp --no-dcl examples/fib.lsp
55
```

## Linux

### Установка

Последнюю версию можно найти [здесь](https://github.com/ten0s/velisp/releases).

#### Скачайте и распакуйте

```
$ curl -LJO https://github.com/ten0s/velisp/releases/download/0.7.10/velisp-0.7.10-linux-x64.tar.xz
$ tar xfJ velisp-0.7.10-linux-x64.tar.xz
```

#### Добавьте в ~/.bashrc

```
export PATH=<PATH_TO>/velisp-0.7.10-linux-x64:$PATH
```

### Использование

```
$ cd velisp-0.7.10-linux-x64
```

#### Запускаем REPL (Read–Eval–Print Loop)

```
$ ./velisp
VeLisp 0.7.10 on Linux
Type ".license" or ".help" for more information
_$ (+ 1 2)
3
_$ (defun add (a b) (+ a b))
ADD
_$ (mapcar '(lambda (x y) (itoa (add x y))) '(1 2 3) '(9 8 7))
("10" "10" "10")
_$ (quit)
```

#### Запускаем код из файла

Вычисляем 10-е (по умолчанию) число Фибоначчи

```
$ ./velisp examples/fib.lsp
55
```

Вычисляем 11-е число Фибоначчи

```
$ ./velisp examples/fib.lsp 11
89
```

Запускаем пример Калькулятор

```
$ ./velisp examples/calc.lsp
```

![App Calc Linux Image](/images/app-calc-linux.png)

Запускаем пример Сапер

```
$ ./velisp examples/mines.lsp
```

![App Mines Linux Image](/images/app-mines-linux.png)

Запускаем пример Пятнашки

```
$ ./velisp examples/fifteen.lsp
```

![App Fifteen Linux Image](/images/app-fifteen-linux.png)

Запускаем пример Демо

```
$ ./velisp examples/demo.lsp
```

![App Demo Linux Image](/images/app-demo-linux.png)

Запускаем пример Слайды

```
> ./velisp examples/slides.lsp
```

![App Slides Linux Image](/images/app-slides-linux.png)

#### Запускаем код из стандартного ввода

```
$ cat examples/fib.lsp | ./velisp
55
```

```
$ cat examples/fib.lsp | ./velisp -- 11
89
```

```
$ echo '(alert "Hello from VeLisp!")' | ./velisp
```

![Alert Hello From VeLisp Linux Image](/images/alert-hello-velisp-linux.png)

```
$ echo '(alert (strcat "Hello from " (argv 2) "!"))' | ./velisp -- Arg
```

![Alert Hello From Arg Linux Image](/images/alert-hello-arg-linux.png)

#### Запускаем без поддержки DCL (более быстрая загрузка, хорошо для скриптов)

```
$ ./velisp --no-dcl examples/fib.lsp
55
```

## Реализованные функции

* [AutoLISP Функции](/AutoLISP-Functions.md)
* [DCL Функции](DCL-Functions.md)

## Известные проблемы и ограничения

* Целые числа со знаком в диапазоне от -9,007,199,254,740,991 (-2<sup>53</sup>+1) до +9,007,199,254,740,991 (+2<sup>53</sup>-1)

## Версия

Версия отражает субъективный процент готовности.
Например, версия 0.7.3 означает, что **VeLisp** готов на 73% (0.73).

## Предлагаемые изменения

Все предлагаемые изменения будут иметь лицензии представленные ниже, без каких-либо
дополнительных условий.

## Лицензия

Проект выпущен под лицензией GNU General Public License v3.0 или более поздней
версии с исключением, представленным ниже.
Полную информацию о лицензии можно найти в [LICENSE](LICENSE) или
https://spdx.org/licenses/GPL-3.0-or-later.html.

Файлы в папках [examples/](examples/) и [lib/](lib/)
выпущены под лицензией BSD Zero Clause License.
Полную информацию о лицензии можно найти в [LICENSE](examples/LICENSE),
[LICENSE](lib/LICENSE) или https://spdx.org/licenses/0BSD.html.

## Дополнительно

* [Разработка](/DEVEL.md)
* [Как сделать?](/HOWTO.md)
