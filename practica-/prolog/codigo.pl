% Declaración dinámica del predicado student/4
% Permite agregar y eliminar estudiantes en tiempo de ejecución
:- dynamic student/4.

% Librería para leer líneas completas desde archivos
:- use_module(library(readutil)).

% Ruta del archivo donde se almacenan los estudiantes
file_path('University.txt').

% Punto de inicio del programa
% Carga los estudiantes desde archivo y muestra el menú
start :-
    load_students,
    menu.

% Menú principal del sistema
menu :-
    nl,
    write('===== STUDENT REGISTRATION SYSTEM - PROLOG ====='), nl,
    write('1. Check In'), nl,
    write('2. Search by Student ID'), nl,
    write('3. Time Calculation'), nl,
    write('4. Students List'), nl,
    write('5. Check Out'), nl,
    write('6. Exit'), nl,
    write('Choose an option: '),
    read(Option),
    handle_option(Option).

% Manejo de opciones del menú
handle_option(1) :- check_in, menu.
handle_option(2) :- search_student, menu.
handle_option(3) :- calculate_time, menu.
handle_option(4) :- list_students, menu.
handle_option(5) :- check_out, menu.
handle_option(6) :- write('Program finished.'), nl.
handle_option(_) :- write('Invalid option.'), nl, menu.

% Carga los estudiantes desde el archivo
load_students :-
    % Elimina cualquier dato previo en memoria
    retractall(student(_, _, _, _)),
    file_path(File),
    exists_file(File), % Verifica si el archivo existe
    !,
    open(File, read, Stream), % Abre el archivo en modo lectura
    read_students(Stream),    % Lee los estudiantes
    close(Stream).            % Cierra el archivo

% Si el archivo no existe, lo crea vacío
load_students :-
    file_path(File),
    open(File, write, Stream),
    close(Stream).

% Lee cada línea del archivo recursivamente
read_students(Stream) :-
    read_line_to_string(Stream, Line),
    (
        Line == end_of_file -> % Fin del archivo
            true
        ;
            (
                Line \= "" ->   % Si la línea no está vacía
                    parse_student(Line)
                ;
                    true
            ),
            read_students(Stream) % Llamada recursiva
    ).

% Procesa una línea del archivo y la convierte en un estudiante
parse_student(Line) :-
    split_string(Line, ",", "", Parts),
    (
        % Caso con 4 datos: ID, Nombre, Entrada, Salida
        Parts = [IDStr, NameStr, EntryStr, ExitStr] ->
            atom_string(ID, IDStr),
            atom_string(Name, NameStr),
            parse_field(EntryStr, Entry),
            parse_field(ExitStr, Exit),
            assertz(student(ID, Name, Entry, Exit))
        ;
        % Caso con solo ID y Nombre
        Parts = [IDStr, NameStr] ->
            atom_string(ID, IDStr),
            atom_string(Name, NameStr),
            assertz(student(ID, Name, empty, empty))
    ).

% Convierte un campo vacío en 'empty'
parse_field("", empty).

% Convierte un string a número
parse_field(Str, Value) :-
    Str \= "",
    number_string(Value, Str).

% Guarda todos los estudiantes en el archivo
save_students :-
    file_path(File),
    open(File, write, Stream),
    forall(
        student(ID, Name, Entry, Exit),
        (
            write_student_line(Stream, ID, Name, Entry, Exit)
        )
    ),
    close(Stream).

% Escribe un estudiante en una línea del archivo
write_student_line(Stream, ID, Name, Entry, Exit) :-
    atom_string(ID, IDStr),
    atom_string(Name, NameStr),
    field_to_string(Entry, EntryStr),
    field_to_string(Exit, ExitStr),
    write(Stream, IDStr),
    write(Stream, ','),
    write(Stream, NameStr),
    write(Stream, ','),
    write(Stream, EntryStr),
    write(Stream, ','),
    write(Stream, ExitStr),
    nl(Stream).

% Convierte 'empty' a cadena vacía
field_to_string(empty, "").

% Convierte número a string
field_to_string(Value, Str) :-
    number_string(Value, Str).

% Registro de entrada del estudiante
check_in :-
    write('Enter ID in quotes: '),
    read(ID),
    (
        % Si el estudiante existe y no ha ingresado
        student(ID, Name, empty, _) ->
            ask_time(Time),
            retract(student(ID, Name, empty, _)),
            assertz(student(ID, Name, Time, empty)),
            save_students,
            write('Check in successful.'), nl
        ;
        % Si ya ingresó
        student(ID, _, Entry, _),
        Entry \= empty ->
            write('Student already checked in.'), nl
        ;
        % Si no existe
        write('Student not found.'), nl
    ).

% Buscar estudiante por ID
search_student :-
    write('Enter ID in quotes: '),
    read(ID),
    (
        % Está dentro de la universidad
        student(ID, Name, Entry, empty),
        Entry \= empty ->
            write('Student is inside the university.'), nl,
            print_student(ID, Name, Entry, empty)
        ;
        % Existe pero no está dentro
        student(ID, _, _, _) ->
            write('Student is not currently inside.'), nl
        ;
        % No existe
        write('Student not found.'), nl
    ).

% Calcula el tiempo que estuvo el estudiante
calculate_time :-
    write('Enter ID in quotes: '),
    read(ID),
    (
        % Tiene entrada y salida
        student(ID, _, Entry, Exit),
        Entry \= empty,
        Exit \= empty ->
            Duration is Exit - Entry,
            write('Time spent: '),
            write(Duration),
            write(' minutes'),
            nl
        ;
        % No ha salido
        student(ID, _, Entry, empty),
        Entry \= empty ->
            write('The student has not checked out yet.'), nl
        ;
        % Información insuficiente
        student(ID, _, _, _) ->
            write('There is not enough information to calculate time.'), nl
        ;
        % No existe
        write('Student not found.'), nl
    ).

% Lista todos los estudiantes
list_students :-
    (
        \+ student(_, _, _, _) ->
            write('No students loaded.'), nl
        ;
        forall(
            student(ID, Name, Entry, Exit),
            print_student(ID, Name, Entry, Exit)
        )
    ).

% Registro de salida del estudiante
check_out :-
    write('Enter ID in quotes: '),
    read(ID),
    (
        % Caso válido de salida
        student(ID, Name, Entry, empty),
        Entry \= empty ->
            ask_time(Time),
            Time >= Entry, % Verifica que la salida sea después de la entrada
            retract(student(ID, Name, Entry, empty)),
            assertz(student(ID, Name, Entry, Time)),
            save_students,
            write('Check out successful.'), nl
        ;
        % No ha hecho check-in
        student(ID, _, empty, _) ->
            write('Student has not checked in yet.'), nl
        ;
        % Ya salió
        student(ID, _, _, Exit),
        Exit \= empty ->
            write('Student already checked out.'), nl
        ;
        % Error general
        write('Student not found or invalid exit time.'), nl
    ).

% Imprime la información de un estudiante
print_student(ID, Name, Entry, Exit) :-
    write('ID: '), write(ID),
    write(' | Name: '), write(Name),
    write(' | Entry: '), write(Entry),
    write(' | Exit: '), write(Exit),
    nl.

% Solicita la hora al usuario
ask_time(Time) :-
    write('Enter time HH:MM in quotes: '),
    read(Input),
    parse_time_input(Input, Time).

% Convierte una hora en formato HH:MM a minutos totales
parse_time_input(Atom, Minutes) :-
    atom_string(Atom, Str),
    split_string(Str, ":", "", [HStr, MStr]),
    number_string(H, HStr),
    number_string(M, MStr),
    H >= 0, H < 24,
    M >= 0, M < 60,
    Minutes is H * 60 + M.