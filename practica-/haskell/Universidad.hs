-- Módulo principal del programa
module Main where

-- Librerías necesarias
import System.IO
import System.Directory (doesFileExist) -- Para verificar si el archivo existe
import Data.List (find) -- Para buscar elementos en listas
import Text.Read (readMaybe) -- Para convertir texto a número de forma segura

-- Definición del tipo de dato Student
-- Representa un estudiante con ID, nombre, hora de entrada y salida
data Student = Student
    { studentId :: String        -- ID del estudiante
    , studentName :: String      -- Nombre del estudiante
    , entryTime :: Maybe Int     -- Hora de entrada en minutos (Nothing si no ha ingresado)
    , exitTime :: Maybe Int      -- Hora de salida en minutos (Nothing si no ha salido)
    } deriving (Eq)

-- Definición de cómo se muestra un estudiante al imprimirlo
instance Show Student where
    show student =
        "ID: " ++ studentId student ++
        " | Name: " ++ studentName student ++
        " | Entry: " ++ formatMaybeTime (entryTime student) ++
        " | Exit: " ++ formatMaybeTime (exitTime student)

-- Ruta del archivo donde se almacenan los estudiantes
filePath :: FilePath
filePath = "University.txt"

-- Función principal
-- Carga los estudiantes y muestra el menú
main :: IO ()
main = do
    students <- loadStudents
    putStrLn "===== STUDENT REGISTRATION SYSTEM - HASKELL ====="
    mainMenu students

-- Menú principal interactivo
mainMenu :: [Student] -> IO ()
mainMenu students = do
    putStrLn "\nMenu:"
    putStrLn "1. Check In"
    putStrLn "2. Search by Student ID"
    putStrLn "3. Time Calculation"
    putStrLn "4. Students List"
    putStrLn "5. Check Out"
    putStrLn "6. Exit"
    putStr "Choose an option: "
    option <- getLine

    case option of
        "1" -> do
            updatedStudents <- checkIn students
            mainMenu updatedStudents
        "2" -> do
            searchStudent students
            mainMenu students
        "3" -> do
            calculateStudentTime students
            mainMenu students
        "4" -> do
            listStudents students
            mainMenu students
        "5" -> do
            updatedStudents <- checkOut students
            mainMenu updatedStudents
        "6" -> putStrLn "Program finished."
        _   -> do
            putStrLn "Invalid option."
            mainMenu students

-- Carga los estudiantes desde el archivo
loadStudents :: IO [Student]
loadStudents = do
    exists <- doesFileExist filePath
    if not exists
        then do
            -- Si no existe el archivo, lo crea vacío
            writeFile filePath ""
            return []
        else do
            -- Lee el contenido del archivo
            content <- readFile filePath
            let studentLines = lines content
            return (map parseStudent studentLines)

-- Guarda la lista de estudiantes en el archivo
saveStudents :: [Student] -> IO ()
saveStudents students = do
    writeFile filePath (unlines (map studentToLine students))

-- Convierte una línea del archivo en un estudiante
parseStudent :: String -> Student
parseStudent line =
    case splitBy ',' line of
        -- Caso completo: ID, nombre, entrada, salida
        [sid, name, ent, ext] ->
            Student sid name (parseMaybeInt ent) (parseMaybeInt ext)
        -- Caso incompleto: solo ID y nombre
        [sid, name] ->
            Student sid name Nothing Nothing
        -- Caso inválido
        _ ->
            Student "" "Invalid Record" Nothing Nothing

-- Convierte un estudiante en una línea de texto para el archivo
studentToLine :: Student -> String
studentToLine student =
    studentId student ++ "," ++
    studentName student ++ "," ++
    maybeToString (entryTime student) ++ "," ++
    maybeToString (exitTime student)

-- Registro de entrada (Check In)
checkIn :: [Student] -> IO [Student]
checkIn students = do
    putStr "Enter student ID: "
    sid <- getLine

    case findStudentById sid students of
        -- Si no existe el estudiante
        Nothing -> do
            putStrLn "Student not found."
            return students

        Just student ->
            case entryTime student of
                -- Ya hizo check-in
                Just _ -> do
                    putStrLn "This student already checked in."
                    return students
                -- Registrar entrada
                Nothing -> do
                    time <- askTime "Enter entry time (HH:MM): "
                    let updatedStudent = student
                            { entryTime = Just time
                            , exitTime = Nothing
                            }
                    let updatedStudents = replaceStudent updatedStudent students
                    saveStudents updatedStudents
                    putStrLn "Check in registered successfully."
                    return updatedStudents

-- Buscar estudiante por ID
searchStudent :: [Student] -> IO ()
searchStudent students = do
    putStr "Enter student ID: "
    sid <- getLine

    case findStudentById sid students of
        Nothing -> putStrLn "Student not found."
        Just student ->
            case (entryTime student, exitTime student) of
                -- Está dentro
                (Just _, Nothing) -> do
                    putStrLn "Student is currently inside the university:"
                    print student
                -- No está dentro
                _ -> putStrLn "Student is not currently inside the university."

-- Calcular el tiempo que estuvo el estudiante
calculateStudentTime :: [Student] -> IO ()
calculateStudentTime students = do
    putStr "Enter student ID: "
    sid <- getLine

    case findStudentById sid students of
        Nothing -> putStrLn "Student not found."
        Just student ->
            case (entryTime student, exitTime student) of
                -- Tiene entrada y salida válidas
                (Just ent, Just ext) ->
                    if ext >= ent
                        then putStrLn ("Time spent: " ++ formatDuration (ext - ent))
                        else putStrLn "Invalid data: exit time is earlier than entry time."
                -- No ha salido
                (Just _, Nothing) ->
                    putStrLn "The student has not checked out yet."
                -- Información incompleta
                _ ->
                    putStrLn "There is not enough information to calculate time."

-- Lista todos los estudiantes
listStudents :: [Student] -> IO ()
listStudents [] = putStrLn "No students loaded."
listStudents students = do
    putStrLn "\nStudents loaded from file:"
    mapM_ print students

-- Registro de salida (Check Out)
checkOut :: [Student] -> IO [Student]
checkOut students = do
    putStr "Enter student ID: "
    sid <- getLine

    case findStudentById sid students of
        Nothing -> do
            putStrLn "Student not found."
            return students

        Just student ->
            case (entryTime student, exitTime student) of
                -- No ha hecho check-in
                (Nothing, _) -> do
                    putStrLn "The student has not checked in yet."
                    return students

                -- Ya hizo check-out
                (_, Just _) -> do
                    putStrLn "This student already checked out."
                    return students

                -- Registro válido de salida
                (Just ent, Nothing) -> do
                    time <- askTime "Enter exit time (HH:MM): "
                    if time < ent
                        then do
                            putStrLn "Exit time cannot be earlier than entry time."
                            return students
                        else do
                            let updatedStudent = student { exitTime = Just time }
                            let updatedStudents = replaceStudent updatedStudent students
                            saveStudents updatedStudents
                            putStrLn "Check out registered successfully."
                            return updatedStudents

-- Busca un estudiante por ID
findStudentById :: String -> [Student] -> Maybe Student
findStudentById sid = find (\student -> studentId student == sid)

-- Reemplaza un estudiante en la lista
replaceStudent :: Student -> [Student] -> [Student]
replaceStudent updatedStudent =
    map (\student ->
        if studentId student == studentId updatedStudent
            then updatedStudent
            else student
    )

-- Solicita una hora válida al usuario
askTime :: String -> IO Int
askTime message = do
    putStr message
    input <- getLine
    case parseTimeToMinutes input of
        Just minutes -> return minutes
        Nothing -> do
            putStrLn "Invalid format. Use HH:MM."
            askTime message

-- Convierte HH:MM a minutos totales
parseTimeToMinutes :: String -> Maybe Int
parseTimeToMinutes input =
    case splitBy ':' input of
        [hh, mm] -> do
            h <- readMaybe hh :: Maybe Int
            m <- readMaybe mm :: Maybe Int
            if h >= 0 && h < 24 && m >= 0 && m < 60
                then Just (h * 60 + m)
                else Nothing
        _ -> Nothing

-- Formatea un Maybe Int como hora legible
formatMaybeTime :: Maybe Int -> String
formatMaybeTime Nothing = "N/A"
formatMaybeTime (Just minutes) = minutesToHHMM minutes

-- Convierte minutos a formato HH:MM
minutesToHHMM :: Int -> String
minutesToHHMM totalMinutes =
    let hours = totalMinutes `div` 60
        minutes = totalMinutes `mod` 60
    in padZero hours ++ ":" ++ padZero minutes

-- Formatea duración en horas y minutos
formatDuration :: Int -> String
formatDuration totalMinutes =
    let hours = totalMinutes `div` 60
        minutes = totalMinutes `mod` 60
    in show hours ++ " hour(s) and " ++ show minutes ++ " minute(s)"

-- Agrega un 0 si el número es menor a 10
padZero :: Int -> String
padZero n
    | n < 10    = '0' : show n
    | otherwise = show n

-- Convierte un String a Maybe Int
parseMaybeInt :: String -> Maybe Int
parseMaybeInt "" = Nothing
parseMaybeInt str = readMaybe str :: Maybe Int

-- Convierte Maybe Int a String
maybeToString :: Maybe Int -> String
maybeToString Nothing = ""
maybeToString (Just value) = show value

-- Función para dividir un String por un delimitador
splitBy :: Char -> String -> [String]
splitBy _ "" = [""]
splitBy delimiter text =
    let (firstPart, rest) = break (== delimiter) text
    in case rest of
        [] -> [firstPart]
        (_:remainingText) -> firstPart : splitBy delimiter remainingText