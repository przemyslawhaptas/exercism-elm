module GradeSchool exposing (addStudent, allStudents, empty, studentsInGrade)

import Dict exposing (Dict)


type alias Grade =
    Int


type alias Student =
    String


type alias School =
    Dict Grade (List Student)


empty : School
empty =
    Dict.empty


addStudent : Grade -> Student -> School -> School
addStudent grade student school =
    Dict.insert grade (addAndSort student (studentsInGrade grade school)) school


addAndSort : Student -> List Student -> List Student
addAndSort student students =
    List.sort (students ++ [ student ])


studentsInGrade : Grade -> School -> List Student
studentsInGrade grade school =
    Dict.get grade school |> Maybe.withDefault []


allStudents : School -> List ( Grade, List Student )
allStudents =
    Dict.toList
