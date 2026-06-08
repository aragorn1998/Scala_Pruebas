
object ejercicio_11{
    import scala.io.Source
    def readCsv(path: String): List[String] = {
        
        val source = Source.fromFile(path)
        try source.getLines().toList
        finally source.close()
    }
    val lines = readCsv("C://Users//a.i.garcia//Downloads//student_dataset_10000_rows.csv")
    val dataLines: List[String] = lines.drop(1)
    
    case class StudentRaw(
    studyHours: Int,
    attendance: Int,
    sleepHours: Int,
    internetUsage: Int,
    assignmentsCompleted: Int,
    previousScore: Int,
    examScore: Double,
    placementStatus: String
)

    def parseStudent(line: String): StudentRaw = {
        val Array(
            studyHours,
            attendance,
            sleepHours,
            internetUsage,
            assignmentsCompleted,
            previousScore,
            examScore,
            placementStatus
        ) = line.split(",")

        StudentRaw(
            studyHours.toInt,
            attendance.toInt,
            sleepHours.toInt,
            internetUsage.toInt,
            assignmentsCompleted.toInt,
            previousScore.toInt,
            examScore.toDouble,
            placementStatus
        )
        }
    
    val studentsRaw: List[StudentRaw] =
    dataLines.map(parseStudent)

    def engagementScore(s: StudentRaw): Double =
    s.studyHours * 0.4 +
    s.attendance * 0.3 +
    s.assignmentsCompleted * 0.3

    def passedExam(s: StudentRaw): Boolean =
        s.examScore >= 80

    def riskLevel(s: StudentRaw): String =
        if (passedExam(s) && s.attendance >= 75) "LOW"
        else if (passedExam(s)) "MEDIUM"
        else "HIGH"
    case class StudentEnriched(
        studyHours: Int,
        attendance: Int,
        sleepHours: Int,
        internetUsage: Int,
        assignmentsCompleted: Int,
        previousScore: Int,
        examScore: Double,
        placementStatus: String,
        engagementScore: Double,
        passedExam: Boolean,
        riskLevel: String
    )
    def enrich(s: StudentRaw): StudentEnriched =
        StudentEnriched(
            studyHours = s.studyHours,
            attendance = s.attendance,
            sleepHours = s.sleepHours,
            internetUsage = s.internetUsage,
            assignmentsCompleted = s.assignmentsCompleted,
            previousScore = s.previousScore,
            examScore = s.examScore,
            placementStatus = s.placementStatus,
            engagementScore = engagementScore(s),
            passedExam = passedExam(s),
            riskLevel = riskLevel(s)
        )
    
    val studentsEnriched: List[StudentEnriched] =
    studentsRaw.map(enrich)

    val finalStudents = 
    studentsEnriched.filter { s =>
        s.placementStatus == "Placed" &&
        (s.riskLevel == "LOW" || s.riskLevel == "MEDIUM")
    }

    
    def toCsv(s: StudentEnriched): String =
    List(
        s.studyHours,
        s.attendance,
        s.sleepHours,
        s.internetUsage,
        s.assignmentsCompleted,
        s.previousScore,
        s.examScore,
        s.placementStatus,
        f"${s.engagementScore}%.2f",
        s.passedExam,
        s.riskLevel
    ).mkString(",")
    val header =
        "study_hours,attendance,sleep_hours,internet_usage,assignments_completed," +
        "previous_score,exam_score,placement_status,engagement_score,passed_exam,risk_level"
    
    import java.nio.file.{Files, Paths}
    import java.nio.charset.StandardCharsets

    val output =
    (header :: finalStudents.map(toCsv)).mkString("\n")

    Files.write(
        Paths.get("C://Users//a.i.garcia//Downloads//output_students.csv"),
        output.getBytes(StandardCharsets.UTF_8)
    )
    def main(args:Array[String]){
        println(s"Total final students: ${finalStudents.size}")
    }
}

object ejercicio_12{

    def esPrimo(n: Int): Boolean = {
    if (n <= 1) false
    else !(2 until n).exists(i => n % i == 0)
    }
    def primosHasta(n:Int): List[Int] = {
    (2 to n)
        .filter(i => (2 until i).forall(i % _ != 0))
        .toList

    }
    def main(args: Array[String]): Unit = {
        val numeros = 1 to 100
        val primos = numeros.filter(esPrimo)
        val primosHasta_2 = primosHasta(100)
        println(s"Números primos entre 1 y 100: $primosHasta_2")
    }
}