import scala.io.Source
case class Time(hours: Int, minutes: Int)
case class StrungTime(hours: String, minutes: String)
case class TimeAndDay(timeLength: Time, tomorrowOrToday: String)
object app {
  def main(args: Array[String]): Unit ={
    val fileName = "src/times.txt"
    for(line <- Source.fromFile(fileName).getLines){
      println(getNextJob("6:30", line.substring(0, 4)) + " " + line.substring(5))
    }
  }
  def getNextJob(currentTime: String, jobTime: String): String = {
    val formattedTimes = formatTimes(currentTime, jobTime)
    val formattedJobTime = new Time(formattedTimes(1).hours, formattedTimes(1).minutes)
    val hourly = nextJob(formattedTimes)
    f"${formattedJobTime.hours}%2.0f:${formattedJobTime.minutes}%2.0f ${hourly.tomorrowOrToday}"
  }
  def nextJob(timeList: List[Time]): TimeAndDay = {
    //timeList(0) = current time
    //timeList(1) = job time
    val currentTime = timeList(0).hours*60 + timeList(0).minutes
    val jobTime = timeList(1).hours*60 + timeList(1).minutes
    if(jobTime >= currentTime){
      val time = formatMinuteTotal(jobTime - currentTime)
      val timeDate = new TimeAndDay(time, "today")
      timeDate
    }
    else{
      val time = formatMinuteTotal(1440 - currentTime + jobTime)
      val timeDate = new TimeAndDay(time, "tomorrow")
      timeDate
    }
  }
  def formatMinuteTotal(minuteTotal: Int): Time = {
    val minutes = minuteTotal % 60
    val hours = (minuteTotal - minutes)/60
    new Time(hours, minutes)
  }
  def formatTimes(currentTime: String, jobTime: String): List[Time] = {
    val currentTimey = new Time(currentTime.split(':')(0).toInt, currentTime.split(':')(1).toInt)
    val jobTimey = new StrungTime(jobTime.split(' ')(1), jobTime.split(' ')(0))
    if(jobTimey.hours.contains('*')){
      if(jobTimey.minutes.contains('*')){
        val resultList: List[Time] = List(currentTimey, currentTimey)
        resultList
      }
      else{
        if(currentTimey.minutes > jobTimey.minutes.toInt){
          val finalJobTime = new Time(currentTimey.hours + 1, jobTimey.minutes.toInt)
          val resultList : List[Time] = List(currentTimey, finalJobTime)
          resultList
        }
        else{
          val finalJobTime = new Time(currentTimey.hours, jobTimey.minutes.toInt)
          val resultList : List[Time] = List(currentTimey, finalJobTime)
          resultList
        }
      }
    }
    else{
      if(jobTimey.minutes.contains('*')){
        if(jobTimey.hours.toInt == currentTimey.hours){
          val finalJobTime = new Time(jobTimey.hours.toInt, currentTimey.minutes)
          val resultList: List[Time] = List(currentTimey, finalJobTime)
          resultList
        }
        else {
          val finalJobTime = new Time(jobTimey.hours.toInt, 0)
          val resultList: List[Time] = List(currentTimey, finalJobTime)
          resultList
        }
      }
      else{
        val finalJobTime = new Time(jobTimey.hours.toInt, jobTimey.minutes.toInt)
        val resultList: List[Time] = List(currentTimey, finalJobTime)
        resultList
      }
    }
  }
}