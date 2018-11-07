/******************************************************************************
  * BTC-akka
  *
  * Pepe Gallardo, 2018
  *
  ****************************************************************************/

package util

import java.text.SimpleDateFormat
import java.util.Date

object UnixTime {
  def now : Long =
    System.currentTimeMillis() / 1000L

  def toString(time : Long) : String =  {

    val sdf = new SimpleDateFormat("MMM dd,yyyy HH:mm")
    val date = new Date(time * 1000L)
    sdf.format(date)
  }
}
